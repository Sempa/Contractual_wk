############################################################
# ? 0. LOAD LIBRARIES
############################################################
library(tidyverse)
library(tidymodels)
library(mice)
library(glmnet)
library(keras3)
library(yardstick)
library(vip)
library(janitor)
library(parallel)
library(reticulate)
# py_require(c("tensorflow", "keras"))


############################################################
# ? 1. DATA PREPARATION
############################################################

# Clean variable names
df <- read_csv("final_dataset_02052026.csv") %>%
  clean_names() %>%
  mutate(
    across(
      where(~ is.character(.) || is.logical(.)),
      ~ case_when(
        is.na(.)                                   ~ NA_real_,
        str_to_lower(as.character(.)) %in% c("Checked", "True", "Yes", "1")  ~ 1,
        str_to_lower(as.character(.)) %in% c("Unchecked", "False", "No", "0") ~ 0,
        TRUE ~ NA_real_
      )
    )
  ) %>%
  mutate(across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE))))

# Convert outcome to factor (0 = No AI, 1 = AI)
df <- df %>%
  mutate(outcome = factor(outcome, levels = c(0, 1)))

# Remove problematic columns (dates, free text, near-empty)
# df <- df %>%
#   dplyr::select(-matches("date|comment|specify"), -total_cd4_count_91, -total_ai,
#                 -uin_3,-uin_179,
#                 -hospital_folder_number, -x1, - event_name, -cd4_100, -ward, 
#                 -did_the_patient_consent_to_dna_substudy, -baseline_samples_taken_time,
#                 -informed_consent_before_enrolment, -hiv_when_diagnosed)

yes_no_vars <- names(df)[
  sapply(df, function(x)
    is.character(x) &&
      all(na.omit(x) %in% c("Yes", "No", "Checked", "Unchecked"))
  )
]

df_imp <- df %>%
  mutate(across(
    all_of(yes_no_vars),
    ~ factor(.x,
             levels = c("No", "Yes", "Unchecked", "Checked"))
  )) %>%
  mutate(across(where(is.character), as.factor),
         # hiv_when_diagnosed = as.numeric(hiv_when_diagnosed)
  ) %>%
  mutate(
    oi_num = rowSums(
      across(
        starts_with("primary_medical_conditions_"),
        ~ !is.na(.) & str_trim(.) != ""
      )
    ),
    ois = oi_num > 0
  ) %>%
  dplyr::select(-starts_with("primary_medical_conditions_")) %>%
  mutate(
    coi_num = rowSums(
      across(
        starts_with("co_existing_medical_conditions_"),
        ~ !is.na(.) & str_trim(.) != ""
      )
    ),
    cois = oi_num > 0
  ) %>%
  dplyr::select(-starts_with("co_existing_medical_conditions_")) %>%
  mutate(
    conc_med_num = rowSums(
      across(
        starts_with("concomitant_therapy_"),
        ~ !is.na(.) & str_trim(.) != ""
      )
    ),
    conc_med = oi_num > 0
  ) %>%
  dplyr::select(-starts_with("concomitant_therapy_"))

source('cols_to_drop_vars.R')
df_imp1 <- df_imp %>%
  dplyr::select(-all_of(cols_to_drop))

constant_vars <- sapply(df_imp1, function(x) {
  length(unique(x[!is.na(x)])) <= 1
})

df_imp1 <- df_imp1[, !constant_vars, drop = FALSE]

nzv <- caret::nearZeroVar(df_imp1, saveMetrics = TRUE)

df_imp1 <- df_imp1[, !nzv$nzv, drop = FALSE] 

############################################################
# ? 2. MICE IMPUTATION (TRAINING DATA ONLY)
############################################################

## -----------------------------
## 2.1. Get number of cores from HPC scheduler
## -----------------------------
## Works on SLURM; safe fallback otherwise
n_cores <- as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))
if (is.na(n_cores) || n_cores < 1) {
  n_cores <- 1
}

## -----------------------------
## 2.2. Predictor matrix
## -----------------------------
predM <- make.predictorMatrix(
  df_imp1 %>% dplyr::select(-outcome)
)

quick_pred <- quickpred(
  df_imp1 %>% dplyr::select(-outcome),
  mincor = 0.1,
  minpuc = 0.25
)

## -----------------------------
## 2.3. Reproducibility (CRITICAL on HPC)
## -----------------------------
RNGkind(kind = "Mersenne-Twister", sample.kind = "Rounding")
set.seed(33)

## -----------------------------
## 2.4. Multiple imputation (parallelised across imputations)
## -----------------------------
nr_imputations <- 5

imputedData <- mice(
  data = df_imp1 %>%
    dplyr::select(-outcome) %>%
    mutate(across(where(is.character), as.factor)) %>%
    as.data.frame(),
  
  m = nr_imputations,
  maxit = 10,
  predictorMatrix = quick_pred,
  ridge = 1e-05,
  defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
  printFlag = FALSE,
  
  ## HPC SAFE PARALLELISATION
  parallel = "snow",
  ncpus = n_cores
)

## -----------------------------
## 2.5. Extract one completed dataset
## -----------------------------
mice_complete <- complete(imputedData, 1)


y_outcome <- df_imp1$outcome

mice_complete <- complete(imputedData, 1)

analysis_data <- cbind(
  outcome = y_outcome,
  mice_complete
) %>%
  dplyr::select(-adrenal_insufficiency, -random_cortisol_result, 
                -synacthen_0_minute_cortisol_result, -synacthen_30_minute_cortisol_result)

# NOTE:
# For publication-grade work, you can repeat the entire pipeline
# across all 5 imputations and pool results.

############################################################
# ? 3. TRAIN-TEST SPLIT (STRATIFIED)
############################################################

RNGkind(kind = "Mersenne-Twister", sample.kind = "Rounding")
set.seed(123)

split <- initial_split(analysis_data, prop = 0.8, strata = outcome)

train_data <- training(split)
test_data  <- testing(split)

############################################################
# ? 4. SIMPLE IMPUTATION FOR TEST DATA (NO LEAKAGE)
############################################################

# Use median/mode for test data to avoid leakage
# test_data <- test_data %>%
#   mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
#   mutate(across(where(is.character), ~ replace_na(., "Missing")))

############################################################
# ? 5. PREPROCESSING (ENCODING + SCALING)
############################################################

rec <- recipe(outcome ~ ., data = train_data) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Prep recipe
prep_rec <- prep(rec)

# Apply transformations
train_processed <- bake(prep_rec, new_data = train_data)
test_processed  <- bake(prep_rec, new_data = test_data)

############################################################
# ? 6. LASSO FEATURE SELECTION
############################################################

lasso_model <- logistic_reg(penalty = tune(), mixture = 1) %>%
  set_engine("glmnet")

lasso_workflow <- workflow() %>%
  add_model(lasso_model) %>%
  add_formula(outcome ~ .)

# Cross-validation
lasso_fit <- tune_grid(
  lasso_workflow,
  resamples = vfold_cv(train_processed, v = 5),
  grid = 20,
  metrics = metric_set(roc_auc)
)

# Select best penalty
best_lambda <- select_best(lasso_fit, metric = "roc_auc")

# Fit final LASSO
final_lasso <- finalize_workflow(lasso_workflow, best_lambda) %>%
  fit(data = train_processed)

# Extract selected features (non-zero coefficients)
lasso_coefs <- tidy(final_lasso) %>%
  filter(estimate != 0)

selected_features <- lasso_coefs$term[-1]

############################################################
# ? 7. PREPARE MATRICES FOR NEURAL NETWORK
############################################################

# Keep only selected features
train_nn <- train_processed %>%
  select(all_of(c("outcome", selected_features)))

test_nn <- test_processed %>%
  select(all_of(c("outcome", selected_features)))

# Convert to matrices
x_train <- train_nn %>% select(-outcome) %>% as.matrix()
y_train <- as.numeric(train_nn$outcome) - 1

x_test <- test_nn %>% select(-outcome) %>% as.matrix()
y_test <- as.numeric(test_nn$outcome) - 1

############################################################
# ? 8. HANDLE CLASS IMBALANCE (WEIGHTS)
############################################################

class_weights <- table(y_train)
class_weights <- sum(class_weights) / (2 * class_weights)

############################################################
# ? 9. BUILD FEEDFORWARD NEURAL NETWORK (FNN)
############################################################

build_model <- function(input_dim) {
  
  model <- keras_model_sequential()
  
  model$add(
    layer_dense(
      units = 128,
      activation = "relu",
      input_shape = c(input_dim)
    )
  )
  
  model$add(
    layer_dropout(rate = 0.4)
  )
  
  model$add(
    layer_dense(
      units = 64,
      activation = "relu"
    )
  )
  
  model$add(
    layer_dropout(rate = 0.3)
  )
  
  model$add(
    layer_dense(
      units = 1,
      activation = "sigmoid"
    )
  )
  
  model$compile(
    optimizer = optimizer_adam(learning_rate = 0.0005),
    loss = "binary_crossentropy",
    metrics = list("accuracy")
  )
  
  return(model)
}

############################################################
# ? TRAIN WITH EARLY STOPPING
############################################################
# Ensure no missing values
# ------------------------------
# ✅ Libraries
# ------------------------------
library(tidyverse)
library(keras3)
library(yardstick)

# ------------------------------
# ✅ Preprocessing
# ------------------------------

# Convert to numeric matrix
x_train <- as.matrix(x_train)
x_test  <- as.matrix(x_test)

storage.mode(x_train) <- "double"
storage.mode(x_test)  <- "double"

y_train <- as.numeric(y_train)
y_test  <- as.numeric(y_test)

# ------------------------------
# ✅ Train / Validation split
# ------------------------------
set.seed(123)

n <- nrow(x_train)
val_idx <- sample(seq_len(n), size = floor(0.2 * n))

x_val <- x_train[val_idx, ]
y_val <- y_train[val_idx]

x_tr  <- x_train[-val_idx, ]
y_tr  <- y_train[-val_idx]

# ------------------------------
# ✅ Convert to arrays (keras3 safe)
# ------------------------------
x_tr  <- array(as.numeric(x_tr), dim = dim(x_tr))
x_val <- array(as.numeric(x_val), dim = dim(x_val))
x_test <- array(as.numeric(x_test), dim = dim(x_test))

y_tr  <- as.numeric(y_tr)
y_val <- as.numeric(y_val)
y_test <- as.numeric(y_test)

# ============================================================
# IMPORTANT: Restart R session, then run from the top.
# Only load keras3 — never load library(keras) or library(tensorflow)
# ============================================================

# -- Environment vars FIRST (before any library call) -------
Sys.setenv(TF_CPP_MIN_LOG_LEVEL   = "2")
Sys.setenv(OMP_NUM_THREADS        = "4")

# -- Libraries -----------------------------------------------
library(keras3)
library(tidyverse)
library(yardstick)

# ============================================================
# FIX 1: Convert R matrices to proper Python float32 arrays
# ============================================================
x_tr   <- reticulate::np_array(x_tr,   dtype = "float32")
x_val  <- reticulate::np_array(x_val,  dtype = "float32")
x_test <- reticulate::np_array(x_test, dtype = "float32")

y_tr   <- reticulate::np_array(as.numeric(y_tr),   dtype = "float32")
y_val  <- reticulate::np_array(as.numeric(y_val),  dtype = "float32")
# keep y_test as R vector for yardstick later
y_test_r <- as.numeric(y_test)

# ============================================================
# FIX 2: build_model using $compile() not pipe-compile
# ============================================================
# build_model <- function(input_dim) {
#   
#   inputs  <- layer_input(shape = c(input_dim))
#   
#   outputs <- inputs |>
#     layer_dense(units = 64L, activation = "relu") |>
#     layer_batch_normalization() |>
#     layer_dropout(rate = 0.3) |>
#     layer_dense(units = 32L, activation = "relu") |>
#     layer_dropout(rate = 0.2) |>
#     layer_dense(units = 1L,  activation = "sigmoid")
#   
#   model <- keras_model(inputs = inputs, outputs = outputs)
#   
#   model$compile(
#     optimizer = optimizer_adam(learning_rate = 0.001),
#     loss      = "binary_crossentropy",
#     metrics   = list("AUC")
#   )
#   
#   model
# }

build_model <- function(input_dim) {
  inputs <- layer_input(shape = c(input_dim))
  
  outputs <- inputs |>
    layer_dense(units = 128L, activation = "relu") |>
    layer_batch_normalization() |>
    layer_dropout(rate = 0.4) |>
    layer_dense(units = 64L, activation = "relu") |>
    layer_batch_normalization() |>
    layer_dropout(rate = 0.3) |>
    layer_dense(units = 32L, activation = "relu") |>
    layer_dropout(rate = 0.2) |>
    layer_dense(units = 16L, activation = "relu") |>
    layer_dropout(rate = 0.1) |>
    layer_dense(units = 1L, activation = "sigmoid")
  
  model <- keras_model(inputs = inputs, outputs = outputs)
  
  model$compile(
    optimizer = optimizer_adam(learning_rate = 0.0005, clipnorm = 1.0),  # Gradient clipping
    loss = "binary_crossentropy",
    metrics = list("AUC")
  )
  
  model
}


# ============================================================
# FIX 3: class_weight must be a plain named list, not a vector
# ============================================================
class_weights_list <- list("0" = 1.0, "1" = 3.0)

# ============================================================
# Training callback
# ============================================================
es_callback <- callback_early_stopping(
  monitor              = "val_loss",
  patience             = 10L,
  restore_best_weights = TRUE
)

# ============================================================
# Training loop (sequential — TF/Keras is NOT fork-safe)
# ============================================================
results    <- vector("list", 5)
auc_values <- numeric(5)

for (i in seq_len(5)) {
  
  cat("\n--- Model", i, "of 5 ---\n")
  
  # Reproducible seed per run
  tensorflow::tf$random$set_seed(100L + i)
  set.seed(100 + i)
  
  model <- build_model(ncol(x_tr))
  
  # model$fit(
  #   x               = x_tr,
  #   y               = y_tr,
  #   epochs          = 100L,
  #   batch_size      = 32L,
  #   validation_data = list(x_val, y_val),
  #   class_weight    = class_weights_list,
  #   callbacks       = list(es_callback),
  #   verbose         = 0L
  # )
  
  # In your fit() call:
  model$fit(
    x = x_tr, y = y_tr,
    epochs = 200L,                # More epochs
    batch_size = 16L,             # Smaller batches for imbalance
    validation_data = list(x_val, y_val),
    class_weight = list("0" = 1.0, "1" = 6.0),  # Tune based on imbalance ratio
    callbacks = list(
      callback_early_stopping(monitor = "val_auc", mode = "max", patience = 15L, restore_best_weights = TRUE),
      callback_reduce_lr_on_plateau(monitor = "val_loss", factor = 0.5, patience = 7L)
    ),
    verbose = 1L  # Watch training progress
  )
  
  
  # Predict — returns a numpy matrix; flatten to R vector
  preds <- as.numeric(model$predict(x_test, verbose = 0L))
  
  # yardstick needs factor truth
  y_test_factor <- factor(y_test_r, levels = c(0, 1))
  
  auc_values[i] <- roc_auc_vec(
    truth    = y_test_factor,
    estimate = preds,
    event_level = "second"   # 1 = positive class
  )
  
  results[[i]] <- tibble(
    model_id = i,
    truth    = y_test_factor,
    pred     = preds
  )
  
  cat("  AUC =", round(auc_values[i], 4), "\n")
}

cat("\nMean AUC across 10 runs:", round(mean(auc_values), 4), "\n")

############################################################
# ? 11. MODEL EVALUATION
############################################################

# 1. Compute AUCs (your code is fine)
# Recompute AUCs CORRECTLY — specify event_level
auc_values_fixed <- map_dbl(results, function(res) {
  roc_auc_vec(
    truth = res$truth,
    estimate = res$pred,
    event_level = "second"  # "1" is positive class
  )
})
mean_auc_fixed <- mean(auc_values_fixed)
cat("Fixed Mean AUC:", round(mean_auc_fixed, 4), "\n")

# Check if inverted: if <0.5, flip preds
if (mean_auc_fixed < 0.5) {
  cat("FLIPPING PREDS (inverted sigmoid?)\n")
  for (i in seq_along(results)) {
    results[[i]]$pred <- 1 - results[[i]]$pred
  }
  auc_values_fixed <- map_dbl(results, ~ roc_auc_vec(.x$truth, .x$pred, event_level = "second"))
  mean_auc_fixed <- mean(auc_values_fixed)
  cat("Corrected Mean AUC:", round(mean_auc_fixed, 4), "\n")
}

# Verify distributions
bind_rows(results, .id = "model") %>%
  ggplot(aes(x = pred, fill = truth)) +
  geom_density(alpha = 0.5) +
  facet_wrap(~model)

# Pool ALL models for ensemble (better than single best)
all_preds <- bind_rows(results) %>%
  group_by(truth) %>%
  summarise(pred = mean(pred), .groups = "drop")  # Average probs

# Find optimal threshold
roc_data <- all_preds %>% roc_curve(truth, pred)
optimal_thresh <- roc_data %>%
  mutate(youden_j = .sensitivity + .specificity - 1) %>%
  slice_max(youden_j, n = 1) %>%
  pull(.threshold)

cat("Optimal Threshold:", round(optimal_thresh, 3), "\n")

# Re-classify
all_preds_opt <- all_preds %>%
  mutate(
    class_opt = factor(ifelse(pred > optimal_thresh, "1", "0"), levels = c("0", "1"))
  )

# New conf_mat + metrics
all_preds_opt %>%
  conf_mat(truth, class_opt) %>%
  summary()

# Ensemble probs
ensemble_preds <- bind_rows(results) %>%
  group_by(truth) %>%
  summarise(
    pred_ensemble = mean(pred),
    n_models = n(),
    .groups = "drop"
  )

# Metrics
roc_auc_vec(ensemble_preds$truth, ensemble_preds$pred_ensemble)


# LASSO VIP (already have lasso_coefs)
lasso_vip <- lasso_coefs %>%
  mutate(
    importance = abs(estimate),
    term = str_remove(term, "^`+|`+$")  # Clean dummy vars
  ) %>%
  arrange(desc(importance)) %>%
  slice_head(n = 20)

lasso_vip %>%
  ggplot(aes(y = reorder(term, importance), x = importance)) +
  geom_col() +
  labs(y = "Features", title = "LASSO Variable Importance")

# NN VIP (Permutation Importance — on ensemble)
library(vip)  # Already loaded

# Retrain 1 model on full train for VIP (use best config)
final_model <- build_model(ncol(x_tr))
final_model$fit(x_tr, y_tr, epochs = 50, verbose = 0, class_weight = class_weights_list)
vip(final_model, train = as.data.frame(x_tr), target = "outcome", method = "permute", nsim = 10)


# Save data/models
saveRDS(results, "nn_ensemble_results.rds")
saveRDS(lasso_coefs, "lasso_features.rds")
saveRDS(ensemble_preds, "final_predictions.rds")

# Full metrics table
final_metrics <- all_preds_opt %>%
  conf_mat(truth, class_opt) %>%
  summary() %>%
  mutate(model = "NN Ensemble (Opt Threshold)")

print(final_metrics)

# Export to CSV
final_metrics %>% write_csv("model_metrics.csv")
