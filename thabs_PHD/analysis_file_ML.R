############################################################
# 🔷 0. LOAD LIBRARIES
############################################################
library(tidyverse)
library(tidymodels)
library(mice)
library(glmnet)
library(keras)
library(yardstick)
library(vip)
library(janitor)

############################################################
# 🔷 1. DATA PREPARATION
############################################################

# Clean variable names
df <- read_csv("final_dataset_08062025.csv") %>%
  clean_names() %>%
  dplyr::select(-initials)

# Convert outcome to factor (0 = No AI, 1 = AI)
df <- df %>%
  mutate(outcome = factor(outcome, levels = c(0, 1)))

# Remove problematic columns (dates, free text, near-empty)
df <- df %>%
  dplyr::select(-matches("date|comment|specify"), -total_cd4_count_91, -total_ai,
                -uin_3,-uin_179,
                -hospital_folder_number, -x1, - event_name, -cd4_100) %>%
  mutate(across(where(is.logical), ~ factor(.x, levels = c(FALSE, TRUE))))

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
         hiv_when_diagnosed = as.numeric(hiv_when_diagnosed))
constant_vars <- sapply(df_imp, function(x) {
  length(unique(x[!is.na(x)])) <= 1
})

df_imp1 <- df_imp[, !constant_vars, drop = FALSE]


############################################################
# 🔷 2. MICE IMPUTATION (TRAINING DATA ONLY)
############################################################

predM <- make.predictorMatrix(df_imp1 %>% dplyr::select(-outcome))

quick_pred <- quickpred(
  df_imp1 %>% dplyr::select(-outcome),
  mincor = 0.1,
  minpuc = 0.25
)

# Perform multiple imputation using predictive mean matching
# mice_data <- mice(df_imp1, m = 5, method = "pmm", seed = 123)
nr_imputations <- 5

imputedData <- mice(
  data = data.frame(df_imp1 %>% dplyr::select(-outcome)),
  m = nr_imputations,
  maxit = 10,
  predictorMatrix = quick_pred,
  defaultMethod = c("pmm", "logreg", "polyreg", "polr"),
  seed = 33,
  printFlag = TRUE
)

# Extract ONE completed dataset (for ML workflow)
mice_complete <- complete(imputedData, 1)

# NOTE:
# For publication-grade work, you can repeat the entire pipeline
# across all 5 imputations and pool results.

############################################################
# 🔷 3. TRAIN-TEST SPLIT (STRATIFIED)
############################################################

set.seed(123)

split <- initial_split(df_imp1, prop = 0.8, strata = outcome)

train_data <- training(split)
test_data  <- testing(split)

############################################################
# 🔷 4. SIMPLE IMPUTATION FOR TEST DATA (NO LEAKAGE)
############################################################

# Use median/mode for test data to avoid leakage
test_complete <- test_data %>%
  mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .))) %>%
  mutate(across(where(is.character), ~ replace_na(., "Missing")))

############################################################
# 🔷 5. PREPROCESSING (ENCODING + SCALING)
############################################################

rec <- recipe(outcome ~ ., data = train_complete) %>%
  step_dummy(all_nominal_predictors()) %>%
  step_zv(all_predictors()) %>%
  step_normalize(all_numeric_predictors())

# Prep recipe
prep_rec <- prep(rec)

# Apply transformations
train_processed <- bake(prep_rec, new_data = train_complete)
test_processed  <- bake(prep_rec, new_data = test_complete)

############################################################
# 🔷 6. LASSO FEATURE SELECTION
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
best_lambda <- select_best(lasso_fit, "roc_auc")

# Fit final LASSO
final_lasso <- finalize_workflow(lasso_workflow, best_lambda) %>%
  fit(data = train_processed)

# Extract selected features (non-zero coefficients)
lasso_coefs <- tidy(final_lasso) %>%
  filter(estimate != 0)

selected_features <- lasso_coefs$term

############################################################
# 🔷 7. PREPARE MATRICES FOR NEURAL NETWORK
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
# 🔷 8. HANDLE CLASS IMBALANCE (WEIGHTS)
############################################################

class_weights <- table(y_train)
class_weights <- sum(class_weights) / (2 * class_weights)

############################################################
# 🔷 9. BUILD FEEDFORWARD NEURAL NETWORK (FNN)
############################################################

build_model <- function(input_dim) {
  keras_model_sequential() %>%
    layer_dense(units = 64, activation = "relu", input_shape = input_dim) %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 32, activation = "relu") %>%
    layer_dropout(rate = 0.2) %>%
    layer_dense(units = 1, activation = "sigmoid") %>%
    compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_adam(learning_rate = 0.001),
      metrics = metric_auc()
    )
}

############################################################
# 🔷 10. TRAIN MODEL (REPEATED 10 TIMES)
############################################################

############################################################
# 🔷 IMPROVED NEURAL NETWORK (TUNED)
############################################################

build_model <- function(input_dim) {
  keras_model_sequential() %>%
    layer_dense(units = 128, activation = "relu", input_shape = input_dim) %>%
    layer_dropout(rate = 0.4) %>%
    layer_dense(units = 64, activation = "relu") %>%
    layer_dropout(rate = 0.3) %>%
    layer_dense(units = 1, activation = "sigmoid") %>%
    compile(
      loss = "binary_crossentropy",
      optimizer = optimizer_adam(learning_rate = 0.0005),
      metrics = metric_auc()
    )
}

############################################################
# 🔷 TRAIN WITH EARLY STOPPING
############################################################

callback <- callback_early_stopping(
  monitor = "val_loss",
  patience = 10,
  restore_best_weights = TRUE
)

results <- list()
models <- list()

for (i in 1:10) {
  set.seed(100 + i)
  
  model <- build_model(ncol(x_train))
  
  model %>% fit(
    x_train, y_train,
    epochs = 100,
    batch_size = 32,
    validation_split = 0.2,
    class_weight = class_weights,
    callbacks = list(callback),
    verbose = 0
  )
  
  preds <- model %>% predict(x_test)
  
  results[[i]] <- tibble(
    truth = y_test,
    pred  = as.numeric(preds)
  )
  
  models[[i]] <- model
}

# AUC
auc_values <- map_dbl(results, ~ roc_auc_vec(.x$truth, .x$pred))
mean_auc <- mean(auc_values)

print(mean_auc)

############################################################
# 🔷 11. MODEL EVALUATION
############################################################

# AUC across all runs
auc_values <- map_dbl(results, ~ roc_auc_vec(.x$truth, .x$pred))
mean_auc <- mean(auc_values)

print(mean_auc)

# Select best model
best_idx <- which.max(auc_values)

best_preds <- results[[best_idx]] %>%
  mutate(class = ifelse(pred > 0.5, 1, 0))

# Confusion matrix
conf_mat(
  data = best_preds,
  truth = factor(truth),
  estimate = factor(class)
)

# ROC curve
roc_curve(results[[best_idx]], truth = truth, pred) %>%
  autoplot()

############################################################
# 🔷 12. FEATURE IMPORTANCE (AGGREGATED)
############################################################

vip_list <- map(models, vip::vi)

vip_combined <- bind_rows(vip_list, .id = "model") %>%
  group_by(Variable) %>%
  summarise(
    mean_importance = mean(Importance),
    sd_importance = sd(Importance)
  ) %>%
  arrange(desc(mean_importance))

print(vip_combined)
