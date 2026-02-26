library(tidyverse)
library(magrittr)
library(knitr)
# library(kableExtra)
library(tinytex)
library(gmodels)
library(readxl)
# library(MASS)
library(gtsummary)
library(caret)
library(leaps)
library(MASS)
library(predtools)
library(randomForest)
library(mice)
# library(psfmi)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)
library(ggpubr)
library(rstatix)

##############################################################################################
library(tidyverse)
library(magrittr)
library(knitr)
# library(kableExtra)
library(tinytex)
library(gmodels)
library(readxl)
# library(MASS)
library(gtsummary)
library(caret)
library(leaps)
library(MASS)
library(predtools)
library(randomForest)
library(mice)
# library(psfmi)
library(survival)
library(ranger)
library(ggplot2)
library(dplyr)
library(ggfortify)
library(survminer)
library(ggpubr)
library(rstatix)

##############################################################################################
final_dataset <- read_excel("hypo_alldata_wide_labels.xls") %>%
  filter(!is.na(UIN...1)) %>%
  mutate(
    # --- Primary AI classifications ---
    addisons_disease = if_else(as.numeric(`Random cortisol result`) < 240, 1, 0),
    
    PAI = case_when(
      as.numeric(`Random cortisol result`) < 240 &
        as.numeric(`Synacthen: 30 minute cortisol result`) < 240 &
        as.numeric(`ACTH result`) >= 64.7 & !is.na(`ACTH result`) ~ 1,
      TRUE ~ 0
    ),
    
    SAI = case_when(
      as.numeric(`Random cortisol result`) < 240 &
        as.numeric(`Synacthen: 30 minute cortisol result`) < 240 &
        as.numeric(`ACTH result`) < 64.7 ~ 1,
      TRUE ~ 0
    ),
    
    total_AI = case_when(
      as.numeric(`Random cortisol result`) < 240 & (PAI == 1 | SAI == 1) ~ 1,
      as.numeric(`Random cortisol result`) < 240 & (PAI != 1 & SAI != 1) ~ 0,
      TRUE ~ NA_real_
    ),
    
    addisons_disease = if_else(is.na(total_AI), 0, total_AI),
    
    # Override Addisons classification for specified UINs
    addisons_disease = case_when(
      UIN...1 %in% c(392, 435, 478, 488, 496) ~ 1,
      `Random cortisol result` <= 274 &
        (is.na(`Synacthen: 0 minute cortisol result`) | is.na(`Synacthen: 30 minute cortisol result`)) ~ 1,
      UIN...1 == 500 ~ 0,
      TRUE ~ addisons_disease
    ),
    
    # Update PAI and SAI based on clinical logic
    PAI = case_when(
      `Random cortisol result` <= 274 &
        (is.na(`Synacthen: 0 minute cortisol result`) | is.na(`Synacthen: 30 minute cortisol result`)) &
        as.numeric(`ACTH result`) >= 64.7 & !is.na(`ACTH result`) &
        PAI %in% c(0, NA) ~ 1,
      UIN...1 == 392 ~ 1,
      TRUE ~ PAI
    ),
    
    SAI = case_when(
      `Random cortisol result` <= 274 &
        (is.na(`Synacthen: 0 minute cortisol result`) | is.na(`Synacthen: 30 minute cortisol result`)) &
        as.numeric(`ACTH result`) < 64.7 &
        SAI %in% c(0, NA) ~ 1,
      TRUE ~ SAI
    ),
    
    total_AI = case_when(
      PAI == 1 | SAI == 1 ~ 1,
      PAI != 1 & SAI != 1 ~ 0,
      TRUE ~ NA_real_
    ),
    
    # --- AI classification label ---
    AI = case_when(
      PAI == 1 ~ "PAI",
      SAI == 1 ~ "SAI",
      TRUE ~ ""
    ),
    AI = case_when(
      UIN...1 %in% c(435, 478, 488, 496) ~ "SAI",
      UIN...1 %in% c(392) ~ "PAI",
      TRUE ~ AI
    ),
    
    `Adrenal insufficiency` = if_else(!is.na(addisons_disease), addisons_disease, 0),
    
    # --- Binary symptom conversions ---
    across(
      c(`Increased pigmentation of the skin`, `Loss of consciousness`, `Hypoglycaemia`,
        Dizziness, Shock, `Presence of anaemia`, Hypotension, Weakness, Tiredness,
        `Poor appetite`, `Weight loss`, Nausea, Vomiting, `Liking for salt`,
        Diarrhoea, Anorexia, `Any postural drop in blood pressure`),
      ~ .x == "Yes",
      .names = "{.col}"
    ),
    
    # Handle missing hair-loss variable
    `Loss of axillary and pubic hair, if female` =
      if_else(`Loss of axillary and pubic hair, if female` == "NA", "No",
              `Loss of axillary and pubic hair, if female`),
    
    # --- Cortisol increment calculations ---
    incremental_cortisol = abs(`Synacthen: 30 minute cortisol result` -
                                 `Synacthen: 0 minute cortisol result`),
    incremental_cortisol_percent_increase =
      abs(incremental_cortisol) / `Synacthen: 0 minute cortisol result` * 100,
    incremental_cortisol = if_else(incremental_cortisol < 0, NA_real_, incremental_cortisol)
  )
#%>%
#   dplyr::select(id = UIN...1, `Hospital folder number`, `Primary medical conditions (1)`, `Primary medical conditions (2)`, `Primary medical conditions (3)`, `Primary medical conditions (4)`, `Co-existing medical conditions (2)`, `Other, specify...67`,
#                 `Opportunistic infection  (choice=Tuberculosis)`, `Opportunistic infection  (choice=Cryptococcus neoformans)`,
# `Opportunistic infection  (choice=Toxoplasmosis)`, `Opportunistic infection  (choice=Mycobacterium avium-intracellulare)`, `Opportunistic infection  (choice=Kaposis sarcoma)`, `Opportunistic infection  (choice=Cytomegalovirus)`,`Opportunistic infection  (choice=Other)`) 
# write.csv(final_dataset, 'opportunistic infections_554.csv')
# dt01 <- read_csv("opportunistic infections_554.csv") %>%
#   filter(!(`Hospital folder number` %in% read_excel("hypo_alldata_wide_labels 549.xlsx")$`Hospital folder number`))
# write.csv(dt01, 'opportunistic infections_updated_549_to_554.csv')
dat <- final_dataset %>%
  dplyr::select(`Hospital folder number`,`Random cortisol result`, `ACTH sample drawn`, `ACTH result`, `Synacthen: 0 minute cortisol result`, `Synacthen: 30 minute cortisol result`, addisons_disease)
write.csv(dat, 'outcome_dat.csv')
final_dataset <- final_dataset %>%
  left_join(bind_rows(
    read_excel("Updated OI's  05 March 2024.xlsx"), 
    read_excel("opportunistic infections_additional_patients +OI_updated.xlsx"),
    read_excel("opportunistic infections_additional_patients +OI - 554.xlsx")
  ) %>%
    mutate(Tuberculosis = as.logical(`Opportunistic infection  (choice=Tuberculosis)`), 
           `Cryptococcus neoformans` = as.logical(`Opportunistic infection  (choice=Cryptococcus neoformans)`), 
           Pneumonia = as.logical(`Opportunistic infection  (choice=Pneumonia)`),
           `Staph aureus` = as.logical(`Opportunistic infection  (choice=Staph aureus)`), 
           `Kaposis sarcoma` = as.logical(`Opportunistic infection  (choice=Kaposis sarcoma)`),
           Cytomegalovirus = as.logical(`Opportunistic infection  (choice=Cytomegalovirus)`), 
           HSV = as.logical(`Opportunistic infection  (choice=HSV)`), 
           HepB = as.logical(`Opportunistic infection  (choice=HepB)`),
           Candida = as.logical(`Opportunistic infection  (choice=Candida)`),
           Other = as.logical(`Opportunistic infection  (choice=Other)`),
           `GE/c diff`  = as.logical(`Opportunistic infection  (choice=GE / C diff)`), 
           `Parvo B19` = as.logical(`Opportunistic infection  (choice=Parvo B19)`),
           `Syphilis` = as.logical(`Opportunistic infection  (choice=Syphilis)`),
           `B menigitis` = as.logical(`Opportunistic infection  (choice=B menigitis)`), 
           `UTI / Leptospirosis` = as.logical(`Opportunistic infection  (choice=UTI / Leptospirosis)`), 
           PCP = as.logical(`Opportunistic infection  (choice=PCP)`), 
           `COVID-19` = as.logical(`Opportunistic infection  (choice=covid)`),
           `Neurocysticercosis` = as.logical(`Opportunistic infection  (choice=Neurocysticercosis)`)) %>%
    dplyr::select(`Hospital folder number`, Tuberculosis,  `Cryptococcus neoformans`, Pneumonia, `Staph aureus`, `Kaposis sarcoma`, Cytomegalovirus, HSV, HepB, Candida, Other, `GE/c diff`, `Parvo B19`, `Syphilis`, `B menigitis`, `UTI / Leptospirosis`, PCP, `COVID-19`, `Neurocysticercosis`) %>% 
    replace(is.na(.), 0), by = 'Hospital folder number') %>%
  left_join(read_delim("opportunistic infections_554 - TB.csv", 
                       delim = ";", escape_double = FALSE, trim_ws = TRUE) %>%
              dplyr::select(`Hospital folder number`, PTB, EPTB), by = 'Hospital folder number') %>%
  mutate(`White cell count` = as.numeric(`White cell count`),
         `Lymphocyte count` = as.numeric(`Lymphocyte count`),
         Neutrophils = as.numeric(Neutrophils))
# cod_oi <- read_excel("hypo_alldata_wide_labels_cod_oi.xls") %>%
#   dplyr::select(`Hospital folder number`, `If deceased, cause of death`)
# write.csv(final_dataset %>% 
#             # mutate(total_AI = ifelse(is.na(total_AI), 0, total_AI)) %>%
#             relocate(addisons_disease, .after = `Hospital folder number`), "Full_528_dataset.csv")
# write.csv(final_dataset %>% 
#             # mutate(total_AI = ifelse(is.na(total_AI), 0, total_AI)) %>%
#             relocate(addisons_disease, .after = `Hospital folder number`) %>%
#             dplyr::select(`Hospital folder number`, addisons_disease, `Random cortisol result`, 
#                           `Synacthen: 0 minute cortisol result`, `ACTH result`, 
#                           `Synacthen: 30 minute cortisol result`), "stimulation_test_dataset.csv")
# 
# dt02 <- final_dataset %>%
#   filter(!(`Hospital folder number` %in% read_csv("opportunistic infections.csv")$`Hospital folder number`)) %>%
#   dplyr::select(id = `Hospital folder number`, `Concomitant therapy (1)`, `Concomitant therapy (2)`, `Concomitant therapy (3)`, `Concomitant therapy (4)`, `Concomitant therapy (5)`, `Concomitant therapy (6)`, `Concomitant therapy (7)`, `Concomitant therapy (8)`, `Concomitant therapy (9)`, `Concomitant therapy (10)`) %>%
#   pivot_longer(cols=c(`Concomitant therapy (1)`, `Concomitant therapy (2)`, `Concomitant therapy (3)`, `Concomitant therapy (4)`, `Concomitant therapy (5)`, `Concomitant therapy (6)`, `Concomitant therapy (7)`, `Concomitant therapy (8)`, `Concomitant therapy (9)`, `Concomitant therapy (10)`),
#                     names_to='drug',
#                     values_to='drug2') %>%
#   filter(drug2 != 'NA')
# write.csv(dt02, 'list of drugs_additional_patients.csv')
dt03 <- bind_rows(
  read_delim("list of drugs - updated 23 April24 TB & Fungal .csv",
             delim = ";", escape_double = FALSE, trim_ws = TRUE
  ) %>%
    dplyr::select(id, ART, tb, kidney, fungal, rifampicin, fluconazole, opiates),
  read_delim("list of drugs_additional_patients + CKD, TB & Fungal.csv",
             delim = ";", escape_double = FALSE, trim_ws = TRUE
  )
) %>%
  dplyr::select(id, ART, tb = TB, kidney, fungal = Fungal, rifampicin, fluconazole, opiates) %>%
  group_by(id) %>%
  mutate(
    `ART exposure` = max(ifelse(is.na(ART), 0, ART), na.rm = T),
    `Kidney medication` = max(ifelse(is.na(kidney), 0, kidney), na.rm = T),
    `TB medication` = max(ifelse(is.na(tb), 0, tb), na.rm = T),
    `Fungal medication` = max(ifelse(is.na(fungal), 0, fungal), na.rm = T),
    `rifampicin` = max(ifelse(is.na(rifampicin), 0, rifampicin), na.rm = T),
    `fluconazole` = max(ifelse(is.na(fluconazole), 0, fluconazole), na.rm = T),
    `opiates` = max(ifelse(is.na(opiates), 0, opiates), na.rm = T)
  ) %>%
  ungroup() %>%
  distinct(id, .keep_all = T) %>%
  dplyr::select(id, `ART exposure`, `Kidney medication`, `TB medication`, `Fungal medication`,
                Rifampicin = `rifampicin`, Fluconazole = `fluconazole`, `Opiates` = `opiates`
  )

final_dataset <- final_dataset %>%
  # dplyr::select(id = `Hospital folder number`, `CD4 count` = `Total CD4 count...10`) %>%
  left_join(dt03 %>%
              mutate(`Hospital folder number` = id), by = "Hospital folder number") %>%
  mutate(
    `ART exposure` = as.factor(ifelse(is.na(`ART exposure`), "No",
                                      ifelse(`ART exposure` == 1, "Yes",
                                             ifelse(`ART exposure` == 0, "No", `ART exposure`)
                                      )
    )),
    `Kidney medication` = as.factor(ifelse(is.na(`Kidney medication`), "No",
                                           ifelse(`Kidney medication` == 1, "Yes",
                                                  ifelse(`Kidney medication` == 0, "No", `Kidney medication`)
                                           )
    )),
    `TB medication` = as.factor(ifelse(is.na(`TB medication`), "No",
                                       ifelse(`TB medication` == 1, "Yes",
                                              ifelse(`TB medication` == 0, "No", `TB medication`)
                                       )
    )),
    `Fungal medication` = as.factor(ifelse(is.na(`Fungal medication`), "No",
                                           ifelse(`Fungal medication` == 1, "Yes",
                                                  ifelse(`Fungal medication` == 0, "No", `Fungal medication`)
                                           )
    )),
    Rifampicin = as.factor(ifelse(is.na(Rifampicin), "No",
                                  ifelse(Rifampicin == 1, "Yes",
                                         ifelse(Rifampicin == 0, "No", Rifampicin)
                                  )
    )),
    Fluconazole = as.factor(ifelse(is.na(Fluconazole), "No",
                                   ifelse(Fluconazole == 1, "Yes",
                                          ifelse(Fluconazole == 0, "No", Fluconazole)
                                   )
    )),
    Opiates = as.factor(ifelse(is.na(Opiates), "No",
                               ifelse(Opiates == 1, "Yes",
                                      ifelse(Opiates == 0, "No", Opiates)
                               )
    ))
  ) # %>%
# mutate(`CD4 count` = as.numeric(`CD4 count`))
x=final_dataset %>% filter(is.na(addisons_disease)) %>% dplyr::select(UIN...1, `Random cortisol result`, `ACTH result`, `Synacthen: 0 minute cortisol result`, `Synacthen: 30 minute cortisol result`)
x=final_dataset %>% filter(`Random cortisol result`<500 & (!is.na(`Synacthen: 0 minute cortisol result`) | !is.na(`Synacthen: 30 minute cortisol result`))) %>% dplyr::select(UIN...1, `Random cortisol result`, `ACTH result`, `Synacthen: 0 minute cortisol result`, `Synacthen: 30 minute cortisol result`)
##############################################################################################
table_mortality <- final_dataset %>%
  dplyr::select(`Hospital folder number`, `3 followup_patientstatus`, `6 followup_patientstatus`, `12 followup_patientstatus`) %>%
  pivot_longer(cols = c(`3 followup_patientstatus`, `6 followup_patientstatus`, `12 followup_patientstatus`),
               names_to = 'col_num',
               values_to = 'followup_patientstatus') %>%
  dplyr::select(`Hospital folder number`, `followup_patientstatus`) %>%
  group_by(`Hospital folder number`) %>%
  mutate(mortality = ifelse(followup_patientstatus == 'Deceased --> EXIT form', 1,ifelse(followup_patientstatus == 'Alive', 0, 0))) %>%
  dplyr::select(`Hospital folder number`, mortality)
table_mortality[is.na(table_mortality)] <- 0
table_mortality1 <- table_mortality %>%
  group_by(`Hospital folder number`) %>%
  mutate(mortality = max(mortality)) %>%
  distinct(`Hospital folder number`, .keep_all = T)

time_to_death <- final_dataset%>%
  dplyr::select(`Hospital folder number`, `Point of exit from study`, `3 followup_patientstatus`,
                `6 followup_patientstatus`, `12 followup_patientstatus`
  ) %>%
  mutate(flag1 = ifelse(`3 followup_patientstatus` == "Deceased --> EXIT form", 3, NA),
         flag2 = ifelse(`6 followup_patientstatus` == "Deceased --> EXIT form", 6, NA),
         flag3 = ifelse(`12 followup_patientstatus` == "Deceased --> EXIT form", 12, NA)) %>%
  dplyr::select(`Hospital folder number`, `Point of exit from study`, flag1:flag3) %>%
  pivot_longer(cols = c(flag1:flag3),
               names_to = 'col_num',
               values_to = 'time to death') %>%
  dplyr::select(`Hospital folder number`, `Point of exit from study`, `time to death`) %>%
  group_by(`Hospital folder number`) %>%
  mutate(ttdeath = min(`time to death`, na.rm = T)) %>%
  ungroup() %>%
  mutate(ttdeath = ifelse(is.infinite(ttdeath), 12, ttdeath)) %>%
  distinct(`Hospital folder number`, .keep_all = T) %>%
  dplyr::select(`Hospital folder number`, ttdeath)

interval_deaths <- final_dataset%>%
  dplyr::select(`Hospital folder number`, `3 followup_patientstatus`,
                `6 followup_patientstatus`, `12 followup_patientstatus`
  ) %>%
  mutate(flag1 = ifelse(`3 followup_patientstatus` == "Deceased --> EXIT form", 3, NA),
         flag2 = ifelse(`6 followup_patientstatus` == "Deceased --> EXIT form", 6, NA),
         flag3 = ifelse(`12 followup_patientstatus` == "Deceased --> EXIT form", 12, NA)) %>%
  dplyr::select(`Hospital folder number`, flag1:flag3) %>%
  pivot_longer(cols = c(flag1:flag3),
               names_to = 'col_num',
               values_to = 'time to death') %>%
  dplyr::select(`Hospital folder number`, `time to death`) %>%
  group_by(`Hospital folder number`) %>%
  mutate(ttdeath = min(`time to death`, na.rm = T)) %>%
  ungroup()
x <- interval_deaths %>% filter(!is.na(`time to death`))%>% distinct(`Hospital folder number`, .keep_all = T)
# table(x$ttdeath)

table_mortality1 <- table_mortality1 %>%
  left_join(time_to_death, by = 'Hospital folder number')

table_3 <- final_dataset %>%
  left_join(table_mortality1, by = 'Hospital folder number') %>%
  mutate(Ethnicity = as.character(Ethnicity),
         mortality = ifelse(`Point of exit from study` == 'Deceased', 1, 0)) %>%
  mutate(mortality = ifelse(is.na(mortality), 0, mortality)) %>%
  mutate(
    `log10 viral load` = log(as.numeric(`HIV viral load`, 10)),
    `Age at enrolment` = as.numeric(`Age at enrolment`),
    `Duration of current illness` = as.numeric(`Duration of current illness`),
    `Total CD4 count` = as.numeric(`Total CD4 count...10`),
    discharged = ifelse(!is.na(`Date of discharge`), 'Yes', 'No'),
    HIV_duration = as.numeric(as.Date(`Date of enrolment in study`, format='%m/%d/%Y') - as.Date(`HIV, when diagnosed`, format='%m/%d/%Y')),
    `Gender` = (ifelse(Gender == 'NA', 'Unknown', Gender)),
    `Addisons disease`  = as.factor(addisons_disease),
    Sodium = as.numeric(Sodium),
    Potassium = as.numeric(Potassium),
    Haemoglobin = as.numeric(Haemoglobin), 
    `White cell count` = as.numeric(`White cell count`), 
    `Lymphocyte count` = as.numeric(`Lymphocyte count`), 
    Neutrophils = as.numeric(Neutrophils),
    Ethnicity = ifelse(Ethnicity == "Asian" | Ethnicity == "Coloured" | Ethnicity == "White", "Other", ifelse(Ethnicity == 'NA', 'Unknown', Ethnicity)),
    `Toxoplasmosis` = as.factor(`Opportunistic infection  (choice=Toxoplasmosis)`),
    `Random cortisol result` = as.numeric(`Random cortisol result`), 
    `Synacthen: 0 minute cortisol result` = as.numeric(`Synacthen: 0 minute cortisol result`),
    `Synacthen: 30 minute cortisol result` = as.numeric(`Synacthen: 30 minute cortisol result`), 
    `ACTH result` = as.numeric(`ACTH result`), 
    `BP (systolic)` = as.numeric(`BP (systolic)`), 
    `BP (diastolic)` = as.numeric(`BP (diastolic)`), 
    `Heart rate` = as.numeric(`Heart rate`),
    `Toxoplasmosis` = as.factor(`Opportunistic infection  (choice=Toxoplasmosis)`),
    AI_recoded = as.factor(ifelse(AI %in% c('PAI', 'SAI') | UIN...1 %in% c(392, 435, 478, 488, 496, 500), 'AI', 'No-AI')),
    mortality = ifelse(`Point of exit from study` == 'Deceased', 1,  0)) %>%
  mutate(mortality = ifelse(is.na(mortality), 0, mortality)) %>%
  dplyr::select(record_id = UIN...1, `Age at enrolment`, Gender, Ethnicity, `Duration of current illness`, 
                `log10 viral load`, `Total CD4 count`, Sodium, Potassium, Haemoglobin, `White cell count`, 
                `Lymphocyte count`, Neutrophils, `Addisons disease`, mortality, ttdeath, `Random cortisol result`, 
                `Synacthen: 0 minute cortisol result`, `Synacthen: 30 minute cortisol result`, `ACTH result`, 
                `BP (systolic)`, `BP (diastolic)`, discharged,
                `Heart rate`, Hypotension, Weakness, Tiredness, `Poor appetite`, `Weight loss`, 
                `Increased pigmentation of the skin`, Nausea, Vomiting, `Liking for salt`, Hypoglycaemia, 
                `Loss of consciousness`, Diarrhoea, Dizziness, Shock, Anorexia, 
                `Loss of axillary and pubic hair, if female`, `Any postural drop in blood pressure`, 
                `Presence of anaemia`, #`Presence of an opportunistic infection` = `Opportunistic infection present`,
                Tuberculosis, PTB, EPTB,  `Cryptococcus neoformans`, Pneumonia, `Staph aureus`, `Kaposis sarcoma`, 
                Cytomegalovirus, HSV, HepB, Candida, `GE/c diff`, `Parvo B19`, `Syphilis`, `B menigitis`, 
                `UTI / Leptospirosis`, PCP, `COVID-19`, `Neurocysticercosis`, `Date of enrolment in study`, 
                `HIV, when diagnosed`, AI_recoded, AI, total_AI, `ART exposure`, `Kidney medication`, 
                incremental_cortisol,
                incremental_cortisol_percent_increase, Rifampicin, Fluconazole, Opiates) %>%
  mutate(Ethnicity = as.factor(Ethnicity),
         Gender = as.factor(Gender),
         Addisons_disease = as.numeric(as.factor(`Addisons disease`)),
         Log10_viralload = `log10 viral load`,
         Age_at_enrolment = `Age at enrolment`,
         Total_CD4_count = `Total CD4 count`,
         White_cell_count = `White cell count`,
         Lymphocyte_count = `Lymphocyte count`,
         Duration_of_current_illness = `Duration of current illness`) %>%
  dplyr::select(record_id, Age_at_enrolment, Gender, Ethnicity, Duration_of_current_illness, 
                `Viral load (log10 Copies/mL)` = Log10_viralload, `CD4 count` = Total_CD4_count, Sodium, 
                `Potassium mmol/L` = Potassium, Haemoglobin, White_cell_count, Lymphocyte_count, 
                Neutrophils, Addisons_disease, mortality, ttdeath,
                `Random cortisol` = `Random cortisol result`, `Basal cortisol` = `Synacthen: 0 minute cortisol result`, 
                `Stimulated cortisol` = `Synacthen: 30 minute cortisol result`, `ACTH` = `ACTH result`, `BP (systolic)`, 
                `BP (diastolic)`, `Addisons disease`, discharged, `Heart rate`, Hypotension, Weakness, Tiredness, 
                `Poor appetite`, `Weight loss`, `Increased pigmentation of the skin`, Nausea, Vomiting, 
                `Liking for salt`, Hypoglycaemia, `Loss of consciousness`, Diarrhoea, Dizziness, Shock, 
                Anorexia, `Loss of axillary and pubic hair, if female`, `Any postural drop in blood pressure`, 
                `Presence of anaemia`, #`Presence of an opportunistic infection`, 
                Tuberculosis, PTB, EPTB,  `Cryptococcus neoformans`, Pneumonia, `Staph aureus`, `Kaposis sarcoma`, Cytomegalovirus, HSV, HepB, Candida, `GE/c diff`, `Parvo B19`, `Syphilis`, `B menigitis`, `UTI / Leptospirosis`, PCP, `COVID-19`, `Neurocysticercosis`, `Date of enrolment in study`, `HIV, when diagnosed`, AI_recoded, AI, total_AI, `ART exposure`, `Kidney medication`, Rifampicin, Fluconazole, Opiates, incremental_cortisol, incremental_cortisol_percent_increase)

################################################################################################

## KM curves
dt04 <- table_3 %>% 
  mutate(strata = ifelse(is.na(AI), 'No AI', AI)) %>% 
  mutate(strata = ifelse(strata == "", 'No AI', strata))

# Fit the Kaplan-Meier survival model
km_fit <- survfit(Surv(time = ttdeath, event = mortality) ~ strata, data = dt04)

# Plot the Kaplan-Meier curve
# km_plot1 <- ggsurvplot(
#   km_fit,
#   data = dt04,
#   # risk.table = TRUE,           # Add a risk table below the plot
#   pval = TRUE,                 # Show p-value for log-rank test
#   # conf.int = TRUE,             # Add confidence intervals
#   legend.title = "Strata",     # Customize legend title
#   legend.labs = c("No AI", "PAI", "SAI"),  # Customize group labels
#   xlab = "Time",               # Label for x-axis
#   ylab = "Survival Probability", # Label for y-axis
#   ggtheme = theme_minimal()    # Use a minimal theme
# )
# ggsave("Kaplan_Meier_Curve1.png", plot = km_plot1$plot, width = 8, height = 6, dpi = 300)
# 
# ggsurvplot(
#   survfit(Surv(time = ttdeath, event = mortality) ~ strata, data = dt04 %>%
#             mutate(strata = ifelse(strata != "No AI", 'AI', strata ))),
#   data = dt04,
#   # risk.table = TRUE,           # Add a risk table below the plot
#   pval = TRUE,                 # Show p-value for log-rank test
#   # conf.int = TRUE,             # Add confidence intervals
#   legend.title = "Strata",     # Customize legend title
#   legend.labs = c("AI", "No AI"),  # Customize group labels
#   xlab = "Time",               # Label for x-axis
#   ylab = "Survival Probability", # Label for y-axis
#   ggtheme = theme_minimal()    # Use a minimal theme
# )

# dev.off()

####P-values at diferent timepoints
# Define specific time points
time_points <- c(3, 6, 12)

# Initialize a data frame to store p-values
p_values <- data.frame(
  Time = time_points,
  P_Value = NA
)

km_fit <- survfit(Surv(time = ttdeath, event = mortality) ~ strata, data = dt04)

# Loop through each time point
for (i in seq_along(time_points)) {
  # Survival probabilities at the specific time point
  surv_probs <- summary(km_fit, times = time_points[i])$surv
  
  # Perform a pairwise log-rank test for survival differences
  surv_diff <- survdiff(Surv(time = ttdeath, event = mortality) ~ strata, data = dt04, 
                        subset = (ttdeath >= time_points[i]))
  
  # Extract the p-value (if groups > 2, chi-squared p-value)
  p_values$P_Value[i] <- 1 - pchisq(surv_diff$chisq, df = length(unique(dt04$strata)) - 1)
}

# Display p-values
print(p_values)

### for strata of two groups AI vs Non_AI
# Define specific time points
time_points <- c(3, 6, 12)

# Initialize a data frame to store p-values
p_values <- data.frame(
  Time = time_points,
  P_Value = NA
)

km_fit2 <- survfit(Surv(time = ttdeath, event = mortality) ~ strata, data = dt04 %>%
                     mutate(strata = ifelse(strata != "No AI", 'AI', strata )))
# Plot the Kaplan-Meier curve

# km_plot2 <- ggsurvplot(
#   km_fit2,
#   data = dt04 %>%
#             mutate(strata = ifelse(strata != "No AI", 'AI', strata )),
#   # risk.table = TRUE,           # Add a risk table below the plot
#   pval = TRUE,                 # Show p-value for log-rank test
#   # conf.int = TRUE,             # Add confidence intervals
#   legend.title = "Strata",     # Customize legend title
#   legend.labs = c("AI", "No AI"),  # Customize group labels
#   xlab = "Time",               # Label for x-axis
#   ylab = "Survival Probability", # Label for y-axis
#   ggtheme = theme_minimal()    # Use a minimal theme
# )

# Extract and relabel strata
dt04_adj <- dt04 %>%
  mutate(strata = ifelse(strata != "No AI", "AI", strata),
         AI_strata = ifelse(record_id %in% (readxl::read_excel("only_AI_participants.xlsx"))$UIN...1, "Stimulated",
                            ifelse(!record_id %in% (readxl::read_excel("only_AI_participants.xlsx"))$UIN...1 & strata != "No AI", "non-Stimulated", "")
                            )
  )

# Fit KM curve
km_fit2 <- survfit(Surv(ttdeath, mortality) ~ strata, data = dt04_adj)

dt_km2 <- dt04_adj %>%
  filter(strata %in% c("No AI", "AI"))
# Log-rank test
surv_diff3 <- survdiff(Surv(ttdeath, mortality) ~ strata,
                       data = dt_km2)

# Extract p-value
p_value3 <- 1 - pchisq(surv_diff3$chisq, df = 1)

# Format nicely
p_label3 <- paste0("Log-rank p = ",
                   format.pval(p_value3, digits = 3, eps = .001))

# Generate initial ggsurvplot to access curve data
km_plot2 <- ggsurvplot(km_fit2, data = dt04_adj)

# Extract plotted data
plot_data <- km_plot2$plot$data

# Apply small horizontal jitter to one group (e.g., AI)
plot_data <- plot_data %>%
  mutate(time_jittered = ifelse(strata == "AI", time + 0.2, time))

# Plot manually using ggplot2
km_plot_manual <- ggplot(plot_data, aes(x = time_jittered, y = surv, color = strata)) +
  geom_step(size = 1.2) +
  geom_point(size = 1.5) +
  labs(x = "Time (months)", y = "Survival Probability", color = "Strata") +
  scale_color_manual(values = c("red", "darkgreen")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(0, 13), breaks = 0:13) +
  annotate("text", x = 9, y = 0.2, label = p_label3,  size = 6, hjust = 0) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    legend.position = "null"
    )

# ggsave("KM_curve_jittered.png", km_plot_manual, width = 8, height = 6, dpi = 300)
km_fit3 <- survfit(Surv(ttdeath, mortality) ~ AI_strata, 
                   data = dt04_adj %>%
                     filter(AI_strata %in% c("non-Stimulated", "Stimulated") )
)

# Generate initial ggsurvplot to access curve data
km_plot3 <- ggsurvplot(km_fit3, 
                       data = dt04_adj %>%
                         filter(AI_strata %in% c("non-Stimulated", "Stimulated"))
)

dt_km3 <- dt04_adj %>%
  filter(AI_strata %in% c("non-Stimulated", "Stimulated"))
# Log-rank test
surv_diff3 <- survdiff(Surv(ttdeath, mortality) ~ AI_strata,
                       data = dt_km3)

# Extract p-value
p_value3 <- 1 - pchisq(surv_diff3$chisq, df = 1)

# Format nicely
p_label3 <- paste0("Log-rank p = ",
                   format.pval(p_value3, digits = 3, eps = .001))
# Extract plotted data
plot_data3 <- km_plot3$plot$data

# Apply small horizontal jitter to one group (e.g., Stimulated)
plot_data3 <- plot_data3 %>%
  mutate(time_jittered = ifelse(AI_strata == "Stimulated", time + 0.2, time))

# Plot manually using ggplot2
km_plot_manual2 <- ggplot(plot_data3, aes(x = time_jittered, y = surv, color = AI_strata)) +
  geom_step(size = 1.2) +
  geom_point(size = 1.5) +
  labs(x = "Time (months)", y = "Survival Probability", color = "AI_strata") +
  scale_color_manual(values = c("red", "darkgreen")) +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, by = 0.25)) +
  scale_x_continuous(limits = c(0, 13), breaks = 0:13) +
  annotate("text", x = 9, y = 0.2, label = p_label3,  size = 6, hjust = 0) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null"),
    legend.position = "null")
combine_plot_240 <- cowplot::plot_grid(km_plot_manual, km_plot_manual2, ncol = 2)

final_stacked_plot <- cowplot::plot_grid(
  combine_plot_500,
  combine_plot_400,
  combine_plot_240,
  ncol = 1
)

ggsave("KM_stacked_3x2.png",
       final_stacked_plot,
       width = 17,
       height = 18,
       dpi = 300)
ggsave("KM_curve_jittered2.png", plot = combine_plot, width = 12, height = 6, dpi = 300)
