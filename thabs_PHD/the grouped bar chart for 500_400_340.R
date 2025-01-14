final_dataset <- read_excel("hypo_alldata_wide_labels - 554.xls") %>%
  mutate(addisons_disease = ifelse(as.numeric(`Random cortisol result`) < 340, 1, 0),
         # addisons_status = ifelse(as.numeric(`Random cortisol result`) < 200, "probable", ifelse(as.numeric(`Random cortisol result`) >=200 & as.numeric(`Random cortisol result`) < 340, 'Suspects')),
         # addisons_disease = ifelse(addisons_disease == 1 & as.numeric(``) < 340), 1, 0),
         PAI = ifelse(as.numeric(`Random cortisol result`) < 340 & as.numeric(`Synacthen: 30 minute cortisol result`) < 340 & (as.numeric(`ACTH result`) >= 64.7 & !is.na(`ACTH result`)), 1, 0),
         SAI = ifelse(as.numeric(`Random cortisol result`) < 340 & as.numeric(`Synacthen: 30 minute cortisol result`) < 340 & (as.numeric(`ACTH result`) < 64.7), 1, 0),
         total_AI = ifelse(as.numeric(`Random cortisol result`) < 340 & (PAI == 1 | SAI == 1), 1, 
                           ifelse(as.numeric(`Random cortisol result`) < 340 & (PAI != 1 & SAI != 1), 0, NA)),
         addisons_disease  = ifelse(is.na(total_AI), 0, total_AI),
         addisons_disease = ifelse(UIN...1 %in% c(392, 435, 478, 488, 496, 500), 1, addisons_disease)
  ) %>%
  mutate(AI = ifelse(PAI == 1, 'PAI', 
                     ifelse(SAI == 1, 'SAI', ''))) %>%
  mutate(AI = ifelse(UIN...1 %in% c(435, 478, 488, 496), 'SAI',
                     ifelse(UIN...1 %in% c(392, 500), 'PAI', AI)),
         `Adrenal insufficiency` = ifelse(!is.na(addisons_disease), addisons_disease, 0)#,
         # `Loss of axillary and pubic hair, if female` = ifelse(`Loss of axillary and pubic hair, if female` == 'Not applicable', '', `Loss of axillary and pubic hair, if female`)
  ) %>%
  mutate(`Increased pigmentation of the skin` = as.logical(ifelse(`Increased pigmentation of the skin` == 'Yes', 1, 0)),
         `Loss of consciousness` = as.logical(ifelse(`Loss of consciousness` == 'Yes', 1, 0)),
         `Hypoglycaemia` = as.logical(ifelse(`Hypoglycaemia` == 'Yes', 1, 0)),
         Dizziness = as.logical(ifelse(Dizziness == 'Yes', 1, 0)),
         Shock = as.logical(ifelse(Shock == 'Yes', 1, 0)),
         `Presence of anaemia` = as.logical(ifelse(`Presence of anaemia` == 'Yes', 1, 0)),
         Hypotension = as.logical(ifelse(Hypotension == 'Yes', 1, 0)),
         Weakness = as.logical(ifelse(Weakness == 'Yes', 1, 0)),
         Tiredness = as.logical(ifelse(Tiredness == 'Yes', 1, 0)),
         `Poor appetite` = as.logical(ifelse(`Poor appetite` == 'Yes', 1, 0)),
         `Weight loss` = as.logical(ifelse(`Weight loss` == 'Yes', 1, 0)),
         Nausea = as.logical(ifelse(Nausea == 'Yes', 1, 0)),
         Vomiting = as.logical(ifelse(Vomiting == 'Yes', 1, 0)),
         `Liking for salt` = as.logical(ifelse(`Liking for salt` == 'Yes', 1, 0)),
         Diarrhoea = as.logical(ifelse(Diarrhoea == 'Yes', 1, 0)),
         Anorexia = as.logical(ifelse(Anorexia == 'Yes', 1, 0)),
         `Loss of axillary and pubic hair, if female` = ifelse(`Loss of axillary and pubic hair, if female` == 'NA', 'No', `Loss of axillary and pubic hair, if female`),
         `Any postural drop in blood pressure` = as.logical(ifelse(`Any postural drop in blood pressure` == 'Yes', 1, 0)),
         incremental_cortisol = (`Synacthen: 30 minute cortisol result` - `Synacthen: 0 minute cortisol result`)
  ) %>%
  mutate(incremental_cortisol = ifelse(incremental_cortisol<0, NA, incremental_cortisol))

table(final_dataset$addisons_disease)

table(final_dataset$AI)

#500
# > table(final_dataset$addisons_disease)
# 
# 0   1 
# 521  33 
# > table(final_dataset$AI)
# 
# PAI SAI 
# 301   9  24 
# > 
#400
# > table(final_dataset$addisons_disease)
# 
# 0   1 
# 536  18 
# > 
#   > table(final_dataset$AI)
# 
# PAI SAI 
# 373   5  13
#340
# > table(final_dataset$addisons_disease)
# 
# 0   1 
# 542  12 
# > 
#   > table(final_dataset$AI)
# 
# PAI SAI 
# 402   3   9 

dt = bind_cols(`Cortisol (nmol/L)` = c('< 500', '< 500', '< 500', '< 500', '< 400', '< 400', '< 400', '< 400', '< 340', '< 340', '< 340', '< 340'),
               group = c(1, 1, 2, 2, 1, 1, 2, 2, 1, 1, 2, 2),
               strata = c('No AI', 'AI', 'PAI', 'SAI', 'No AI', 'AI', 'PAI', 'SAI', 'No AI', 'AI', 'PAI', 'SAI'),
               Frequency = c(521, 33, 9, 24, 536, 18, 5, 13, 542, 12, 3, 9)
) %>%
  mutate(`Cortisol (nmol/L)` = fct_relevel(as.factor(`Cortisol (nmol/L)`), c("< 500", '< 400')))
ggsave("plot1 - cortisol.jpeg", width = 4, height = 4)
ggplot(dt %>% filter(group == 1), aes(fill = strata, y = Frequency, x = `Cortisol (nmol/L)`)) + 
  geom_bar(position="dodge", stat="identity") +
  scale_y_continuous(name="Frequency", limits=c(0, 550), breaks=c(0, 100, 200, 300, 400, 500)) +
  theme(
    text = element_text(size = 20),
    plot.title = element_text(hjust = 0.5),
    axis.line = element_line(colour = "black"),
    axis.text = element_text(size = 18),
    axis.title = element_text(size = 18),
    panel.background = element_blank(),
    panel.border = element_blank(),
    plot.margin = unit(c(0, 0, 0, 0), "null")#,
    # axis.text.x = element_text(angle = 60, hjust = 1)
  )
dev.off()

dt <- table_3 %>% 
  mutate(strata = ifelse(is.na(AI), 'No AI', AI)) %>% 
  mutate(strata = ifelse(strata == "", 'No AI', strata))

# Fit the Kaplan-Meier survival model
km_fit <- survfit(Surv(time = ttdeath, event = mortality) ~ strata, data = dt)

# Plot the Kaplan-Meier curve
km_plot1 <- ggsurvplot(
  km_fit,
  data = dt,
  # risk.table = TRUE,           # Add a risk table below the plot
  pval = TRUE,                 # Show p-value for log-rank test
  # conf.int = TRUE,             # Add confidence intervals
  legend.title = "Strata",     # Customize legend title
  legend.labs = c("No AI", "PAI", "SAI"),  # Customize group labels
  xlab = "Time",               # Label for x-axis
  ylab = "Survival Probability", # Label for y-axis
  ggtheme = theme_minimal()    # Use a minimal theme
)
ggsave("Kaplan_Meier_Curve1.png", plot = km_plot1$plot, width = 8, height = 6, dpi = 300)

km_fit <- 

# Plot the Kaplan-Meier curve
km_plot2 <- ggsurvplot(
  survfit(Surv(time = ttdeath, event = mortality) ~ strata, data = dt %>%
            mutate(strata = ifelse(strata != "No AI", 'AI', strata ))),
  data = dt,
  # risk.table = TRUE,           # Add a risk table below the plot
  pval = TRUE,                 # Show p-value for log-rank test
  # conf.int = TRUE,             # Add confidence intervals
  legend.title = "Strata",     # Customize legend title
  legend.labs = c("AI", "No AI"),  # Customize group labels
  xlab = "Time",               # Label for x-axis
  ylab = "Survival Probability", # Label for y-axis
  ggtheme = theme_minimal()    # Use a minimal theme
)
ggsave("Kaplan_Meier_Curve2.png", plot = km_plot2$plot, width = 8, height = 6, dpi = 300)
