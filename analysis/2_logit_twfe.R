
# Basline logit (Zeltzer) ------------------------------------------------------------------------------------

logit1 <- feglm(
  referral ~ same_sex + male_spec + exp_spec | year + doctor,
  data = df_logit_cond,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit2 <- feglm(
  referral ~ same_sex + male_spec + exp_spec + same_prac + same_zip | year + doctor,
  data = df_logit_cond,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit3 <- feglm(
  referral ~ same_sex + male_spec + exp_spec + same_prac + same_zip + diff_age + diff_gradyear | year + doctor,
  data = df_logit_cond,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit4 <- feglm(
  referral ~ same_sex + male_spec + exp_spec + same_prac + same_zip + diff_age + diff_gradyear | year + doctor,
  data = df_logit_cond %>% filter(!is.na(same_school)),
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit5 <- feglm(
  referral ~ same_sex + male_spec + exp_spec + same_prac + same_zip + diff_age + diff_gradyear + same_school | year + doctor,
  data = df_logit_cond %>% filter(!is.na(same_school)),
  vcov = "HC1",
  family = binomial(link = "logit")
)


mfx_logit1 <- avg_slopes(logit1)
mfx_logit2 <- avg_slopes(logit2)
mfx_logit3 <- avg_slopes(logit3)
mfx_logit4 <- avg_slopes(logit4)
mfx_logit5 <- avg_slopes(logit5)





logit_race1 <- feglm(
  referral ~ same_sex + same_race + male_spec + exp_spec | year + doctor,
  data = df_logit_cond,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_race2 <- feglm(
  referral ~ same_sex + same_race + male_spec + exp_spec + same_prac + same_zip | year + doctor,
  data = df_logit_cond,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_race3 <- feglm(
  referral ~ same_sex + same_race + male_spec + exp_spec + same_prac + same_zip + diff_age + diff_gradyear | year + doctor,
  data = df_logit_cond,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_race4 <- feglm(
  referral ~ same_sex + same_race + male_spec + exp_spec + same_prac + same_zip + diff_age + diff_gradyear | year + doctor,
  data = df_logit_cond %>% filter(!is.na(same_school)),
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_race5 <- feglm(
  referral ~ same_sex + same_race + male_spec + exp_spec + same_prac + same_zip + diff_age + diff_gradyear + same_school | year + doctor,
  data = df_logit_cond %>% filter(!is.na(same_school)),
  vcov = "HC1",
  family = binomial(link = "logit")
)


mfx_logit_race1 <- avg_slopes(logit_race1)
mfx_logit_race2 <- avg_slopes(logit_race2)
mfx_logit_race3 <- avg_slopes(logit_race3)
mfx_logit_race4 <- avg_slopes(logit_race4)
mfx_logit_race5 <- avg_slopes(logit_race5)


# TWFE Logit -------------------------------------------------------------------------------------------------

logit_twfe1 <- feglm(
  referral ~ same_sex + same_prac + same_zip + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_twfe2 <- feglm(
  referral ~ same_sex + same_prac + same_zip + same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_twfe3 <- feglm(
  referral ~ same_sex + same_prac + same_zip + same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe %>% filter(!is.na(same_school)),
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_twfe4 <- feglm(
  referral ~ same_sex + same_prac + same_zip + same_race + diff_age + diff_gradyear + same_school | year,
  data = df_logit_twfe %>% filter(!is.na(same_school)),
  vcov = "HC1",
  family = binomial(link = "logit")
)


mfx_logit_twfe1 <- avg_slopes(logit_twfe1)
mfx_logit_twfe2 <- avg_slopes(logit_twfe2)
mfx_logit_twfe3 <- avg_slopes(logit_twfe3)
mfx_logit_twfe4 <- avg_slopes(logit_twfe4)


# Summary of Conditional Logit Models -------------------------------------------------------------------------------

## Baseline

# Organize models into a list
models_logit <- list(
  "(1)" = mfx_logit1,
  "(2)" = mfx_logit2,
  "(3)" = mfx_logit3,
  "(4)" = mfx_logit4,
  "(5)" = mfx_logit5
)

# Custom coefficient labels to match your table rows
coef_labels <- c(
  "same_sex" = "Same gender",
  "male_spec" = "Male specialist",   
  "exp_spec" = "Specialist experience (10 years)",
  "same_prac" = "Same practice group",  
  "same_zip" = "Same zip code",  
  "diff_age" = "Similar age",
  "diff_gradyear" = "Similar experience",
  "same_school" = "Same medical school"
)

# Additional rows for FE indicators and summary stats
add_rows <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,~`(4)`, ~`(5)`,
  "Year FE", "Yes", "Yes", "Yes","Yes", "Yes",
  "Doctor FE", "Yes", "Yes","Yes","Yes", "Yes",
  "Specialist FE", "No", "No", "No", "No", "No",
  "Observations", format(nobs(logit1), big.mark=","), 
                  format(nobs(logit2), big.mark=","), 
                  format(nobs(logit3), big.mark=","), 
                  format(nobs(logit4), big.mark=","), 
                  format(nobs(logit5), big.mark=","),
  "Pseudo-$R^2$", format(logit1$pseudo_r2, digits = 2),
                  format(logit2$pseudo_r2, digits = 2),
                  format(logit3$pseudo_r2, digits = 2),
                  format(logit4$pseudo_r2, digits = 2),
                  format(logit5$pseudo_r2, digits = 2)
)


# Create the LaTeX table
options(modelsummary_format_numeric_latex = "plain")
summary_logit <- modelsummary(
  models_logit,
  coef_map = coef_labels,
  stars = FALSE,
  gof_omit = ".*",
  add_rows = add_rows,
  output = "latex_tabular"
)

writeLines(as.character(summary_logit), "results/app_logit_mfx.tex")



## With same race variale

# Organize models into a list
models_logit_race <- list(
  "(1)" = mfx_logit_race1,
  "(2)" = mfx_logit_race2,
  "(3)" = mfx_logit_race3,
  "(4)" = mfx_logit_race4,
  "(5)" = mfx_logit_race5
)

# Custom coefficient labels to match your table rows
coef_labels <- c(
  "same_sex" = "Same gender",
  "male_spec" = "Male specialist",   
  "exp_spec" = "Specialist experience (10 years)",
  "same_prac" = "Same practice group",
  "same_race" = "Same race",
  "same_zip" = "Same zip code",  
  "diff_age" = "Similar age",
  "diff_gradyear" = "Similar experience",
  "same_school" = "Same medical school"
)

# Additional rows for FE indicators and summary stats
add_rows <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,~`(4)`, ~`(5)`,
  "Year FE", "Yes", "Yes", "Yes","Yes", "Yes",
  "Doctor FE", "Yes", "Yes","Yes","Yes", "Yes",
  "Specialist FE", "No", "No", "No", "No", "No",
  "Observations", format(nobs(logit_race1), big.mark=","), 
                  format(nobs(logit_race2), big.mark=","), 
                  format(nobs(logit_race3), big.mark=","), 
                  format(nobs(logit_race4), big.mark=","), 
                  format(nobs(logit_race5), big.mark=","),
  "Pseudo-$R^2$", format(logit_race1$pseudo_r2, digits = 2),
                  format(logit_race2$pseudo_r2, digits = 2),
                  format(logit_race3$pseudo_r2, digits = 2),
                  format(logit_race4$pseudo_r2, digits = 2),
                  format(logit_race5$pseudo_r2, digits = 2)
)


# Create the LaTeX table
options(modelsummary_format_numeric_latex = "plain")
summary_logit_race <- modelsummary(
  models_logit_race,
  coef_map = coef_labels,
  stars = FALSE,
  gof_omit = ".*",
  add_rows = add_rows,
  output = "latex_tabular"
)

writeLines(as.character(summary_logit_race), "results/app_logit_race_mfx.tex")




# Summary of TWFE Logit Models --------------------------------------------------------------------------------

# Organize models into a list
models_twfe <- list(
  "(1)" = mfx_logit_twfe1,
  "(2)" = mfx_logit_twfe2,
  "(3)" = mfx_logit_twfe3,
  "(4)" = mfx_logit_twfe4
)

# Custom coefficient labels to match your table rows
coef_labels <- c(
  "same_sex" = "Same gender",
  "same_prac" = "Same practice group",
  "same_race" = "Same race",
  "same_zip" = "Same zip code",  
  "diff_age" = "Similar age",
  "diff_gradyear" = "Similar experience",
  "same_school" = "Same medical school"
)

# Additional rows for FE indicators and summary stats
add_rows <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,~`(4)`, 
  "Year FE", "Yes", "Yes", "Yes","Yes", 
  "Doctor FE", "Yes", "Yes","Yes","Yes", 
  "Specialist FE", "Yes", "Yes", "Yes", "Yes",
  "Observations", format(nobs(logit_twfe1), big.mark=","), 
                  format(nobs(logit_twfe2), big.mark=","), 
                  format(nobs(logit_twfe3), big.mark=","),
                  format(nobs(logit_twfe4), big.mark=","),
  "Pseudo-$R^2$", format(logit_twfe1$pseudo_r2, digits = 2),
                  format(logit_twfe2$pseudo_r2, digits = 2),
                  format(logit_twfe3$pseudo_r2, digits = 2),
                  format(logit_twfe4$pseudo_r2, digits = 2)
)


# Create the LaTeX table
options(modelsummary_format_numeric_latex = "plain")
summary_twfe <- modelsummary(
  models_twfe,
  coef_map = coef_labels,
  stars = FALSE,
  gof_omit = ".*",
  add_rows = add_rows,
  output = "latex_tabular"
)

writeLines(as.character(summary_twfe), "results/logit_twfe_mfx.tex")

