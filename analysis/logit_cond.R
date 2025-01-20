# Load required libraries
library(dplyr)
library(readr)
library(glmnet)
library(margins)
library(survival)

# Set working directory
setwd("/Users/pablo/Library/CloudStorage/Dropbox/1_Research/1_Ongoing/Referral Drivers/Code/notebooks")

# Load the movers_referrals.csv data
df_ref_all <- read_csv("temp_data/movers_referrals.csv")

# Generate variables for male specialist and male doctor
df_ref_all <- df_ref_all %>%
  mutate(
    male_spec = ifelse(sex_spec == "M", 1, 0),
    male_doc = ifelse(sex_doc == "M", 1, 0),
    exp_spec = year - grad_year_spec
  )

# Logistic regression (logit)
logit_model <- glm(
  referral ~ same_sex + male_doc + male_spec + same_prac + exp_spec + same_zip + factor(year),
  data = df_ref_all,
  family = binomial(link = "logit")
)
summary(logit_model)

# Marginal effects for logit
marginal_effects_logit <- margins(logit_model)
print(marginal_effects_logit)

# Conditional logistic regression (clogit)
clogit_model <- clogit(
  referral ~ same_sex + male_spec + same_prac + exp_spec + same_zip + factor(year) + strata(doctor),
  data = df_ref_all
)
summary(clogit_model)

# Alternative clogit model (commented in Stata)
# clogit_model_alt <- clogit(
#   referral ~ male_doc * male_spec + same_prac + exp_spec + same_zip + factor(year) + strata(doctor),
#   data = df_ref_all
# )
# summary(clogit_model_alt)

# Marginal effects for clogit
# (Note: glmnet and survival models do not directly provide margins like Stata; simulate manually if required)

# Load the df_logit.csv data
df_logit <- read_csv("temp_data/df_logit.csv")

# Logistic regression on df_logit
logit_model_df_logit <- glm(
  referral ~ same_sex + same_school + same_prac + diff_age + diff_gradyear + same_zip + factor(year),
  data = df_logit,
  family = binomial(link = "logit")
)
summary(logit_model_df_logit)

# Alternative logistic regression (commented in Stata)
# logit_model_alt_df_logit <- glm(
#   referral ~ same_male + same_female + same_school + same_prac + diff_age + diff_gradyear + same_zip + factor(year),
#   data = df_logit,
#   family = binomial(link = "logit")
# )
# summary(logit_model_alt_df_logit)

# Marginal effects for df_logit
marginal_effects_df_logit <- margins(logit_model_df_logit)
print(marginal_effects_df_logit)
