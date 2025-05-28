library(tidyverse)
library(fixest)
library(scales)      # optional: for pretty printing

## ------------------------------------------------------------
## 1.  extract β from the fitted feglm model
## ------------------------------------------------------------
covars   <- c("same_sex", "same_prac", "same_zip",
              "same_race", "diff_age", "diff_gradyear",
              "same_school")

beta_hat <- coef(logit_twfe4)[covars]            # numeric vector (k = 7)

## ------------------------------------------------------------
## 2.  attach surgeon quality to each quartet
## ------------------------------------------------------------
quartets <- df_logit_twfe %>%
  left_join(surgeon_quality, by = c("spec1" = "specialist")) %>%
  rename(q1 = q) %>%
  left_join(surgeon_quality, by = c("spec2" = "specialist")) %>%
  rename(q2 = q)

## ------------------------------------------------------------
## 3.  predict choice probabilities with existing β
## ------------------------------------------------------------
Xmat     <- as.matrix(select(quartets, all_of(covars)))   # n × 7
linpred  <- drop(Xmat %*% beta_hat)
p_choose <- plogis(linpred)                               # P(referral to spec1)

quartets <- quartets %>%
  mutate(p       = p_choose,
         q_best  = pmax(q1, q2),
         q_exp   = p * q1 + (1 - p) * q2,
         shortfall = q_best - q_exp)

overall_EQS <- mean(quartets$shortfall, na.rm = TRUE)

## ------------------------------------------------------------
## 4.  counterfactual: set the proximity term (same_zip) to zero
## ------------------------------------------------------------
beta_cf <- beta_hat
beta_cf["same_zip"] <- 0

linpred_cf  <- drop(Xmat %*% beta_cf)
p_choose_cf <- plogis(linpred_cf)

quartets <- quartets %>%
  mutate(p_cf      = p_choose_cf,
         q_exp_cf  = p_cf * q1 + (1 - p_cf) * q2,
         shortfall_cf = q_best - q_exp_cf)

EQS_noZIP <- mean(quartets$shortfall_cf, na.rm = TRUE)

## ------------------------------------------------------------
## 5.  display results
## ------------------------------------------------------------
cat("Mean quality shortfall (baseline):       ",
    comma(overall_EQS,  accuracy = .0001), "\n",
    "Mean quality shortfall (ZIP term set to 0): ",
    comma(EQS_noZIP,    accuracy = .0001), "\n",
    "Quality cost attributable to proximity: ",
    comma(overall_EQS - EQS_noZIP, accuracy = .0001), "\n")
