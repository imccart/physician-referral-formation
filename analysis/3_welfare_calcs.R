# Summarize results in terms of welfare costs -------------------------------------

## extract coefficients from preferred logit model
covars   <- c("same_sex", "same_prac", "diff_dist",
              "same_race", "diff_age", "diff_gradyear",
              "same_school")

beta_hat <- coef(logit_twfe4)[covars]         

quartets <- df_logit_twfe %>%
  rename(q1=spec1_qual, q2=spec2_qual)


## predict choice probabilities with existing coefficient estimates
Xmat     <- as.matrix(select(quartets, all_of(covars)))   # n × 7
linpred  <- drop(Xmat %*% beta_hat)
p_choose <- plogis(linpred)                               # P(referral to spec1)

quartets <- quartets %>%
  mutate(p       = p_choose,
         q_best  = pmax(q1, q2),
         q_exp   = p * q1 + (1 - p) * q2,
         shortfall = q_best - q_exp)

overall_EQS <- mean(quartets$shortfall, na.rm = TRUE)

## counterfactual: set same practice to 0
beta_cf <- beta_hat
beta_cf["same_prac"] <- 0

linpred_cf  <- drop(Xmat %*% beta_cf)
p_choose_cf <- plogis(linpred_cf)

quartets <- quartets %>%
  mutate(p_cf      = p_choose_cf,
         q_exp_cf  = p_cf * q1 + (1 - p_cf) * q2,
         shortfall_cf = q_best - q_exp_cf)

EQS_nopractice <- mean(quartets$shortfall_cf, na.rm = TRUE)

## summarize results
cat("Mean quality shortfall (baseline):       ",
    comma(overall_EQS,  accuracy = .0001), "\n",
    "Mean quality shortfall (same practice term set to 0): ",
    comma(EQS_nopractice,    accuracy = .0001), "\n",
    "Quality cost attributable to intra-practice referrals: ",
    comma(overall_EQS - EQS_nopractice, accuracy = .0001), "\n")
