# ------------------------------------------------------------------------------
# Build actual and counterfactual referral-network weights for VRDC upload.
#
# For each PCP-specialist pair we compute the model-predicted link probability
# (a) as estimated (w_actual) and (b) with the organizational tie switched off
# (w_cf_noorg), i.e. the same-practice coefficient zeroed. These are the weights
# the VRDC uses to form the actual vs. counterfactual network and reweight
# patient outcomes. Reuses the two-stage Jochmans estimates and the Stage-2
# fixed-effect recovery (same machinery as 5_welfare.R), across all three
# specialties.
#
# Output: data/output/vrdc_referral_network_weights.csv
#   pcp_npi, specialist_npi, hrr, specialty, w_actual, w_cf_noorg
#
# NOTE on keys: `doctor` in the pipeline is the PCP's individual NPI (the column
# is renamed from `Practice_ID` in 1_referrals_full.R, which joins to the MDPPAS
# physician `npi`); `specialist` is the specialist NPI. Both are the join keys
# the VRDC will use.
# ------------------------------------------------------------------------------

suppressMessages({library(tidyverse); library(fixest)})

specs   <- c("ortho", "cardioem", "derm")
covars  <- c("same_sex", "same_prac", "diff_dist", "same_race", "diff_age", "diff_gradyear")
org_var <- "same_prac"   # organizational tie switched off in the counterfactual

one_spec <- function(sp) {
  message("  ", sp, ": recovering FEs...")

  est <- read_csv(sprintf("results/tables/estimates_%s.csv", sp), show_col_types = FALSE) %>%
    filter(spec == 4)
  beta_hat <- setNames(est$beta, est$term)[covars]

  df <- read_csv(sprintf("data/output/df_logit_movers_%s.csv", sp), show_col_types = FALSE) %>%
    mutate(doctor = as.factor(doctor), specialist = as.factor(specialist))

  na_mask <- complete.cases(as.matrix(df[, covars]))
  dat     <- df[na_mask, ]
  Xbeta   <- drop(as.matrix(dat[, covars]) %*% beta_hat)
  dat$.Xbeta <- Xbeta

  fe_mod <- feglm(referral ~ 1 | doctor + specialist, data = dat,
                  offset = ~.Xbeta, family = binomial("logit"), glm.iter = 200)
  fes <- fixef(fe_mod)
  eta <- Xbeta + fes$doctor[as.character(dat$doctor)] + fes$specialist[as.character(dat$specialist)]
  valid <- !is.na(eta)

  eta_cf <- eta - beta_hat[[org_var]] * dat[[org_var]]

  tibble(
    pcp_npi        = as.character(dat$doctor)[valid],
    specialist_npi = as.character(dat$specialist)[valid],
    hrr            = dat$doc_hrr[valid],
    specialty      = sp,
    w_actual       = plogis(eta[valid]),
    w_cf_noorg     = plogis(eta_cf[valid])
  )
}

out <- map_dfr(specs, one_spec)
write_csv(out, "data/output/vrdc_referral_network_weights.csv")
message("Wrote data/output/vrdc_referral_network_weights.csv: ", nrow(out), " rows across ",
        n_distinct(out$specialty), " specialties, ",
        n_distinct(out$pcp_npi), " PCPs, ", n_distinct(out$specialist_npi), " specialists")
