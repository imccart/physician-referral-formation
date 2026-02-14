# Convergence stability of two-stage estimation --------------------------------
# Run AFTER source("analysis/_main.R") so df_logit and df_logit_twfe are in memory
#
# The FE-recovery step (Stage 2) does not formally converge because ~75% of
# doctor-specialist pairs are separated (all-zero referrals in a sparse network).
# Separated FEs diverge to ±∞, but non-separated obs stabilize quickly.
# This script verifies that MFX are invariant to iteration count beyond 50.

covars <- c("same_sex", "same_prac", "diff_dist",
            "same_race", "diff_age", "diff_gradyear")

## Stage 1: Jochmans β (identical for all runs) ----
joch_mod <- fixest::feglm(
  referral ~ same_sex + same_prac + diff_dist +
             same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = ~hrr,
  family = binomial("logit")
)
beta_joch <- coef(joch_mod)[covars]

## Prep offset data ----
Xmat <- as.matrix(df_logit[, covars])
na_mask <- complete.cases(Xmat)
Xbeta <- drop(Xmat[na_mask, ] %*% beta_joch)
dat_fe_test <- df_logit[na_mask, ] %>% mutate(.Xbeta = Xbeta)

## Helper: FE recovery + MFX at a given iteration count ----
run_at_iter <- function(max_iter) {
  fe_mod <- fixest::feglm(
    referral ~ 1 | doctor + specialist,
    data = dat_fe_test,
    offset = ~.Xbeta,
    family = binomial("logit"),
    glm.iter = max_iter
  )

  fes <- fixef(fe_mod)
  doc_fe  <- fes$doctor[as.character(dat_fe_test$doctor)]
  spec_fe <- fes$specialist[as.character(dat_fe_test$specialist)]
  eta     <- dat_fe_test$.Xbeta + doc_fe + spec_fe
  valid   <- !is.na(eta)

  dat_valid <- dat_fe_test[valid, ]
  eta_valid <- eta[valid]

  mfx <- map_dfr(covars, function(v) {
    is_bin <- v %in% c("same_sex", "same_prac", "same_race")
    delta  <- if (v == "diff_dist") 5 else 1
    if (is_bin) {
      eta1 <- eta_valid - beta_joch[v] * dat_valid[[v]] + beta_joch[v]
      eta0 <- eta_valid - beta_joch[v] * dat_valid[[v]]
    } else {
      eta1 <- eta_valid + beta_joch[v] * delta
      eta0 <- eta_valid
    }
    tibble(variable = v, mfx = mean(plogis(eta1) - plogis(eta0)))
  })

  list(iter = max_iter, deviance = deviance(fe_mod), n_valid = sum(valid),
       fe_doc_sd = sd(fes$doctor), fe_spec_sd = sd(fes$specialist),
       mean_phat = mean(plogis(eta_valid)), mfx = mfx)
}

## Run at multiple iteration counts ----
iter_counts <- c(10, 25, 50, 100, 250)
message("Convergence stability test: iter = ", paste(iter_counts, collapse = ", "))

results <- list()
for (it in iter_counts) {
  message("  iter = ", it, " ...")
  results[[as.character(it)]] <- run_at_iter(it)
}

## Build appendix table ----

nice_lab <- c(
  same_sex      = "Same gender",
  same_prac     = "Same practice group",
  diff_dist     = "Distance (+5 mi)",
  same_race     = "Same race",
  diff_age      = "Age difference",
  diff_gradyear = "Experience difference"
)

# MFX panel: rows = covariates, columns = iteration counts
mfx_table <- map_dfr(results, function(r) {
  r$mfx %>% mutate(iter = r$iter)
}) %>%
  mutate(variable = nice_lab[variable]) %>%
  pivot_wider(names_from = iter, values_from = mfx,
              names_prefix = "iter_")

# Diagnostics panel: deviance, FE SD, mean p-hat
diag_table <- map_dfr(results, function(r) {
  tibble(iter = r$iter, deviance = r$deviance,
         doc_fe_sd = r$fe_doc_sd, spec_fe_sd = r$fe_spec_sd,
         mean_phat = r$mean_phat)
})

diag_rows <- tribble(
  ~variable, ~iter_10, ~iter_25, ~iter_50, ~iter_100, ~iter_250,
  "Deviance",
    diag_table$deviance[1], diag_table$deviance[2], diag_table$deviance[3],
    diag_table$deviance[4], diag_table$deviance[5],
  "Doctor FE (SD)",
    diag_table$doc_fe_sd[1], diag_table$doc_fe_sd[2], diag_table$doc_fe_sd[3],
    diag_table$doc_fe_sd[4], diag_table$doc_fe_sd[5],
  "Specialist FE (SD)",
    diag_table$spec_fe_sd[1], diag_table$spec_fe_sd[2], diag_table$spec_fe_sd[3],
    diag_table$spec_fe_sd[4], diag_table$spec_fe_sd[5],
  "Mean $\\hat{p}$",
    diag_table$mean_phat[1], diag_table$mean_phat[2], diag_table$mean_phat[3],
    diag_table$mean_phat[4], diag_table$mean_phat[5]
)

# Combine and format
full_table <- bind_rows(
  mfx_table %>% mutate(across(-variable, ~ formatC(.x, digits = 4, format = "f"))),
  tibble(variable = "", iter_10 = "", iter_25 = "", iter_50 = "",
         iter_100 = "", iter_250 = ""),
  diag_rows %>% mutate(across(-variable, ~ formatC(.x, digits = 2, format = "f")))
)

kable(full_table,
      format    = "latex",
      booktabs  = TRUE,
      align     = c("l", rep("r", 5)),
      col.names = c("", "10", "25", "50", "100", "250"),
      escape    = FALSE) %>%
  add_header_above(c(" " = 1, "Maximum iterations" = 5)) %>%
  pack_rows("Panel A: Average marginal effects", 1, nrow(mfx_table)) %>%
  pack_rows("Panel B: Diagnostics", nrow(mfx_table) + 2, nrow(full_table)) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("results/tables/app_convergence.tex")

message("Saved: results/tables/app_convergence.tex")
