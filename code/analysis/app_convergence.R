# Convergence stability of the two-stage recovery ---------------------------
# Run AFTER source("analysis/2_logit_twfe.R") so df_logit and df_logit_twfe are
# in memory.
#
# The specialist effects are estimated on the full sample and held fixed
# (specialist_fe.R), so the second stage recovers only the doctor effects and
# converges cleanly. This script verifies that the referral-probability-weighted
# marginal effects are invariant to the iteration cap.

covars <- c("same_sex", "same_prac", "diff_dist",
            "same_race", "diff_age", "diff_gradyear")

## Stage 1: Jochmans β (identical for all runs) ----
joch_mod <- feglm(
  referral ~ same_sex + same_prac + diff_dist +
             same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = ~hrr,
  family = binomial("logit")
)
beta_joch <- coef(joch_mod)[covars]

## Prep offset data: specialist effect held fixed, Jochmans index as offset ----
spec_fe_tab <- read_csv(sprintf("data/output/specialist_fe_%s.csv", current_specialty),
                        show_col_types = FALSE) %>%
  transmute(spec_key = as.character(specialist), gamma_j)
Xmat <- as.matrix(df_logit[, covars])
na_mask <- complete.cases(Xmat)
dat_fe_test <- df_logit[na_mask, ] %>%
  mutate(spec_key = as.character(specialist)) %>%
  left_join(spec_fe_tab, by = "spec_key")
message("Specialist FE match: ",
        sprintf("%.1f%%", 100 * mean(!is.na(dat_fe_test$gamma_j))))
dat_fe_test <- dat_fe_test %>%
  mutate(gamma_spec = coalesce(gamma_j, 0),
         .Xbeta = drop(as.matrix(dat_fe_test[, covars]) %*% beta_joch) + gamma_spec)

## Helper: doctor-effect recovery + weighted MFX at a given iteration count ----
run_at_iter <- function(max_iter) {
  fe_mod <- feglm(
    referral ~ 1 | doctor,
    data = dat_fe_test,
    offset = ~.Xbeta,
    family = binomial("logit"),
    glm.iter = max_iter
  )

  fes     <- fixef(fe_mod)
  doc_fe  <- fes$doctor[as.character(dat_fe_test$doctor)]
  eta     <- dat_fe_test$.Xbeta + doc_fe
  valid   <- !is.na(eta)

  dat_valid <- dat_fe_test[valid, ]
  eta_valid <- eta[valid]
  w_base    <- plogis(eta_valid - beta_joch[["same_prac"]] * dat_valid[["same_prac"]])
  w_base    <- w_base / sum(w_base)

  mfx <- map(covars, function(v) {
    is_bin <- v %in% c("same_sex", "same_prac", "same_race")
    delta  <- if (v == "diff_dist") 5 else 1
    if (is_bin) {
      eta1 <- eta_valid - beta_joch[v] * dat_valid[[v]] + beta_joch[v]
      eta0 <- eta_valid - beta_joch[v] * dat_valid[[v]]
    } else {
      eta1 <- eta_valid + beta_joch[v] * delta
      eta0 <- eta_valid
    }
    tibble(variable = v, mfx = sum(w_base * (plogis(eta1) - plogis(eta0))))
  }) %>% bind_rows()

  list(iter = max_iter, deviance = deviance(fe_mod), n_valid = sum(valid),
       fe_doc_sd = sd(fes$doctor), mean_phat = mean(plogis(eta_valid)), mfx = mfx)
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
mfx_table <- map(results, function(r) {
  r$mfx %>% mutate(iter = r$iter)
}) %>% bind_rows() %>%
  mutate(variable = nice_lab[variable]) %>%
  pivot_wider(names_from = iter, values_from = mfx,
              names_prefix = "iter_")

# Diagnostics panel: deviance, doctor FE SD, mean p-hat
diag_table <- map(results, function(r) {
  tibble(iter = r$iter, deviance = r$deviance,
         doc_fe_sd = r$fe_doc_sd, mean_phat = r$mean_phat)
}) %>% bind_rows()

diag_rows <- tribble(
  ~variable, ~iter_10, ~iter_25, ~iter_50, ~iter_100, ~iter_250,
  "Deviance",
    diag_table$deviance[1], diag_table$deviance[2], diag_table$deviance[3],
    diag_table$deviance[4], diag_table$deviance[5],
  "Doctor FE (SD)",
    diag_table$doc_fe_sd[1], diag_table$doc_fe_sd[2], diag_table$doc_fe_sd[3],
    diag_table$doc_fe_sd[4], diag_table$doc_fe_sd[5],
  "Mean $\\hat{p}$",
    diag_table$mean_phat[1], diag_table$mean_phat[2], diag_table$mean_phat[3],
    diag_table$mean_phat[4], diag_table$mean_phat[5]
)

# Helper: format large numbers in scientific notation for LaTeX
fmt_diag <- function(x) {
  sapply(x, function(val) {
    if (is.na(val)) return("")
    if (abs(val) >= 1e6) {
      exp_val <- floor(log10(abs(val)))
      mantissa <- val / 10^exp_val
      paste0("$", formatC(mantissa, digits = 2, format = "f"),
             " \\times 10^{", exp_val, "}$")
    } else {
      formatC(val, digits = 2, format = "f")
    }
  })
}

# Combine and format
full_table <- bind_rows(
  mfx_table %>% mutate(across(-variable,
                               ~ formatC(.x, digits = 4, format = "f"))),
  tibble(variable = "", iter_10 = "", iter_25 = "", iter_50 = "",
         iter_100 = "", iter_250 = ""),
  diag_rows %>% mutate(across(-variable, fmt_diag))
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
  save_kable(sprintf("results/tables/app_convergence_%s.tex", current_specialty))

message(sprintf("Saved: results/tables/app_convergence_%s.tex", current_specialty))
