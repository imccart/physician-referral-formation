## Appendix: Robustness — dropping same_prac (Referee Comment 1b)
## Re-estimate the full Jochmans spec without same_prac, then two-stage MFX.
## Output: two-column table (structural β + MFX) for the no-practice model.

message("  Robustness: dropping same_prac...")

# Stage 1: Jochmans without same_prac
covars_noprac <- c("same_sex", "diff_dist", "same_race", "diff_age", "diff_gradyear")

logit_noprac <- fixest::feglm(
  referral ~ same_sex + diff_dist + same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = ~hrr,
  family = binomial("logit")
)

# Stage 2: FE recovery (reuse dat_fe from 2_logit_twfe.R)
recover_fes_noprac <- function(joch_model, spec_covars) {
  beta <- coef(joch_model)[spec_covars]
  Xbeta <- drop(as.matrix(dat_fe[, spec_covars]) %*% beta)
  dat_tmp <- dat_fe %>% mutate(.Xbeta = Xbeta)

  fe_mod <- fixest::feglm(
    referral ~ 1 | doctor + specialist,
    data = dat_tmp,
    offset = ~.Xbeta,
    family = binomial("logit"),
    glm.iter = 200
  )

  fes <- fixef(fe_mod)
  doc_fe  <- fes$doctor[as.character(dat_fe$doctor)]
  spec_fe <- fes$specialist[as.character(dat_fe$specialist)]
  eta     <- Xbeta + doc_fe + spec_fe
  valid   <- !is.na(eta)

  list(beta = beta, eta = eta, valid = valid, n_valid = sum(valid),
       fe_mod = fe_mod, fes = fes)
}

stage2_noprac <- recover_fes_noprac(logit_noprac, covars_noprac)

# MFX (reuse compute_mfx from 2_logit_twfe.R — it's still in scope)
mfx_noprac <- compute_mfx(stage2_noprac, logit_noprac, covars_noprac)

message("  No-practice robustness: ", stage2_noprac$n_valid, " valid obs")

# Build two-column table: structural β + MFX
vars_noprac <- c("same_sex", "diff_dist", "same_race", "diff_age", "diff_gradyear")
labels_noprac <- c("Same gender", "Distance", "Same race",
                   "Age difference", "Experience difference")

beta_vals <- coef(logit_noprac)[vars_noprac]
beta_ses  <- sqrt(diag(vcov(logit_noprac)))[vars_noprac]

tbl_rows_noprac <- map(seq_along(vars_noprac), function(i) {
  v <- vars_noprac[i]
  mfx_row <- mfx_noprac %>% filter(term == v)
  bind_rows(
    tibble(term = labels_noprac[i],
           `Structural` = fmt_est(beta_vals[i]),
           `MFX` = fmt_est(mfx_row$estimate)),
    tibble(term = "",
           `Structural` = fmt_se(beta_ses[i]),
           `MFX` = fmt_se(mfx_row$std.error))
  )
}) %>% bind_rows()

footer_noprac <- tribble(
  ~term, ~Structural, ~MFX,
  "Year FE", "Yes", "Yes",
  "Doctor FE", "Yes", "Yes",
  "Specialist FE", "Yes", "Yes",
  "Quartet obs", format(nobs(logit_noprac), big.mark = ","), "",
  "Choice-set obs", "", format(stage2_noprac$n_valid, big.mark = ",")
)

tbl_noprac <- bind_rows(tbl_rows_noprac, footer_noprac)

kable(tbl_noprac,
      format = "latex", booktabs = TRUE, linesep = "",
      align = c("l", "r", "r"),
      col.names = c("", "Structural $\\beta$", "Avg. MFX"),
      escape = FALSE) %>%
  row_spec(nrow(tbl_rows_noprac), extra_latex_after = "\\midrule") %>%
  row_spec(nrow(tbl_rows_noprac) + 3, extra_latex_after = "\\midrule") %>%
  save_kable(sprintf("results/tables/app_robustness_noprac_%s.tex", current_specialty))
