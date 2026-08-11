## Appendix: Robustness — dropping same_prac (Referee Comment 1b)
## Re-estimate the full Jochmans spec without same_prac, then two-stage MFX.
## Output: two-column table (structural β + MFX) for the no-practice model.

message("  Robustness: dropping same_prac...")

# Stage 1: Jochmans without same_prac
covars_noprac <- c("same_sex", "diff_dist", "same_race", "diff_age", "diff_gradyear")

logit_noprac <- feglm(
  referral ~ same_sex + diff_dist + same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = ~hrr,
  family = binomial("logit")
)

# Stage 2: FE recovery — specialist effect held fixed (specialist_fe.R, via
# dat_fe$gamma_spec built in 2_logit_twfe.R), doctor effect recovered on movers.
recover_fes_noprac <- function(joch_model, spec_covars) {
  beta <- coef(joch_model)[spec_covars]
  Xbeta <- drop(as.matrix(dat_fe[, spec_covars]) %*% beta) + dat_fe$gamma_spec
  dat_tmp <- dat_fe %>% mutate(.Xbeta = Xbeta)

  fe_mod <- feglm(
    referral ~ 1 | doctor,
    data = dat_tmp,
    offset = ~.Xbeta,
    family = binomial("logit"),
    glm.iter = 200
  )

  fes <- fixef(fe_mod)
  doc_fe <- fes$doctor[as.character(dat_fe$doctor)]
  eta    <- Xbeta + doc_fe          # Xbeta already includes the specialist effect
  valid  <- !is.na(eta)

  list(beta = beta, eta = eta, valid = valid, n_valid = sum(valid),
       fe_mod = fe_mod, fes = fes)
}

# Weighted MFX. With same_prac dropped there is no baseline-without-practice to
# key the weight off, so we weight by the fitted referral probability, the
# natural analog of the main weight.
compute_mfx_noprac <- function(stage2, joch_model, spec_covars) {
  beta  <- stage2$beta
  eta   <- stage2$eta[stage2$valid]
  dat_v <- dat_fe[stage2$valid, ]
  V     <- vcov(joch_model, type = "HC1")[spec_covars, spec_covars]

  w_base <- plogis(eta); w_base <- w_base / sum(w_base)

  map(spec_covars, function(v) {
    is_bin <- v %in% c("same_sex", "same_race")
    delta  <- if (v == "diff_dist") 5 else 1
    if (is_bin) {
      eta1 <- eta - beta[v] * dat_v[[v]] + beta[v]
      eta0 <- eta - beta[v] * dat_v[[v]]
    } else {
      eta1 <- eta + beta[v] * delta
      eta0 <- eta
    }
    p1 <- plogis(eta1); p0 <- plogis(eta0)
    dp <- sum(w_base * (p1 - p0))

    grad_i <- matrix(0, nrow = length(eta), ncol = length(spec_covars))
    colnames(grad_i) <- spec_covars
    for (k in spec_covars) {
      if (is_bin) {
        deta1_dk <- if (k == v) rep(1, length(eta)) else dat_v[[k]]
        deta0_dk <- if (k == v) rep(0, length(eta)) else dat_v[[k]]
      } else {
        deta1_dk <- dat_v[[k]] + if (k == v) delta else 0
        deta0_dk <- dat_v[[k]]
      }
      grad_i[, k] <- (p1 * (1 - p1)) * deta1_dk - (p0 * (1 - p0)) * deta0_dk
    }
    g_bar <- colSums(grad_i * w_base)
    se <- sqrt(as.numeric(t(g_bar) %*% V %*% g_bar))
    tibble(term = v, estimate = dp, std.error = se)
  }) %>% bind_rows()
}

stage2_noprac <- recover_fes_noprac(logit_noprac, covars_noprac)
mfx_noprac    <- compute_mfx_noprac(stage2_noprac, logit_noprac, covars_noprac)

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
