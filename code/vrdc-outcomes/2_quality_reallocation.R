# Quality reallocation: how much each referral determinant moves referrals
# toward higher- or lower-quality specialists, with confidence intervals.
#
# For each PCP the model implies a distribution of referral probabilities over
# the specialists in their choice set. Setting a determinant's coefficient to
# zero gives a counterfactual distribution, and the gain is the resulting change
# in the probability-weighted mean of the specialists' shrunk quality,
#     gain = baseline - counterfactual,
# so a positive gain means quality improves when the determinant is removed. We
# report the gain for every determinant and outcome, both across all PCPs and,
# for practice affiliation, restricted to PCPs who have a same-practice
# specialist available (the effect is a mechanical zero for the rest).
#
# Two sources of sampling error are propagated by a parametric bootstrap: the
# referral coefficients, drawn from their HRR-clustered sampling distribution,
# and each specialist's quality, drawn from its empirical-Bayes posterior. Draws
# hold the recovered fixed effects at their point estimates. data.table is used
# for the within-PCP normalization inside the bootstrap loop, which is otherwise
# prohibitively slow. Spending is expressed per referral, admissions and
# mortality per 1,000 referrals.

set.seed(20260805)

specs    <- c("ortho", "cardioem", "derm")
covars   <- c("same_sex", "same_prac", "diff_dist", "same_race", "diff_age", "diff_gradyear")
outcomes <- c("admit_ne", "admit", "death", "spend")
B        <- 400

eb <- fread("data/output/vrdc_specialist_quality_eb.csv")
eb[, Specialist_NPI := as.character(Specialist_NPI)]

reallocate <- function(sp) {
  message("  ", sp)
  est      <- fread(sprintf("results/tables/estimates_%s.csv", sp))[spec == 4]
  beta_hat <- setNames(est$beta,    est$term)[covars]
  beta_se  <- setNames(est$beta_se, est$term)[covars]

  df <- fread(sprintf("data/output/df_logit_movers_%s.csv", sp))
  df[, specialist := as.character(specialist)]
  df <- df[complete.cases(df[, ..covars])]
  # Specialist effect from the full-sample table (specialist_fe.R), folded into
  # the offset; only the doctor effect is recovered on the movers, matching the
  # main two-stage recovery.
  sfe <- as.data.table(read_csv(sprintf("data/output/specialist_fe_%s.csv", sp),
                                show_col_types = FALSE))
  sfe[, specialist := as.character(specialist)]
  df[sfe, gamma_j := i.gamma_j, on = "specialist"]
  message("  specialist FE match: ", sprintf("%.1f%%", 100 * mean(!is.na(df$gamma_j))))
  df[is.na(gamma_j), gamma_j := 0]
  df[, .Xbeta := as.numeric(as.matrix(.SD) %*% beta_hat) + gamma_j, .SDcols = covars]

  fe <- fixef(feglm(referral ~ 1 | doctor, data = df,
                    offset = ~.Xbeta, family = binomial("logit"), glm.iter = 200))
  df[, eta_hat := .Xbeta + fe$doctor[as.character(doctor)]]
  df <- df[is.finite(eta_hat)]

  q <- eb[specialty == sp, .(specialist = Specialist_NPI,
           m_admit_ne = m_eb_admit_ne, m_admit = m_eb_admit, m_death = m_eb_death, m_spend = m_eb_spend,
           s_admit_ne = se_eb_admit_ne, s_admit = se_eb_admit, s_death = se_eb_death, s_spend = se_eb_spend)]
  df <- q[df, on = "specialist", nomatch = 0]

  Xmat   <- as.matrix(df[, ..covars])
  N_pcp  <- uniqueN(df$doctor)
  df[, has_prac := any(same_prac == 1), by = doctor]
  N_tied <- uniqueN(df$doctor[df$has_prac])

  df[, sp_idx := .GRP, by = specialist]
  spq <- unique(df[, .(sp_idx, m_admit_ne, m_admit, m_death, m_spend,
                       s_admit_ne, s_admit, s_death, s_spend)])
  setkey(spq, sp_idx); S <- nrow(spq)

  rescale <- function(o, g) if (o == "spend") g else g * 1000

  # Shares are normalized in the log domain so that PCPs whose candidates all
  # have negligible link probability do not produce 0/0.
  gains <- function(beta_b, mrow) {
    df[, eta_b := eta_hat + as.numeric(Xmat %*% (beta_b - beta_hat))]
    df[, lw := plogis(eta_b, log.p = TRUE)]
    df[, s0 := { e <- exp(lw - max(lw)); e / sum(e) }, by = doctor]
    out <- numeric(0)
    for (v in covars) {
      df[, lw := plogis(eta_b - beta_b[[v]] * get(v), log.p = TRUE)]
      df[, sv := { e <- exp(lw - max(lw)); e / sum(e) }, by = doctor]
      shift <- df$s0 - df$sv
      for (o in outcomes) {
        contrib <- shift * mrow[[o]]
        out[paste(v, o, "all", sep = "__")] <- rescale(o, sum(contrib) / N_pcp)
        if (v == "same_prac")
          out[paste(v, o, "tied", sep = "__")] <- rescale(o, sum(contrib[df$has_prac]) / N_tied)
      }
    }
    out
  }

  point <- gains(beta_hat, df[, .(admit_ne = m_admit_ne, admit = m_admit, death = m_death, spend = m_spend)])

  draws <- matrix(NA_real_, B, length(point), dimnames = list(NULL, names(point)))
  for (b in seq_len(B)) {
    beta_b <- beta_hat + rnorm(length(beta_hat), 0, beta_se)
    m_b <- data.table(admit_ne = spq$m_admit_ne + rnorm(S, 0, spq$s_admit_ne),
                      admit    = spq$m_admit    + rnorm(S, 0, spq$s_admit),
                      death    = spq$m_death    + rnorm(S, 0, spq$s_death),
                      spend    = spq$m_spend    + rnorm(S, 0, spq$s_spend))
    draws[b, ] <- gains(beta_b, m_b[df$sp_idx])
  }

  data.table(combo = names(point), point = point,
             lo = apply(draws, 2, quantile, 0.025, na.rm = TRUE),
             hi = apply(draws, 2, quantile, 0.975, na.rm = TRUE)
  )[, c("determinant", "outcome", "sample") := tstrsplit(combo, "__", fixed = TRUE)
  ][, combo := NULL][, specialty := sp][]
}

realloc <- rbindlist(lapply(specs, reallocate))
fwrite(realloc, "results/tables/quality_realloc_ci.csv")

nice <- c(same_prac = "Practice", diff_dist = "Distance", same_sex = "Gender",
          same_race = "Race", diff_age = "Age", diff_gradyear = "Experience")
show <- function(det, samp) {
  cat("\n", nice[det], if (samp == "tied") " (PCPs with a same-practice specialist)" else "", "\n", sep = "")
  tab <- realloc[determinant == det & sample == samp,
                 .(specialty, outcome, point = round(point, 2), lo = round(lo, 2), hi = round(hi, 2))]
  print(tab[order(outcome, specialty)])
}
show("same_prac", "tied")
show("same_prac", "all")
show("diff_dist", "all")
