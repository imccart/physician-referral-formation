
# Baseline logit with race (Zeltzer-style, doctor FE only) -------------------

logit_race1 <- fixest::feglm(
  referral ~ same_sex + same_race + spec_male + exp_spec | Year + doctor,
  data = df_logit,
  vcov = ~doctor,
  family = binomial(link = "logit")
)

logit_race2 <- fixest::feglm(
  referral ~ same_sex + same_race + spec_male + exp_spec + same_prac + dist_miles | Year + doctor,
  data = df_logit,
  vcov = ~doctor,
  family = binomial(link = "logit")
)

logit_race3 <- fixest::feglm(
  referral ~ same_sex + same_race + spec_male + exp_spec + same_prac + dist_miles + diff_age + diff_gradyear | Year + doctor,
  data = df_logit,
  vcov = ~doctor,
  family = binomial(link = "logit")
)


## Manual MFX: first-difference + delta-method SEs (avg_slopes incompatible with feglm FEs)
compute_mfx_zeltzer <- function(model, data, spec_covars) {
  beta <- coef(model)[spec_covars]
  V    <- vcov(model)
  V    <- V[spec_covars, spec_covars]

  # Build eta manually (predict() returns estimation-sample length only)
  Xmat    <- as.matrix(data[, spec_covars])
  na_mask <- complete.cases(Xmat)
  dat_v   <- data[na_mask, ]
  Xbeta   <- drop(Xmat[na_mask, ] %*% beta)

  fes     <- fixef(model)
  doc_fe  <- fes$doctor[as.character(dat_v$doctor)]
  year_fe <- fes$Year[as.character(dat_v$Year)]
  eta     <- Xbeta + doc_fe + year_fe

  ok    <- !is.na(eta)
  eta   <- eta[ok]
  dat_v <- dat_v[ok, ]

  map(spec_covars, function(v) {
    is_bin <- v %in% c("same_sex", "same_race", "spec_male", "same_prac")
    delta  <- 1

    if (is_bin) {
      eta1 <- eta - beta[v] * dat_v[[v]] + beta[v]
      eta0 <- eta - beta[v] * dat_v[[v]]
    } else {
      eta1 <- eta + beta[v] * delta
      eta0 <- eta
    }

    p1 <- plogis(eta1)
    p0 <- plogis(eta0)
    dp <- mean(p1 - p0)

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

    g_bar <- colMeans(grad_i)
    se <- sqrt(as.numeric(t(g_bar) %*% V %*% g_bar))

    tibble(term = v, estimate = dp, std.error = se)
  }) %>% bind_rows()
}

covars_race1 <- c("same_sex", "same_race", "spec_male", "exp_spec")
covars_race2 <- c("same_sex", "same_race", "spec_male", "exp_spec", "same_prac", "dist_miles")
covars_race3 <- c("same_sex", "same_race", "spec_male", "exp_spec", "same_prac", "dist_miles",
                   "diff_age", "diff_gradyear")

mfx_logit_race1 <- compute_mfx_zeltzer(logit_race1, df_logit, covars_race1)
mfx_logit_race2 <- compute_mfx_zeltzer(logit_race2, df_logit, covars_race2)
mfx_logit_race3 <- compute_mfx_zeltzer(logit_race3, df_logit, covars_race3)

## Extract MFX estimates and SEs into a table
fmt_num <- function(x) {
  s <- sprintf("%.3f", abs(x))
  if (x < 0) paste0("$-$", s) else s
}

extract_mfx <- function(mfx_obj, terms) {
  df <- as.data.frame(mfx_obj)
  out <- setNames(rep("", length(terms)), terms)
  se  <- setNames(rep("", length(terms)), terms)
  for (v in terms) {
    row <- df[df$term == v, ]
    if (nrow(row) == 1) {
      out[v] <- fmt_num(row$estimate)
      se[v]  <- paste0("(", sprintf("%.3f", row$std.error), ")")
    }
  }
  list(est = out, se = se)
}

mfx_terms <- c("same_sex", "spec_male", "exp_spec", "same_prac",
               "same_race", "dist_miles", "diff_age", "diff_gradyear")
mfx_labels <- c("Same gender", "Male specialist", "Specialist experience (10 years)",
                "Same practice group", "Same race", "Distance (miles)",
                "Similar age", "Similar experience")

m1 <- extract_mfx(mfx_logit_race1, mfx_terms)
m2 <- extract_mfx(mfx_logit_race2, mfx_terms)
m3 <- extract_mfx(mfx_logit_race3, mfx_terms)

## Build rows: coefficient estimate, then SE
tbl_rows <- map(seq_along(mfx_terms), function(i) {
  bind_rows(
    tibble(` ` = mfx_labels[i], `(1)` = m1$est[i], `(2)` = m2$est[i], `(3)` = m3$est[i]),
    tibble(` ` = "",             `(1)` = m1$se[i],  `(2)` = m2$se[i],  `(3)` = m3$se[i])
  )
}) %>% bind_rows()

## Add footer rows
footer <- tribble(
  ~` `, ~`(1)`, ~`(2)`, ~`(3)`,
  "Year FE", "Yes", "Yes", "Yes",
  "Doctor FE", "Yes", "Yes", "Yes",
  "Specialist FE", "No", "No", "No",
  "Observations", format(nobs(logit_race1), big.mark=","),
                  format(nobs(logit_race2), big.mark=","),
                  format(nobs(logit_race3), big.mark=","),
  "Pseudo-$R^2$", gsub("-", "$-$", format(logit_race1$pseudo_r2, digits = 2)),
                  gsub("-", "$-$", format(logit_race2$pseudo_r2, digits = 2)),
                  gsub("-", "$-$", format(logit_race3$pseudo_r2, digits = 2))
)

tbl_all <- bind_rows(tbl_rows, footer)

kable(tbl_all, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
      align = c("l", "r", "r", "r")) %>%
  row_spec(nrow(tbl_rows), extra_latex_after = "\\midrule") %>%
  row_spec(nrow(tbl_rows) + 3, extra_latex_after = "\\midrule") %>%
  save_kable(sprintf("results/tables/app_logit_race_mfx_%s.tex", current_specialty))



# Two-stage estimation: Jochmans β + FE recovery ----------------------------
# Stage 1: Jochmans quartet estimator → consistent β (FEs eliminated via differencing)
# Stage 2: Hold β fixed as offset, recover doctor + specialist FEs via fixest
# MFX: first-difference using full predicted probabilities (Xβ + α_i + γ_j)

covars <- c("same_sex", "same_prac", "diff_dist",
            "same_race", "diff_age", "diff_gradyear")

## Stage 1: Jochmans β (three specifications) ----

message("Stage 1: Jochmans quartet estimation...")

logit_twfe1 <- fixest::feglm(
  referral ~ same_sex + same_prac + diff_dist | year,
  data = df_logit_twfe,
  vcov = ~hrr,
  family = binomial("logit")
)

logit_twfe2 <- fixest::feglm(
  referral ~ same_sex + same_prac + diff_dist + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = ~hrr,
  family = binomial("logit")
)

logit_twfe3 <- fixest::feglm(
  referral ~ same_sex + same_prac + diff_dist + same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = ~hrr,
  family = binomial("logit")
)


## Stage 2: FE recovery with offset ----

message("Stage 2: FE recovery...")

# Prep: complete cases on covariates
Xmat <- as.matrix(df_logit[, covars])
na_mask <- complete.cases(Xmat)
dat_fe <- df_logit[na_mask, ]

# Helper: recover FEs for a given Jochmans model
recover_fes <- function(joch_model, spec_covars) {
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

covars1 <- c("same_sex", "same_prac", "diff_dist")
covars2 <- c("same_sex", "same_prac", "diff_dist", "diff_age", "diff_gradyear")
covars3 <- covars

stage2_1 <- recover_fes(logit_twfe1, covars1)
stage2_2 <- recover_fes(logit_twfe2, covars2)
stage2_3 <- recover_fes(logit_twfe3, covars3)

message("  Spec 1: ", stage2_1$n_valid, " valid obs")
message("  Spec 2: ", stage2_2$n_valid, " valid obs")
message("  Spec 3: ", stage2_3$n_valid, " valid obs")


## MFX via first-difference + delta-method SEs ----

message("Computing MFX with delta-method SEs...")

compute_mfx <- function(stage2, joch_model, spec_covars) {
  beta  <- stage2$beta
  eta   <- stage2$eta[stage2$valid]
  dat_v <- dat_fe[stage2$valid, ]
  V     <- vcov(joch_model, type = "HC1")
  # Subset vcov to spec_covars (Jochmans model may have year FEs in vcov)
  V <- V[spec_covars, spec_covars]

  map(spec_covars, function(v) {
    is_bin <- v %in% c("same_sex", "same_prac", "same_race")
    delta  <- if (v == "diff_dist") 5 else 1

    if (is_bin) {
      eta1 <- eta - beta[v] * dat_v[[v]] + beta[v]
      eta0 <- eta - beta[v] * dat_v[[v]]
    } else {
      eta1 <- eta + beta[v] * delta
      eta0 <- eta
    }

    p1 <- plogis(eta1)
    p0 <- plogis(eta0)
    dp <- mean(p1 - p0)

    # Delta-method gradient: ∂MFX/∂β_k
    # For each obs i: ∂(p1_i - p0_i)/∂β_k
    grad_i <- matrix(0, nrow = length(eta), ncol = length(spec_covars))
    colnames(grad_i) <- spec_covars

    for (k in spec_covars) {
      if (is_bin) {
        # η1 = η - β_v*x_v + β_v*1, η0 = η - β_v*x_v + β_v*0
        # ∂η1/∂β_k = x_k (from η) + I(k==v)*(1 - x_v) - I(k==v is already in the derivative)
        # More carefully: η1 = Σ_{j≠v} β_j*x_j + β_v*1 + α + γ
        # ∂η1/∂β_k = x_k if k≠v, 1 if k==v
        # ∂η0/∂β_k = x_k if k≠v, 0 if k==v
        deta1_dk <- if (k == v) rep(1, length(eta)) else dat_v[[k]]
        deta0_dk <- if (k == v) rep(0, length(eta)) else dat_v[[k]]
      } else {
        # η1 = η + β_v*delta, η0 = η
        # ∂η1/∂β_k = x_k + I(k==v)*delta
        # ∂η0/∂β_k = x_k
        deta1_dk <- dat_v[[k]] + if (k == v) delta else 0
        deta0_dk <- dat_v[[k]]
      }
      grad_i[, k] <- (p1 * (1 - p1)) * deta1_dk - (p0 * (1 - p0)) * deta0_dk
    }

    g_bar <- colMeans(grad_i)
    se <- sqrt(as.numeric(t(g_bar) %*% V %*% g_bar))

    tibble(term = v, estimate = dp, std.error = se)
  }) %>% bind_rows()
}

mfx1 <- compute_mfx(stage2_1, logit_twfe1, covars1)
mfx2 <- compute_mfx(stage2_2, logit_twfe2, covars2)
mfx3 <- compute_mfx(stage2_3, logit_twfe3, covars3)


## Build combined table: structural β + MFX ----

coef_labels <- c(
  same_sex      = "Same gender",
  same_prac     = "Same practice group",
  same_race     = "Same race",
  diff_dist     = "Distance",
  diff_age      = "Age difference",
  diff_gradyear = "Experience difference"
)

vars_order <- names(coef_labels)

# Helper: format a cell
fmt_est <- function(x) { if (is.na(x) || length(x) == 0) " " else gsub("-", "$-$", comma(x, accuracy = 0.001)) }
fmt_se  <- function(x) { if (is.na(x) || length(x) == 0) " " else paste0("(", comma(abs(x), accuracy = 0.001), ")") }

# Build β panel
beta_block <- function(var) {
  lbl <- coef_labels[[var]]
  get_b <- function(mod) {
    b <- coef(mod)
    if (var %in% names(b)) b[[var]] else NA_real_
  }
  get_se <- function(mod) {
    s <- sqrt(diag(vcov(mod, vcov = ~hrr)))
    if (var %in% names(s)) s[[var]] else NA_real_
  }

  tibble(term = lbl,
         `(1)` = fmt_est(get_b(logit_twfe1)),
         `(2)` = fmt_est(get_b(logit_twfe2)),
         `(3)` = fmt_est(get_b(logit_twfe3))) %>%
  bind_rows(
    tibble(term = "",
           `(1)` = fmt_se(get_se(logit_twfe1)),
           `(2)` = fmt_se(get_se(logit_twfe2)),
           `(3)` = fmt_se(get_se(logit_twfe3)))
  )
}

# Build MFX panel
mfx_block <- function(var) {
  lbl <- coef_labels[[var]]
  get_val <- function(mfx_df, what) {
    row <- mfx_df %>% filter(term == var)
    if (nrow(row) == 0) NA_real_ else row[[what]]
  }

  tibble(term = lbl,
         `(4)` = fmt_est(get_val(mfx1, "estimate")),
         `(5)` = fmt_est(get_val(mfx2, "estimate")),
         `(6)` = fmt_est(get_val(mfx3, "estimate"))) %>%
  bind_rows(
    tibble(term = "",
           `(4)` = fmt_se(get_val(mfx1, "std.error")),
           `(5)` = fmt_se(get_val(mfx2, "std.error")),
           `(6)` = fmt_se(get_val(mfx3, "std.error")))
  )
}

beta_body <- lapply(vars_order, beta_block) %>% bind_rows()
mfx_body  <- lapply(vars_order, mfx_block) %>% bind_rows()

# Merge panels side by side
table_body <- bind_cols(beta_body, mfx_body %>% select(-term))

# Footer rows
n_joch  <- format(nobs(logit_twfe3), big.mark = ",")
n_stage2 <- format(stage2_3$n_valid, big.mark = ",")

footer <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`, ~`(5)`, ~`(6)`,
  "Year FE",        "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Doctor FE",      "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Specialist FE",  "Yes", "Yes", "Yes", "Yes", "Yes", "Yes",
  "Quartet obs",
    format(nobs(logit_twfe1), big.mark = ","),
    format(nobs(logit_twfe2), big.mark = ","),
    n_joch, " ", " ", " ",
  "Choice-set obs",
    " ", " ", " ",
    format(stage2_1$n_valid, big.mark = ","),
    format(stage2_2$n_valid, big.mark = ","),
    n_stage2
)

table_out <- bind_rows(table_body, footer)

kable(table_out,
      format    = "latex",
      booktabs  = TRUE,
      linesep   = "",
      align     = c("l", rep("r", 6)),
      col.names = c("", "(1)", "(2)", "(3)", "(4)", "(5)", "(6)")) %>%
  add_header_above(c(" " = 1,
                     "Structural $\\\\beta$ (log-odds)" = 3,
                     "Avg. marginal effects" = 3),
                   escape = FALSE) %>%
  row_spec(12, extra_latex_after = "\\midrule") %>%
  row_spec(15, extra_latex_after = "\\midrule") %>%
  save_kable(sprintf("results/tables/logit_twfe_mfx_%s.tex", current_specialty))


# Comment 4a: unique PCPs and specialists in Jochmans quartets ---------------
quartet_composition <- tibble(
  metric = c("Unique PCPs in quartets",
             "Unique specialists in quartets",
             "Pct non-separated (MFX sample)"),
  value  = c(n_distinct(c(df_logit_twfe$doc1, df_logit_twfe$doc2)),
             n_distinct(c(df_logit_twfe$spec1, df_logit_twfe$spec2)),
             round(stage2_3$n_valid / nrow(dat_fe), 3))
)

# Comment 4b: compare non-separated vs full choice set
nonsep_means <- dat_fe[stage2_3$valid, ] %>%
  summarise(same_prac = mean(same_prac, na.rm = TRUE),
            same_sex  = mean(same_sex, na.rm = TRUE),
            same_race = mean(same_race, na.rm = TRUE),
            dist      = mean(diff_dist, na.rm = TRUE))
full_means <- dat_fe %>%
  summarise(same_prac = mean(same_prac, na.rm = TRUE),
            same_sex  = mean(same_sex, na.rm = TRUE),
            same_race = mean(same_race, na.rm = TRUE),
            dist      = mean(diff_dist, na.rm = TRUE))

quartet_composition <- bind_rows(quartet_composition,
  tibble(metric = c("Non-sep same_prac", "Full same_prac",
                    "Non-sep same_sex", "Full same_sex",
                    "Non-sep same_race", "Full same_race",
                    "Non-sep mean_dist", "Full mean_dist"),
         value = c(round(nonsep_means$same_prac, 3), round(full_means$same_prac, 3),
                   round(nonsep_means$same_sex, 3), round(full_means$same_sex, 3),
                   round(nonsep_means$same_race, 3), round(full_means$same_race, 3),
                   round(nonsep_means$dist, 3), round(full_means$dist, 3))))

# Append to inline stats
inline_stats <- read_csv(sprintf("results/tables/inline_stats_%s.csv", current_specialty))
inline_stats <- bind_rows(inline_stats, quartet_composition)
write_csv(inline_stats, sprintf("results/tables/inline_stats_%s.csv", current_specialty))
