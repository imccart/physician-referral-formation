# specialist_fe.R -- Estimate specialist fixed effects from the full sample.
#
# The mover choice set is too sparse to pin specialist fixed effects: most
# specialists are referred by only a handful of movers or none, so their
# effect separates and runs off to +/- infinity in the second-stage recovery.
# Non-movers refer to the same specialists in far greater numbers, so here we
# estimate the specialist effect on the full choice set (movers and non-movers
# together), holding the Jochmans coefficient fixed as an offset, and winsorize
# the few effects that remain perfectly separated even in the full sample. The
# result is written to data/output/specialist_fe_{spec}.csv and merged into the
# second stage, where only the doctor effects are then recovered on the movers.
#
# The specialist effect is common across all referrers under the model, so
# using the full referring population to estimate it adds data, not assumptions.
# Reads its own data. Requires current_specialty.

covars <- c("same_sex", "same_prac", "diff_dist", "same_race", "diff_age", "diff_gradyear")

# Jochmans coefficient (same first stage as 2_logit_twfe.R, spec 4)
df_jochmans <- read_csv(sprintf("data/output/df_jochmans_%s.csv", current_specialty),
                        show_col_types = FALSE)
beta_model <- feglm(
  referral ~ same_sex + same_prac + diff_dist + same_race + diff_age + diff_gradyear | year,
  data = df_jochmans, vcov = ~hrr, family = binomial("logit")
)
beta <- coef(beta_model)[covars]

# Full choice set: every PCP-specialist pair in an HRR-year, movers and a sample
# of non-movers. Large (a few GB), so read only the needed columns with fread.
df_full <- as_tibble(fread(sprintf("data/output/df_logit_full_%s.csv", current_specialty),
                           select = c("doctor", "specialist", "referral", covars))) %>%
  drop_na(all_of(covars)) %>%
  mutate(doctor = as.numeric(doctor), specialist = as.numeric(specialist))
message("Full-sample specialist FE: ", nrow(df_full), " obs, ",
        n_distinct(df_full$specialist), " specialists, ", n_distinct(df_full$doctor), " doctors")

df_full <- df_full %>%
  mutate(.Xbeta = drop(as.matrix(df_full[, covars]) %*% beta))

fe_full <- feglm(referral ~ 1 | doctor + specialist, data = df_full,
                 offset = ~.Xbeta, family = binomial("logit"), glm.iter = 200)

# Specialist effects: keep the finite estimates and winsorize the separated
# outliers. A log-odds effect beyond +/-6 means the specialist is essentially
# never or always referred (p outside [0.0025, 0.9975]), where the exact value
# is meaningless and a handful of them would otherwise dominate the scale.
# Center so specialists absent from the table default to the mean (0) downstream.
fes  <- fixef(fe_full, notes = FALSE)
g    <- fes$specialist
keep <- is.finite(g)
spec_fe <- tibble(specialist = as.numeric(names(g)[keep]),
                  gamma_j     = pmin(pmax(g[keep], -6), 6))
spec_fe <- spec_fe %>% mutate(gamma_j = gamma_j - mean(gamma_j))
cat(sprintf("DIAG specialist FE: n=%d  sd=%.3f  IQR=[%.2f, %.2f]  n_capped=%d\n",
            nrow(spec_fe), sd(spec_fe$gamma_j),
            quantile(spec_fe$gamma_j, .25), quantile(spec_fe$gamma_j, .75), sum(abs(g[keep]) > 6)))

write_csv(spec_fe, sprintf("data/output/specialist_fe_%s.csv", current_specialty))
message("Wrote ", nrow(spec_fe), " specialist effects to data/output/specialist_fe_",
        current_specialty, ".csv")

# Transfer test: do the full-sample specialist effects predict movers' referrals,
# and at full strength? Enter the estimated effects into the mover-referral model
# as a single regressor with coefficient lambda, holding the structural index as an
# offset and recovering the doctor effects, with the standard error clustered by HRR.
# lambda = 0 means the effects carry no information for movers; lambda = 1 means they
# transfer at full strength (the model's common-specialist-effect assumption is
# calibrated). This is the formal version of the deviance-drop check.
df_mov <- read_csv(sprintf("data/output/df_logit_movers_%s.csv", current_specialty),
                   show_col_types = FALSE) %>%
  drop_na(all_of(covars))
df_mov <- df_mov %>%
  mutate(spec_key = as.character(specialist),
         .Xbeta   = drop(as.matrix(df_mov[, covars]) %*% beta),
         doctor   = as.factor(doctor)) %>%
  left_join(transmute(spec_fe, spec_key = as.character(specialist), gamma_j), by = "spec_key")
message(sprintf("Transfer-test specialist FE match: %.1f%%", 100 * mean(!is.na(df_mov$gamma_j))))
df_mov <- df_mov %>% mutate(gamma_j = coalesce(gamma_j, 0))

m_lambda <- feglm(referral ~ gamma_j | doctor, data = df_mov, offset = ~.Xbeta,
                  vcov = ~doc_hrr, family = binomial("logit"), glm.iter = 200)
lam <- unname(coef(m_lambda)["gamma_j"])
se  <- sqrt(vcov(m_lambda)["gamma_j", "gamma_j"])

transfer <- tibble(specialty = current_specialty, lambda = lam, se = se,
                   ci_lo = lam - 1.96 * se, ci_hi = lam + 1.96 * se,
                   p_lambda0 = 2 * pnorm(-abs(lam / se)),
                   p_lambda1 = 2 * pnorm(-abs((lam - 1) / se)),
                   n_obs = m_lambda$nobs)
write_csv(transfer, sprintf("results/tables/specialist_transfer_%s.csv", current_specialty))
message(sprintf("Transfer test: lambda = %.3f (SE %.3f), p(lambda=0) = %.1e, p(lambda=1) = %.3f",
                lam, se, transfer$p_lambda0, transfer$p_lambda1))
