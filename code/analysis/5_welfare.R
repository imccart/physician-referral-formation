
# Welfare calculations (appendix) -------------------------------------------
# Self-contained: reads ortho data and estimates CSV, reconstructs predicted
# probabilities, computes counterfactual quality reallocation.

current_specialty <- "ortho"

covars <- c("same_sex", "same_prac", "diff_dist",
            "same_race", "diff_age", "diff_gradyear")

## Read estimates from spec 3 (without peer referrals — the welfare-relevant spec)
est <- read_csv("results/tables/estimates_ortho.csv", show_col_types = FALSE) %>%
  filter(spec == 3)
beta_hat <- setNames(est$beta, est$term)

## Read ortho choice set data
df_logit <- read_csv("data/output/df_logit_movers_ortho.csv", show_col_types = FALSE) %>%
  mutate(doctor = as.factor(doctor), specialist = as.factor(specialist))

## Complete cases on covariates
Xmat <- as.matrix(df_logit[, covars])
na_mask <- complete.cases(Xmat)
dat_fe <- df_logit[na_mask, ]

## Reconstruct Stage 2: FE recovery with offset
Xbeta <- drop(as.matrix(dat_fe[, covars]) %*% beta_hat)
dat_fe <- dat_fe %>% mutate(.Xbeta = Xbeta)

message("Recovering FEs for welfare (ortho, spec 3)...")
fe_mod <- fixest::feglm(
  referral ~ 1 | doctor + specialist,
  data = dat_fe,
  offset = ~.Xbeta,
  family = binomial("logit"),
  glm.iter = 200
)

fes <- fixef(fe_mod)
doc_fe  <- fes$doctor[as.character(dat_fe$doctor)]
spec_fe <- fes$specialist[as.character(dat_fe$specialist)]
eta <- Xbeta + doc_fe + spec_fe
valid <- !is.na(eta)

message("  ", sum(valid), " valid obs for welfare")

## Read richer quality measure from original referral pairs file
old_qual <- read_csv("data/input/referrals/ReferralPairs_Large.csv", show_col_types = FALSE) %>%
  select(specialist = Specialist_ID, spec_qual_old = spec_qual) %>%
  distinct(specialist, .keep_all = TRUE) %>%
  mutate(specialist = as.factor(specialist))

## Build welfare data
dat <- dat_fe[valid, ] %>%
  select(doctor, specialist, referral, hrr = doc_hrr, all_of(covars)) %>%
  left_join(old_qual, by = "specialist")
eta_base <- eta[valid]

has_qual <- !is.na(dat$spec_qual_old)
message(sum(has_qual), " of ", length(has_qual), " obs have old quality measure (",
        round(100 * mean(has_qual), 1), "%)")
dat      <- dat[has_qual, ]
eta_base <- eta_base[has_qual]
p0       <- plogis(eta_base)

message(nrow(dat), " obs used for welfare (valid FEs + non-NA spec_qual)")

## Counterfactual: zero out covariate j
counterfactual_prob <- function(var) {
  eta_cf <- eta_base - beta_hat[var] * dat[[var]]
  plogis(eta_cf)
}

## Summary-stat helper & labels
stat_vec <- function(x) {
  c(mean = mean(x, na.rm = TRUE),
    sd   = sd(x,   na.rm = TRUE),
    q10  = quantile(x, .10, na.rm = TRUE),
    q25  = quantile(x, .25, na.rm = TRUE),
    q50  = quantile(x, .50, na.rm = TRUE),
    q75  = quantile(x, .75, na.rm = TRUE),
    q90  = quantile(x, .90, na.rm = TRUE))
}

nice_lab <- c(
  same_sex      = "Gender",
  same_prac     = "Practice group",
  diff_dist     = "Distance",
  same_race     = "Race",
  diff_age      = "Age",
  diff_gradyear = "Experience"
)

## Welfare by covariate: Δp × (q_j - q̄_i) per HRR
welfare_rows <- imap(covars, function(v, idx) {
  p1 <- counterfactual_prob(v)

  pcp_mean_qual <- dat %>%
    filter(referral == 1) %>%
    group_by(doctor) %>%
    summarise(mean_q = mean(spec_qual_old, na.rm = TRUE)) %>%
    ungroup()

  dat_rel <- dat %>% left_join(pcp_mean_qual, by = "doctor")
  delta <- (p1 - p0) * (dat_rel$spec_qual_old - dat_rel$mean_q) * 1000

  hrr_gains <- dat %>%
    mutate(delta = delta) %>%
    group_by(hrr) %>%
    summarise(gain = mean(delta, na.rm = TRUE), .groups = "drop")

  tibble(
    Variable = nice_lab[v],
    Markets  = nrow(hrr_gains),
    !!!stat_vec(hrr_gains$gain)
  )
}) %>% bind_rows()

## Format and export
welfare_out <- welfare_rows %>%
  mutate(across(-c(Variable, Markets),
                ~ formatC(.x, digits = 2, format = "f")))

latex_welfare <- welfare_out %>%
  kable(format   = "latex",
        booktabs = TRUE,
        linesep  = "",
        align    = "lclrrrrrr",
        col.names = c("Variable", "Markets",
                      "Mean", "SD", "P10", "P25", "P50", "P75", "P90")) %>%
  row_spec(5, extra_latex_after = "\\addlinespace")

writeLines(as.character(latex_welfare), "results/tables/welfare_summary_ortho.tex")

## Cleanup
rm(df_logit, dat_fe, fe_mod, fes, Xmat, Xbeta, eta, dat, old_qual)
gc()

message("Welfare table saved: welfare_summary_ortho.tex")
