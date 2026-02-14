
# Welfare calculations (appendix) -------------------------------------------
# Uses two-stage predicted probabilities from 2_logit_twfe.R:
#   stage2_3$eta, stage2_3$valid, stage2_3$beta (from Jochmans spec 3)
# Counterfactual: zero out covariate j, recompute predicted probability

covars <- c("same_sex", "same_prac", "diff_dist",
            "same_race", "diff_age", "diff_gradyear")

## Build welfare data (valid obs from stage 2 with non-NA spec_qual) ----
dat <- dat_fe[stage2_3$valid, ] %>%
  select(doctor, specialist, referral, hrr = doc_hrr, spec_qual,
         all_of(covars))
eta_base <- stage2_3$eta[stage2_3$valid]

# Further restrict to non-NA spec_qual
has_qual <- !is.na(dat$spec_qual)
dat      <- dat[has_qual, ]
eta_base <- eta_base[has_qual]
p0       <- plogis(eta_base)

beta_hat <- stage2_3$beta

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

## Welfare by covariate: Δp × (q_j - q̄_i) per HRR ----
welfare_rows <- imap_dfr(covars, function(v, idx) {

  p1 <- counterfactual_prob(v)

  # Relative quality: specialist quality minus PCP's mean referral quality
  pcp_mean_qual <- dat %>%
    filter(referral == 1) %>%
    group_by(doctor) %>%
    summarise(mean_q = mean(spec_qual, na.rm = TRUE)) %>%
    ungroup()

  dat_rel <- dat %>% left_join(pcp_mean_qual, by = "doctor")
  delta <- (p1 - p0) * (dat_rel$spec_qual - dat_rel$mean_q) * 1000

  hrr_gains <- dat %>%
    mutate(delta = delta) %>%
    group_by(hrr) %>%
    summarise(gain = mean(delta, na.rm = TRUE), .groups = "drop")

  tibble(
    Variable = nice_lab[v],
    Markets  = nrow(hrr_gains),
    !!!stat_vec(hrr_gains$gain)
  )
})

## Format and export ----
welfare_out <- welfare_rows %>%
  mutate(across(-c(Variable, Markets),
                ~ formatC(.x, digits = 2, format = "f")))

latex_welfare <- welfare_out %>%
  kable(format   = "latex",
        booktabs = TRUE,
        linesep  = "",
        align    = "lclrrrrrr",
        col.names = c("Variable", "Markets",
                      "Mean", "SD", "P10", "P25", "P50", "P75", "P90"))

writeLines(as.character(latex_welfare), "results/tables/welfare_summary.tex")
