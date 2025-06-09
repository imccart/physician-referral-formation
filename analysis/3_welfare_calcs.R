

# Results by adverse events foregone (using quartet structure) -----
covars <- c("same_sex","same_prac","diff_dist",
            "same_race","diff_age","diff_gradyear")

## baseline probabilities and shortfall
beta_hat <- as.numeric(coef(logit_twfe3)[covars])

quartets <- df_logit_twfe %>%
  transmute(hrr  = hrr,
            q1       = as.numeric(spec1_qual),
            q2       = as.numeric(spec2_qual),
            across(all_of(covars)))

Xmat    <- as.matrix(quartets[, covars])
p_base  <- plogis(drop(Xmat %*% beta_hat))

q_best     <- pmax(quartets$q1, quartets$q2)
short_base <- q_best - (p_base * quartets$q1 + (1 - p_base) * quartets$q2)

## helper for summary stats
stat_vec <- function(x)
  c(mean = mean(x, na.rm=TRUE), sd = sd(x, na.rm=TRUE),
    q10  = quantile(x, .10, na.rm=TRUE), q25 = quantile(x, .25, na.rm=TRUE),
    q50  = quantile(x, .50, na.rm=TRUE), q75 = quantile(x, .75, na.rm=TRUE),
    q90  = quantile(x, .90, na.rm=TRUE))

## 2.  HRR-level gains per covariate -----------------------------
results <- map_dfr(seq_along(covars), function(j) {

  beta_cf <- beta_hat; beta_cf[j] <- 0                      # shut off one term
  p_cf    <- plogis(drop(Xmat %*% beta_cf))

  short_cf <- q_best - (p_cf * quartets$q1 + (1 - p_cf) * quartets$q2)
  delta    <- short_base - short_cf                        # positive ⇒ gain

 hrr_summary <- quartets %>%
    mutate(delta = delta) %>%
    filter( !!sym(covars[j]) != 0 ) %>%                          # differential quartets
    group_by(hrr) %>%
    summarise(gain = mean(delta * 1000, na.rm = TRUE), .groups = "drop")

  hrr_gain   <- hrr_summary$gain                    # numeric vector
  n_markets  <- nrow(hrr_summary)                   # HRRs with ≥1 diff. quartet

  tibble(
    Variable = covars[j],
    Markets  = n_markets,
    !!!stat_vec(hrr_gain)                           # splice the named vector
  )
}) %>%
  mutate(across(-c(Variable, Markets),
                ~ formatC(.x, digits = 3, format = "f")))

## LaTeX export
welfare_summary <- kable(results,
      format    = "latex",
      booktabs  = TRUE,
      align     = "lrrrrrrrr",
      col.names = c("Variable","Relevant Markets","Mean","SD","P10","P25","P50","P75","P90")) %>%
  kable_styling(latex_options = "hold_position")


writeLines(as.character(welfare_summary), "results/welfare_summary_quadruple.tex")





# Results by adverse events foregone using original logit data -----

covars <- c("same_sex","same_prac","diff_dist",
            "same_race","diff_age","diff_gradyear")

## point estimates for covars, *in the same order*
beta_hat <- coef(logit_twfe3)[covars] |> as.numeric()

## build the data used for welfare calcs
dat <- df_logit %>%                                          # your raw links
  rename(year = Year) %>%                                    # prediction needs 'year'
  select(doctor, referral, hrr=doc_hrr, spec_qual, all_of(covars)) %>%  # keep what we need
  drop_na(spec_qual, all_of(covars))                         # ensure no NAs

## model matrix (n  ×  6)
Xmat <- as.matrix(dat[, covars])

## baseline predicted probabilities
p0 <- plogis(drop(Xmat %*% beta_hat))

## Summary-stat helper & pretty labels
stat_vec <- function(x) {
  c(mean = mean(x, na.rm=TRUE),
    sd   = sd(x,   na.rm=TRUE),
    q10  = quantile(x, .10, na.rm=TRUE),
    q25  = quantile(x, .25, na.rm=TRUE),
    q50  = quantile(x, .50, na.rm=TRUE),
    q75  = quantile(x, .75, na.rm=TRUE),
    q90  = quantile(x, .90, na.rm=TRUE))
}

nice_lab <- c(
  same_sex      = "Gender",
  same_prac     = "Practice group",
  diff_dist     = "Distance",
  same_race     = "Race",
  diff_age      = "Age",
  diff_gradyear = "Experience "
)

## welfare panels
panels_AB <- imap_dfr(covars, function(v, idx) {

  ## 4.a  Shut down channel v (β_v ← 0) ------------------------
  beta_cf <- beta_hat
  beta_cf[idx] <- 0
  p1 <- plogis(drop(Xmat %*% beta_cf))

  ## ----------  PANEL A: original Δp × q ----------------------
  pcp_referral_means <- dat %>%
    filter(referral == 1) %>%
    group_by(doctor) %>%
    summarise(mean_spec_qual = mean(spec_qual, na.rm = TRUE)) %>%
    ungroup()

  # Then, join this back to your main 'dat' dataframe
  dat_rel <- dat %>%
    left_join(pcp_referral_means, by = "doctor")
  
  dat_rel$spec_qual_rel = dat_rel$spec_qual - dat_rel$mean_spec_qual
  delta_A <- (p1 - p0) * dat_rel$spec_qual_rel * 1000

  A_by_hrr <- dat %>% mutate(delta = delta_A) %>%
    group_by(hrr) %>% 
    summarise(gain = mean(delta, na.rm=TRUE), .groups="drop")

  outA <- tibble(
    Variable = v,
    Panel    = "A. Relative to mean specialist",
    Markets  = nrow(A_by_hrr),
    !!!stat_vec(A_by_hrr$gain)
  )

  ## ----------  PANEL B: deviation from 75th-pct specialist --
  ## create q* (75th pct) by hrr, then quality shortfall
  q75_tbl <- dat %>%
    group_by(hrr) %>%
    summarise(q75 = quantile(spec_qual, .75, na.rm=TRUE), .groups="drop")

  dat_q <- dat %>% left_join(q75_tbl, by="hrr")

#  short0 <- (1 - dat_q$q75) - p0 * (1 - dat_q$spec_qual)
#  short1 <- (1 - dat_q$q75) - p1 * (1 - dat_q$spec_qual)
#  delta_B <- (p0 - p1)*( (short0 - short1) ) * 1000
   delta_B <- (p1 - p0)*( dat_q$spec_qual - dat_q$q75 ) * 1000

  B_by_hrr <- dat_q %>%
    mutate(delta = delta_B) %>%
    group_by(hrr) %>%
    summarise(gain = mean(delta, na.rm=TRUE), .groups = "drop")

  outB <- tibble(
    Variable = v,
    Panel    = "B. Relative to 75th-pct specialist",
    Markets  = nrow(B_by_hrr),
    !!!stat_vec(B_by_hrr$gain)
  )

  bind_rows(outA, outB)     

})

q75_tbl <- dat %>%
  group_by(hrr) %>%
  summarise(q75 = quantile(spec_qual, .75, na.rm=TRUE), .groups="drop")

dat_q <- dat %>% left_join(q75_tbl, by="hrr")
#adv0   <- p0 * (1 - dat_q$spec_qual)
#advC   <- (1 - dat_q$q75)
#delta_C <- (advC - adv0) * 1000
delta_C <- p0*(dat_q$spec_qual - dat_q$q75) * 1000

C_by_hrr <- dat |>
  mutate(delta = delta_C) |>
  group_by(hrr) |>
  summarise(gain = mean(delta), .groups = "drop")

panelC <- tibble(
  Variable = "All channels",
  Panel    = "C. Aggregate effect vs 75th-pct benchmark",
  Markets  = nrow(C_by_hrr),
  !!!stat_vec(C_by_hrr$gain)
)

## Formatting & LaTeX export
panel_order <- c("A. Relative to mean specialist",
                 "B. Relative to 75th-pct specialist",
                 "C. Aggregate effect vs 75th-pct benchmark")

welfare_tbl <- bind_rows(panels_AB, panelC) |>
  mutate(
    Variable = ifelse(Variable %in% names(nice_lab),
                      nice_lab[Variable],
                      Variable)
  ) |>
  mutate(across(-c(Variable, Panel, Markets),
                ~ formatC(.x, digits = 2, format = "f"))) |>
  arrange(match(Panel, panel_order),
          match(Variable, c(unname(nice_lab), "All channels")))

library(kableExtra)

n_A <- sum(welfare_tbl$Panel == panel_order[1])
n_B <- sum(welfare_tbl$Panel == panel_order[2])
n_C <- sum(welfare_tbl$Panel == panel_order[3])

latex_welfare <- welfare_tbl |>
  select(-Panel) |>
  kable(format   = "latex",
        booktabs = TRUE,
        align    = "lclrrrrrr",
        col.names = c("Variable","Markets",
                      "Mean","SD","P10","P25","P50","P75","P90")) |>
  kable_styling(latex_options = "hold_position") |>
  pack_rows("Panel A. Relative to mean specialist",     1,   n_A) |>
  pack_rows("Panel B. Relative to 75th-pct specialist",  n_A+1, n_A+n_B) |>
  pack_rows("Panel C. Aggregate effect vs 75th-pct benchmark", n_A+n_B+1, n_A+n_B+n_C)

writeLines(as.character(latex_welfare),
           "results/welfare_summary.tex")