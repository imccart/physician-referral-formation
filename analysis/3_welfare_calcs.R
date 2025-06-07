# Summarize results in terms of welfare costs -------------------------------------

covars <- c("same_sex","same_prac","diff_dist",
            "same_race","diff_age","diff_gradyear","same_school")

## baseline probabilities and shortfall ----------------------
beta_hat <- as.numeric(coef(logit_twfe4)[covars])

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


writeLines(as.character(welfare_summary), "results/welfare_summary.tex")