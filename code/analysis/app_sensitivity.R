
# app_sensitivity.R -- Oster (2019) selection-on-unobservables bound for same_prac.
# Short spec (same_prac alone) vs. long spec (full spec 4), both on the quadruple
# data; delta* is the ratio of unobserved to observed selection needed to drive
# same_prac to zero.

if (!exists("df_logit_twfe")) {
  current_specialty <- "ortho"
  df_logit_twfe <- read_csv("data/output/df_jochmans_ortho.csv", show_col_types = FALSE)
}
if (!exists("current_specialty")) current_specialty <- "ortho"

mod_short <- feglm(
  referral ~ same_prac | year,
  data = df_logit_twfe, vcov = ~hrr, family = binomial("logit")
)

mod_long <- feglm(
  referral ~ same_sex + same_prac + diff_dist + same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe, vcov = ~hrr, family = binomial("logit")
)

beta_short <- coef(mod_short)["same_prac"]
beta_long  <- coef(mod_long)["same_prac"]
r2_short   <- r2(mod_short, "pr2")
r2_long    <- r2(mod_long, "pr2")

oster_delta <- function(r_max) {
  (beta_long * (r2_long - r2_short)) / ((beta_short - beta_long) * (r_max - r2_long))
}

oster_bound <- tibble(
  specialty = current_specialty,
  beta_short = unname(beta_short),
  beta_long = unname(beta_long),
  r2_short = r2_short,
  r2_long = r2_long,
  delta_star_rmax1 = oster_delta(1),
  delta_star_rmax13 = oster_delta(1.3 * r2_long)
)

write_csv(oster_bound, sprintf("results/tables/oster_bound_%s.csv", current_specialty))
