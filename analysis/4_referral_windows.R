# Analysis by referral window to examine variation in referral determinants over time

# First a basic description of referral connections ------------------------------

# ref_windows already holds all *observed* links for movers
# and carries a `window` label: "Year0-0" … "Year0-4"

link_stats_by_win <- ref_windows %>%
  distinct(window, doctor, specialist,          # one row per link
           doc_group, spec_group,
           doc_sex,   spec_sex,
           doc_race,  spec_race,
           dist_miles,
           doc_med_school, spec_med_school,
           doc_grad_year, spec_grad_year) %>%
  group_by(window) %>%
  summarise(
    pct_same_practice = mean(doc_group == spec_group, na.rm = TRUE),
    pct_same_gender   = mean(doc_sex   == spec_sex,   na.rm = TRUE),
    pct_same_race     = mean(doc_race  == spec_race,  na.rm = TRUE),
    pct_same_school   = mean(doc_med_school == spec_med_school, na.rm = TRUE),
    mean_distance     = mean(dist_miles, na.rm = TRUE),
    mean_experience   = mean(abs(doc_grad_year - spec_grad_year), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    across(starts_with("pct_"),
           ~ percent(.x, accuracy = .1)),
    mean_distance   = round(mean_distance, 1),
    mean_experience = round(mean_experience, 1),
    horizon         = as.integer(sub("Up to year ", "", window))
  ) %>%
  select(
    Horizon = horizon,
    `Same practice`          = pct_same_practice,
    `Same gender`            = pct_same_gender,
    `Same race`              = pct_same_race,
    `Same medical school`    = pct_same_school,
    `Mean distance (miles)`  = mean_distance,
    `Mean experience (yrs)`  = mean_experience
  )

print(link_stats_by_win)

link_stats_by_win %>%
  kable(format = "latex", booktabs = TRUE, align = "lrrrrrr") %>%
  kable_styling(latex_options = "hold_position") %>%
  writeLines("results/link_stats_by_window.tex")



# Estimation of Jochman's model by window ----------------------------------------

## helper: label → numeric horizon length
len_from_window <- function(w) as.integer(sub("Up to year ", "", w))

## run the baseline model in every horizon
windows <- sort(unique(df_logit_windows$window))       # Year0-0 … Year0-4

models1 <- map(windows, ~
  feglm(
    referral ~ same_sex + same_race + same_prac + diff_dist + diff_age + diff_gradyear
      | year,                         # year FE (already numeric)
    data  = df_logit_windows %>% filter(window == .x),
    vcov  = "HC1",
    family = binomial(link = "logit")
  )
)

names(models1) <- windows

## marginal effects ⟶ tidy data frame
mfx1 <- imap_dfr(models1, function(mod, win) {
  avg_slopes(mod) %>%
    tidy(conf.int = TRUE) %>%
    mutate(window = win,
           horizon = len_from_window(win))       # numeric x-axis
})

# keep only focal covariates for plotting
plot_vars <- c("same_prac","same_sex","same_race","diff_dist")
mfx_plot  <- mfx1 %>% filter(term %in% plot_vars)

## plot marginal effects vs horizon length
ggplot(mfx_plot,
       aes(x = horizon, y = estimate,
           ymin = conf.low, ymax = conf.high,
           colour = term)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .3) +
  geom_errorbar(width = .15, linewidth = .3) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = 1:5,
                     labels = c("1 yr","2 yr","3 yr","4 yr","5 yr")) +
  labs(
    x = "Length of post-move window",
    y = "Average marginal effect",
    colour = "Covariate",
    title = "How marginal effects evolve as the referral network matures"
  ) +
  theme_minimal(base_size = 11)

ggsave("results/mfx_by_window.png", width = 6.5, height = 4, dpi = 300)
