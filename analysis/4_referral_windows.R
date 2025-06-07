# Analysis by referral window to examine variation in referral determinants over time

## helper: label → numeric horizon length
len_from_window <- function(w) as.integer(sub("Up to year ", "", w))


# First a basic description of referral connections ------------------------------

## cumulative sets  (Year0-0 … Year0-4)
cuml_sets <- ref_windows %>%                  # cumulative windows already
  distinct(window, doctor, specialist) %>%
  mutate(horizon = len_from_window(window)) %>%
  group_by(horizon, doctor) %>%
  summarise(spec_cuml = list(unique(specialist)), .groups = "drop")

## one-year slices  (0, 1, … , 4)
slice_sets <- ref_windows %>%
  distinct(year_slice = years_since_move, doctor, specialist) %>%
  group_by(year_slice, doctor) %>%
  summarise(spec_slice = list(unique(specialist)), .groups = "drop") %>%
  mutate(horizon = year_slice + 1)           # same numeric scale

## merge cumulative and slice info per doctor & horizon
dyn <- full_join(cuml_sets, slice_sets,
                 by = c("horizon", "doctor")) %>%
  arrange(doctor, horizon) %>%
  group_by(doctor) %>%
  mutate(
    prev_cuml   = lag(spec_cuml),
    prev_slice = lag(spec_slice),

    # cumulative network growth
    net_size       = lengths(spec_cuml),
    new_specs      = lengths(map2(spec_cuml, prev_cuml, setdiff)),

    # specialists used in k−1 slice but not used in k slice
    drop_specs     = lengths(map2(prev_slice, spec_slice, setdiff))
  ) %>%
  ungroup() %>%
  group_by(horizon) %>%
  summarise(
    pcp_network_size = mean(net_size,     na.rm = TRUE),
    new_specialists  = mean(new_specs,    na.rm = TRUE),
    drop_specialists = mean(drop_specs,   na.rm = TRUE),
    .groups = "drop"
  )

## concordance & distance (still cumulative windows)
link_stats <- ref_windows %>%
  distinct(window, doctor, specialist,
           doc_group, spec_group,
           doc_sex,   spec_sex,
           doc_race,  spec_race,
           dist_miles,
           doc_med_school, spec_med_school,
           doc_grad_year, spec_grad_year) %>%
  mutate(horizon = len_from_window(window)) %>%
  group_by(horizon) %>%
  summarise(
    pct_same_practice = mean(doc_group == spec_group, na.rm = TRUE),
    pct_same_gender   = mean(doc_sex   == spec_sex,   na.rm = TRUE),
    pct_same_race     = mean(doc_race  == spec_race,  na.rm = TRUE),
    pct_same_school   = mean(doc_med_school == spec_med_school, na.rm = TRUE),
    mean_distance     = mean(dist_miles, na.rm = TRUE),
    mean_experience   = mean(abs(doc_grad_year - spec_grad_year), na.rm = TRUE),
    .groups = "drop"
  )

## summary table
summary_by_win <- link_stats %>%
  left_join(dyn, by = "horizon") %>%
  arrange(horizon) %>%
  mutate(
    across(starts_with("pct_"),
           ~ percent(.x, accuracy = .1)),
    mean_distance     = round(mean_distance,     1),
    mean_experience   = round(mean_experience,   1),
    pcp_network_size  = round(pcp_network_size,  1),
    new_specialists   = round(new_specialists,   1),
    drop_specialists  = round(drop_specialists,  1)
  ) %>%
  select(
    `Years since move`      = horizon,
    `Same practice`         = pct_same_practice,
    `Same gender`           = pct_same_gender,
    `Same race`             = pct_same_race,
    `Same medical school`   = pct_same_school,
    `Mean distance (miles)` = mean_distance,
    `Mean experience (yrs)` = mean_experience,
    `PCP network size`      = pcp_network_size,
    `New specialists`       = new_specialists,
    `Dropped specialists`   = drop_specialists
  )

print(summary_by_win)

tbl_long <- summary_by_win %>%
  mutate(across(-`Years since move`, as.character)) %>%   # <- key fix
  pivot_longer(-`Years since move`, names_to = "Metric")  # long form

table_tex <- tbl_long %>%
  pivot_wider(names_from = `Years since move`, values_from = value) %>%
  arrange(match(Metric, c("Same practice",
                          "Same gender",
                          "Same race",
                          "Same medical school",
                          "Mean distance (miles)",
                          "Mean experience (yrs)",
                          "PCP network size",
                          "New specialists",
                          "Dropped specialists")))

## 2.  LaTeX export
col_hdr <- c("Statistic", paste("Year", 1:5))

table_tex %>%
  kable(format = "latex",
        booktabs = TRUE,
        align = c("l", rep("r", 5)),
        col.names = col_hdr) %>%
  kable_styling(latex_options = "hold_position") %>%
  writeLines("results/link_stats_by_window.tex")


# Estimation of Jochman's model by window ----------------------------------------

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
plot_vars <- c("same_prac","same_sex","same_race","diff_dist","diff_age","diff_gradyear")
mfx_plot  <- mfx1 %>% filter(term %in% plot_vars)

base_shapes <- c(21, 22, 24, 23, 25, 7, 8, 3)           # easily distinguishable
shape_vec   <- setNames(base_shapes[seq_along(plot_vars)],
                        plot_vars)
nice_lab <- function(v)
  stringr::str_replace_all(v,
    c("^same_prac$"  = "Same practice",
      "^same_sex$"   = "Same gender",
      "^same_race$"  = "Same race",
      "^diff_dist$"  = "Distance (miles)",
      "^diff_age$"   = "Age gap",
      "^diff_gradyear$" = "Experience gap"))
shape_labs <- sapply(plot_vars, nice_lab, USE.NAMES = FALSE)

## plot marginal effects vs horizon length
dodge <- position_dodge(width = .30)

ggplot(mfx_plot,
       aes(x = horizon, y = estimate,
           ymin = conf.low, ymax = conf.high,
           group = term, shape = term)) +

  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .3) +

  ## CIs
  geom_linerange(position = dodge,
                 colour = "grey50", alpha = .35, linewidth = .9) +

  ## point estimates
  geom_point(position = dodge,
             size = 3, colour = "black", fill = "white") +

  scale_shape_manual(values = shape_vec,
                     breaks = plot_vars,
                     labels = shape_labs,
                     name   = "Covariate") +

  scale_x_continuous(breaks = 1:6,
                     labels = paste(1:6, "yr")) +

  labs(x = "Years since move",
       y = "Average marginal effect") +

  theme_minimal(base_size = 11) +
  theme(
    legend.position.inside = c(1, 1),     # inside, top-right
    legend.justification  = c(1, 1),
    legend.box.background = element_rect(colour = "grey70"),
    legend.key            = element_blank()
  )

ggsave("results/mfx_by_window.png", width = 6.5, height = 4, dpi = 300)