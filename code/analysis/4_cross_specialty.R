# Cross-specialty comparison figures ----------------------------------------
# Reads per-specialty MFX CSVs saved by run_specialty() in _main.R

specs <- c("ortho", "cardioem", "derm")

# 1. MFX forest plot -------------------------------------------------------
# Full-model MFX (6 covariates) compared across specialties

df_mfx_all <- map(specs, function(s) {
  read_csv(sprintf("results/tables/mfx_%s.csv", s), show_col_types = FALSE)
}) %>% bind_rows()

coef_labels <- c(
  same_sex      = "Same gender",
  same_prac     = "Same practice group",
  same_race     = "Same race",
  diff_dist     = "Distance (+5 mi)",
  diff_age      = "Age difference",
  diff_gradyear = "Experience difference"
)

spec_labels <- c(
  ortho    = "Orthopedic surgery",
  cardioem = "Cardiology (E&M)",
  derm     = "Dermatology"
)

df_forest <- df_mfx_all %>%
  filter(term %in% names(coef_labels)) %>%
  mutate(
    label     = coef_labels[term],
    label     = factor(label, levels = rev(coef_labels)),
    spec_name = spec_labels[specialty],
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

dodge <- position_dodge(width = 0.5)

p_forest <- ggplot(df_forest,
       aes(x = estimate, y = label,
           xmin = conf.low, xmax = conf.high,
           colour = spec_name, shape = spec_name)) +
  geom_vline(xintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_errorbar(position = dodge, width = 0.2, linewidth = 0.6, orientation = "y") +
  geom_point(position = dodge, size = 2.5) +
  scale_colour_manual(values = c("Orthopedic surgery" = "#1b9e77",
                                 "Cardiology (E&M)"   = "#d95f02",
                                 "Dermatology"        = "#7570b3"),
                      name = "Specialty") +
  scale_shape_manual(values = c("Orthopedic surgery" = 16,
                                "Cardiology (E&M)"   = 17,
                                "Dermatology"        = 15),
                     name = "Specialty") +
  labs(x = "Average marginal effect", y = NULL) +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom")

ggsave("results/figures/mfx_cross_specialty.png",
       plot = p_forest, width = 7, height = 4.5, dpi = 300)


# 2. Dynamics comparison figure --------------------------------------------
# Year-by-year (non-cumulative) MFX faceted by covariate, overlaid across specialties

df_mfx_win_all <- map(specs, function(s) {
  read_csv(sprintf("results/tables/mfx_period_%s.csv", s), show_col_types = FALSE)
}) %>% bind_rows()

horizon_from_label <- function(label) {
  as.integer(str_extract(label, "\\d+"))
}

plot_vars <- c("same_prac", "same_sex", "same_race",
               "diff_dist", "diff_age", "diff_gradyear")

nice_lab <- c(
  same_prac     = "Same practice",
  same_sex      = "Same gender",
  same_race     = "Same race",
  diff_dist     = "Distance (+5 mi)",
  diff_age      = "Age gap",
  diff_gradyear = "Experience gap"
)

df_win_plot <- df_mfx_win_all %>%
  filter(term %in% plot_vars) %>%
  mutate(
    horizon   = horizon_from_label(as.character(model)),
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error,
    covar     = nice_lab[term],
    covar     = factor(covar, levels = nice_lab),
    spec_name = spec_labels[specialty]
  )

dodge_w <- position_dodge(width = 0.3)

p_dynamics <- ggplot(df_win_plot,
       aes(x = horizon, y = estimate,
           ymin = conf.low, ymax = conf.high,
           colour = spec_name, shape = spec_name)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = 0.3) +
  geom_linerange(position = dodge_w, linewidth = 0.5, alpha = 0.6) +
  geom_point(position = dodge_w, size = 1.5) +
  facet_wrap(~ covar, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("Orthopedic surgery" = "#1b9e77",
                                 "Cardiology (E&M)"   = "#d95f02",
                                 "Dermatology"        = "#7570b3"),
                      name = "Specialty") +
  scale_shape_manual(values = c("Orthopedic surgery" = 16,
                                "Cardiology (E&M)"   = 17,
                                "Dermatology"        = 15),
                     name = "Specialty") +
  scale_x_continuous(breaks = 1:4, labels = paste(1:4, "yr")) +
  labs(x = "Years since move", y = "Average marginal effect") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("results/figures/mfx_by_window_cross.png",
       plot = p_dynamics, width = 8, height = 5.5, dpi = 300)


# 3. Cross-specialty dynamics test -----------------------------------------
# Tests whether the same-practice attenuation from year 1 to year 4 differs
# across specialties. Per-specialty samples are disjoint, so the cross-specialty
# estimates are independent and Var(a - b) = Var(a) + Var(b). The within-specialty
# change ignores the (positive) cross-year covariance, which makes the test
# conservative.

xspec_within <- df_mfx_win_all %>%
  filter(term == "same_prac", model %in% c("Year 1 only", "Year 4 only")) %>%
  mutate(yr = ifelse(model == "Year 1 only", "y1", "y4")) %>%
  select(specialty, yr, estimate, std.error) %>%
  pivot_wider(names_from = yr, values_from = c(estimate, std.error)) %>%
  mutate(delta    = estimate_y4 - estimate_y1,
         se_delta = sqrt(std.error_y4^2 + std.error_y1^2),
         z        = delta / se_delta,
         p        = 2 * pnorm(-abs(z)))

xspec_pairs <- map_dfr(
  list(c("ortho", "cardioem"), c("ortho", "derm"), c("cardioem", "derm")),
  function(pr) {
    a <- xspec_within %>% filter(specialty == pr[1])
    b <- xspec_within %>% filter(specialty == pr[2])
    dd <- a$delta - b$delta
    se <- sqrt(a$se_delta^2 + b$se_delta^2)
    tibble(pair = paste(pr, collapse = "_vs_"),
           diff_of_deltas = dd, se = se, z = dd / se, p = 2 * pnorm(-abs(dd / se)))
  })

write_csv(xspec_within, "results/tables/xspec_dynamics_within.csv")
write_csv(xspec_pairs,  "results/tables/xspec_dynamics_pairs.csv")

message("Cross-specialty figures and dynamics test saved.")
