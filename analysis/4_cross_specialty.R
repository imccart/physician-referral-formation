# Cross-specialty comparison figures ----------------------------------------
# Combines MFX collected across the specialty loop in _main.R:
#   all_mfx         — full-model MFX (spec 3) per specialty
#   all_mfx_windows — per-window MFX per specialty


# 1. MFX forest plot -------------------------------------------------------
# Full-model MFX (6 covariates) compared across specialties

df_mfx_all <- bind_rows(all_mfx)

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
  geom_errorbarh(position = dodge, height = 0.2, linewidth = 0.6) +
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
# Per-window MFX faceted by covariate, overlaid across specialties

df_mfx_win_all <- bind_rows(all_mfx_windows)

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
  geom_linerange(position = dodge_w, linewidth = 0.6, alpha = 0.5) +
  geom_point(position = dodge_w, size = 2) +
  facet_wrap(~ covar, scales = "free_y", ncol = 3) +
  scale_colour_manual(values = c("Orthopedic surgery" = "#1b9e77",
                                 "Cardiology (E&M)"   = "#d95f02",
                                 "Dermatology"        = "#7570b3"),
                      name = "Specialty") +
  scale_shape_manual(values = c("Orthopedic surgery" = 16,
                                "Cardiology (E&M)"   = 17,
                                "Dermatology"        = 15),
                     name = "Specialty") +
  scale_x_continuous(breaks = 1:6, labels = paste(1:6, "yr")) +
  labs(x = "Years since move", y = "Average marginal effect") +
  theme_minimal(base_size = 10) +
  theme(legend.position = "bottom",
        strip.text = element_text(face = "bold"))

ggsave("results/figures/mfx_by_window_cross.png",
       plot = p_dynamics, width = 8, height = 5.5, dpi = 300)

message("Cross-specialty figures saved.")
