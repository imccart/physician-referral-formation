## Descriptive Statistics


# Group by 'year' and 'hrr_doc' and count unique physicians
hrr_physicians <- df_ref_initial %>%
  group_by(year, hrr_doc) %>%
  summarise(unique_doctors = n_distinct(doctor), .groups = "drop")

# Filter and visualize the number of HRRs per physician count
hrr_distribution <- hrr_physicians %>%
  filter(unique_doctors > 1) %>%
  count(unique_doctors)

# Bar plot for the distribution
ggplot(hrr_distribution, aes(x = as.factor(unique_doctors), y = n)) +
  geom_bar(stat = "identity") +
  labs(
    title = "Number of HRRs per number of physicians",
    x = "Number of Physicians",
    y = "Number of HRRs"
  ) +
  theme_minimal()

# Add visualization
ggsave("hrr_distribution_plot.png")

# Creating additional features
df_ref_initial <- df_ref_initial %>%
  mutate(
    male_spec = ifelse(sex_spec == "M", 1, 0),
    male_doc = ifelse(sex_doc == "M", 1, 0),
    exp_spec = year - grad_year_spec
  )

# Example for grouping and aggregation (replace with actual logic as needed)
agg_data <- df_ref_initial %>%
  group_by(year) %>%
  summarise(
    total_referrals = sum(referral, na.rm = TRUE),
    avg_diff_gradyear = mean(diff_gradyear, na.rm = TRUE)
  )

# Export processed data
write_csv(df_ref_initial, "processed_data.csv")
write_csv(agg_data, "aggregated_data.csv")

# Placeholder for MD-PPAS data processing (if applicable)
# mdppas <- read_csv("path/to/PhysicianData_2012.csv")
# mdppas <- mdppas %>%
#   select(npi, sex, birth_dt) %>%
#   mutate(phy_zip1 = str_pad(phy_zip1, 5, pad = "0")) %>%
#   drop_na()

# Uncomment to show top 20 specialties (if the data includes 'spec')
# top_specialties <- mdppas %>%
#   count(spec, sort = TRUE) %>%
#   mutate(percentage = n / sum(n) * 100) %>%
#   slice_max(n = 20, order_by = n)
# print(top_specialties)







## Descriptive Statistics for Logit Model --------------------------------


# ──────────────────────────────────────────────────────────
# helper that computes degree, age, gender & race shares
# ──────────────────────────────────────────────────────────
side_stats <- function(df, id_var, partner_var, sex_var, race_var, birth_var) {
  df %>%
    group_by({{ id_var }}) %>%                               # doctor or specialist
    summarise(
      deg  = n_distinct({{ partner_var }}),                 # out- or in-degree
      age  = mean(year - {{ birth_var }}, na.rm = TRUE),    # years since birth
      male = first({{ sex_var }}) == "M",
      race = first({{ race_var }}),
      .groups = "drop"
    ) %>%
    summarise(
      "Network Degree (Mean)" = mean(deg),                                 # network size
      "Network Degree (SD)"   = sd(deg),
      "Network Degree (Median)"  = median(deg),
      "Mean Age" = mean(age, na.rm = TRUE),
      "Percent Male"  = mean(male, na.rm = TRUE),
      "Percent White" = mean(race == "white",    na.rm = TRUE),
      "Percent Black" = mean(race == "black",    na.rm = TRUE),
      "Percent Hispanic"  = mean(race == "hispanic", na.rm = TRUE),
      "Observations"    = n() # number of physicians
    )
}

# ──────────────────────────────────────────────────────────
# Panel A: doctors (senders)
# ──────────────────────────────────────────────────────────
doc_full <- side_stats(df_full_referrals,
                       doctor, specialist,
                       doc_sex, doc_race, doc_birth)

doc_move <- side_stats(df_logit_cond,
                       doctor, specialist,
                       sex_doc, race_rf_doc, birth_doc)

# ──────────────────────────────────────────────────────────
# Panel B: specialists (receivers)
# ──────────────────────────────────────────────────────────
spec_full <- side_stats(df_full_referrals,
                        specialist, doctor,
                        spec_sex, spec_race, spec_birth)

spec_move <- side_stats(df_logit_cond,
                        specialist, doctor,
                        sex_spec, race_rf_spec, birth_spec)

# ──────────────────────────────────────────────────────────
# assemble long table, reshape for LaTeX
# ──────────────────────────────────────────────────────────
table_long <- bind_rows(
  doc_full  %>% mutate(sample = "Full",   panel = "Doctors"),
  doc_move  %>% mutate(sample = "Movers", panel = "Doctors"),
  spec_full %>% mutate(sample = "Full",   panel = "Specialists"),
  spec_move %>% mutate(sample = "Movers", panel = "Specialists")
)

table_tex <- table_long %>%
  pivot_longer(-c(sample, panel), names_to = "stat") %>%
  pivot_wider(names_from = sample, values_from = value) %>%
  arrange(match(panel, c("Doctors","Specialists")), stat)

# ──────────────────────────────────────────────────────────
# write LaTeX file “tab_physicians.tex”
# ──────────────────────────────────────────────────────────

table_tex %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%   # light rounding
  select(-panel) %>%
  kable(format   = "latex", booktabs = TRUE,
        col.names = c("Statistic", "All referrals", "New movers"),
        align     = c("l","r","r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  group_rows("Panel A. Doctors (any outgoing referrals)", 1, 9) %>%
  group_rows("Panel B. Specialists (any incoming referrals)", 10, 18) %>%
  writeLines("results/desc.tex")


# ------------------------------------------------------------------
# -- Figure 1: distribution of network size ------------------------
# ------------------------------------------------------------------
deg_plot <- deg_all %>%
  mutate(deg_bin = if_else(deg > 20, ">20", as.character(deg))) %>%
  mutate(deg_bin = factor(deg_bin,
                          levels = c(as.character(0:20), ">20"))) %>%
  count(side, deg_bin)

p <- ggplot(deg_plot, aes(deg_bin, n)) +
  geom_col(fill = "grey60") +
  facet_wrap(~ side, ncol = 2) +             # default scales = "fixed"
  scale_y_continuous(labels = comma) +           # ← comma-format  
  labs(x = "Number of distinct network partners",
       y = "Count of physicians") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("results/fig_degree.png",
       plot   = p,
       width  = 6,       # inches
       height = 3.5,     # inches
       dpi    = 300)     # crisp PNG output
