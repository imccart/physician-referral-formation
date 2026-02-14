## Descriptive Statistics

# Overall sample size and relevant counts ----------------------------
df_full_referrals %>% group_by(doctor, specialist) %>% slice(1) %>% ungroup() %>%
  summarise(
    n_physicians = n_distinct(doctor),
    n_specialists = n_distinct(specialist),
    n_pairs = n()
  )

## Total patients
df_full_referrals %>%
  group_by(specialist) %>% slice(1) %>% ungroup() %>%
  summarise(total_patients = sum(total_spec_patients))


df_initial_referrals %>% group_by(doctor, specialist) %>% slice(1) %>% ungroup() %>%
  summarise(
    n_physicians = n_distinct(doctor),
    n_specialists = n_distinct(specialist),
    n_pairs = n()
  )

## Total patients
df_initial_referrals %>%
  group_by(specialist) %>% slice(1) %>% ungroup() %>%
  summarise(total_patients = sum(total_spec_patients))



# Map of movers ----------------------------------------------

if (is.na(st_crs(gdf))) {
  st_crs(gdf) <- 4269      # NAD83 (EPSG:4269); change if your file uses another CRS
}

hrr_centroids <- gdf %>%
  st_point_on_surface() %>%
  st_coordinates() %>%
  as_tibble() %>%
  bind_cols(hrr = gdf$HRRNUM) |>
  rename(lon = X, lat = Y)


flows <- movers %>% filter(!is.na(origin) & !is.na(destination)) %>%
  group_by(origin, destination) %>%
  summarise(n_movers = n_distinct(doctor), .groups = "drop")

top_origins <- flows %>%
  group_by(origin) %>%
  summarise(total_movers = sum(n_movers), .groups = "drop") %>%
  slice_max(total_movers, n = 20) %>%
  pull(origin)

flows_top <- flows %>%
  filter(origin %in% top_origins) %>%
  group_by(origin) %>%
  slice_max(n_movers, n = 10, with_ties = FALSE) %>%
  ungroup() %>%
  left_join(hrr_centroids,  by = c("origin"      = "hrr")) %>%
  rename(lon_o = lon, lat_o = lat) %>%
  left_join(hrr_centroids,  by = c("destination" = "hrr")) %>%
  rename(lon_d = lon, lat_d = lat) %>%
  filter(!is.na(lon_o) & !is.na(lon_d)) %>%
  mutate(curv_up = lon_o < lon_d)


mover_plot <- ggplot() +
  geom_sf(data = gdf, fill = "grey95", colour = "black", linewidth = 0.1) +
  ## upward-arching curves
  geom_curve(data = filter(flows_top,  curv_up),
             aes(x = lon_o, y = lat_o, xend = lon_d, yend = lat_d),
             curvature =  0.5,
             colour = "grey20",
             linewidth=0.2,
             arrow  = arrow(length = unit(0.08, "cm"), type = "closed"),
             alpha  = 0.8) +
  ## downward-arching curves
  geom_curve(data = filter(flows_top, !curv_up),
             aes(x = lon_o, y = lat_o, xend = lon_d, yend = lat_d),
             curvature = -0.3,
             colour = "grey20",
             linewidth=0.2,
             arrow  = arrow(length = unit(0.08, "cm"), type = "closed"),
             alpha  = 0.8) +
  coord_sf(crs = st_crs(gdf)) +
  theme_void() +
  theme(legend.position = "right")

ggsave("results/figures/fig_movers.png",
       plot   = mover_plot,
       width  = 6,       # inches
       height = 4,       # inches
       dpi    = 300)     # crisp PNG output

flows %>% summarize(total_movers = sum(n_movers)) # total number of movers
flows_top %>% summarize(total_movers = sum(n_movers)) # total number of movers


# Descriptive Statistics  --------------------------------


## helper that computes degree, age, gender & race shares
side_stats <- function(df, id_var, partner_var, sex_var, race_var, birth_var, dist_var) {
  df %>%
    group_by({{ id_var }}, Year) %>%                               # doctor or specialist
    summarise(
      deg  = n_distinct({{ partner_var }}),                 # out- or in-degree
      age  = mean(Year - {{ birth_var }}, na.rm = TRUE),    # years since birth
      male = first({{ sex_var }}) == "M",
      race = first({{ race_var }}),
      distance = mean({{ dist_var }}, na.rm = TRUE), # average distance to partner
      .groups = "drop"
    ) %>%
    summarise(
      "Network Degree (Mean)" = mean(deg),                                 # network size
      "Network Degree (SD)"   = sd(deg),
      "Network Degree (Median)"  = median(deg),
      "Mean Age" = mean(age, na.rm = TRUE),
      "Mean Distance (mi)" = mean(distance, na.rm = TRUE),
      "Percent Male"  = mean(male, na.rm = TRUE),
      "Percent White" = mean(race == "white",    na.rm = TRUE),
      "Percent Black" = mean(race == "black",    na.rm = TRUE),
      "Percent Hispanic"  = mean(race == "hispanic", na.rm = TRUE),
      "Observations"    = n_distinct({{ id_var }}) # number of distinct physicians
    )
}


## Panel A: doctors (senders)
doc_full <- side_stats(df_full_referrals,
                       doctor, specialist,
                       doc_sex, doc_race, doc_birth, dist_miles)

doc_move <- side_stats(df_initial_referrals,
                       doctor, specialist,
                       doc_sex, doc_race, doc_birth, dist_miles)

## Panel B: specialists (receivers)
spec_full <- side_stats(df_full_referrals,
                        specialist, doctor,
                        spec_sex, spec_race, spec_birth, dist_miles)

spec_move <- side_stats(df_initial_referrals,
                        specialist, doctor,
                        spec_sex, spec_race, spec_birth, dist_miles)


## assemble long table, reshape for LaTeX
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

table_tex %>%
  mutate(across(where(is.double), ~ round(.x, 3))) %>%   # light rounding
  select(-panel) %>%
  kable(format   = "latex", booktabs = TRUE, linesep = "",
        col.names = c(" ", "All referrals", "PCP movers"),
        align     = c("l","r","r")) %>%
  kable_styling(latex_options = "hold_position") %>%
  group_rows("Panel A. Doctors (any outgoing referrals)", 1, 10) %>%
  group_rows("Panel B. Specialists (any incoming referrals)", 11, 20) %>%
  writeLines("results/tables/desc.tex")


## Distribution of network size
deg_out <- df_full_referrals %>%
  group_by(doctor, Year) %>%
  summarise(deg = n_distinct(specialist), .groups = "drop") %>%
  mutate(side = "PCP (out-degree)")

## receiver-side degree: how many distinct PCPs refer to each specialist
deg_in  <- df_full_referrals %>%
  group_by(specialist, Year) %>%
  summarise(deg = n_distinct(doctor), .groups = "drop") %>%
  mutate(side = "Specialist (in-degree)")

## put them in one long data frame for facetted plotting
deg_all <- bind_rows(deg_out, deg_in)

deg_plot <- deg_all %>%
  mutate(deg_bin = if_else(deg > 20, ">20", as.character(deg))) %>%
  mutate(deg_bin = factor(deg_bin,
                          levels = c(as.character(0:20), ">20"))) %>%
  dplyr::count(side, deg_bin)

p <- ggplot(deg_plot, aes(deg_bin, n)) +
  geom_col(fill = "grey60") +
  facet_wrap(~ side, ncol = 2, scales="free_y") +             # default scales = "fixed"
  scale_y_continuous(labels = comma) +           # ← comma-format  
  labs(x = "Number of distinct network partners",
       y = "Count of physicians") +
  theme_minimal(base_size = 12) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1))

ggsave("results/figures/fig_degree.png",
       plot   = p,
       width  = 6,       # inches
       height = 3.5,     # inches
       dpi    = 300)     # crisp PNG output


## Network density
density_tbl <- df_full_referrals %>%
  distinct(doctor, specialist, Year, hrr = doc_hrr) %>%  # one row per link
  group_by(Year, hrr) %>%
  summarise(
    observed  = n(),                                     # # distinct links
    n_docs    = n_distinct(doctor),
    n_specs   = n_distinct(specialist),
    potential = n_docs * n_specs,                        # all possible pairs
    .groups   = "drop"
  )
network_density <- sum(density_tbl$observed) / sum(density_tbl$potential)

## Describing observed links
link_stats <- df_full_referrals %>%
  distinct(doctor, specialist,        # one row per realised link
           doc_group, spec_group,
           doc_sex,  spec_sex,
           doc_race, spec_race,
           dist_miles,
           doc_med_school, spec_med_school,
           doc_grad_year, spec_grad_year) 

pct_same_practice <- mean(link_stats$doc_group == link_stats$spec_group, na.rm = TRUE)
pct_same_gender   <- mean(link_stats$doc_sex   == link_stats$spec_sex, na.rm = TRUE)
pct_same_race     <- mean(link_stats$doc_race  == link_stats$spec_race, na.rm = TRUE)
mean_gradyear     <- mean(abs(link_stats$doc_grad_year - link_stats$spec_grad_year), na.rm = TRUE)
mean_distance     <- mean(link_stats$dist_miles, na.rm = TRUE)

summary_tbl <- tibble(
  metric  = c("Same practice",
              "Same gender",
              "Same race",
              "Mean distance (miles)",
              "Mean experience gap (years)"),
  value   = c(scales::percent(pct_same_practice, accuracy = .1),
              scales::percent(pct_same_gender,   accuracy = .1),
              scales::percent(pct_same_race,     accuracy = .1),
              round(mean_distance, 1),
              round(mean_gradyear, 1))
)

network_density
summary_tbl

## Describing observed and unobserved links
link_summary <- function(df) {
  df %>%
    summarise(
      "Same practice"    = mean(same_prac,     na.rm = TRUE),
      "Same gender"      = mean(same_sex,      na.rm = TRUE),
      "Same race"        = mean(same_race,     na.rm = TRUE),
      "Distance (miles)" = mean(dist_miles,    na.rm = TRUE),
      "Experience (yr)"  = mean(diff_gradyear, na.rm = TRUE)
    )
}

tab_links <- bind_rows(
  df_logit %>%                    # established links
    filter(referral == 1) %>%
    link_summary() %>%
    mutate(link = "Established links"),
  df_logit %>%                    # non–links in the choice set
    filter(referral == 0) %>%
    link_summary() %>%
    mutate(link = "Non–established links")
) %>%
  pivot_longer(-link, names_to = "stat") %>%
  pivot_wider(names_from = link, values_from = value) %>%
  rename(Statistic = stat) %>%
  mutate(across(`Established links`:`Non–established links`,
                ~ ifelse(Statistic %in% c("Distance (miles)", "Experience (yr)"),
                         round(.x, 1),                       # numeric, 1-dp
                         percent(.x, accuracy = 0.1))))      # percent shares

tab_links %>%
  kable(format   = "latex",
        booktabs = TRUE,
        linesep  = "",
        align    = c("l","r","r"),
        col.names = c("Statistic",
                      "Established \\ links",
                      "Non--established \\ links")) %>%
  kable_styling(latex_options = "hold_position") %>%
  writeLines("results/tables/link_stats.tex")