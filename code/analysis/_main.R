# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  5/20/2025
## Date Edited:   2/27/2026


# Preliminaries -----------------------------------------------------------
source("code/0-setup.R")


# Shared data -------------------------------------------------------------

## HRR shapefile
gdf <- st_read("data/input/HRR_ShapeFile.shp") %>%
  filter(!str_starts(HRRCITY, "AK") & !str_starts(HRRCITY, "HI"))

## Specialty configuration
specialties <- list(
  ortho    = list(has_qual = TRUE),
  cardioem = list(has_qual = FALSE),
  derm     = list(has_qual = FALSE)
)

## Collectors for cross-specialty comparison
all_mfx <- list()
all_mfx_windows <- list()


# Practice composition by specialty ----------------------------------------
# What share of PCP-containing practice groups also include each specialist type?

mdppas_comp <- map(2009:2018, function(yr) {
  read_csv(sprintf("data/input/MDPPAS/PhysicianData_%d.csv", yr),
           col_types = cols_only(
             npi            = col_number(),
             spec_broad     = col_character(),
             spec_prim_1_name = col_character(),
             group1         = col_character(),
             Year           = col_integer()
           )) %>%
    filter(!is.na(group1) & group1 != "")
}) %>% bind_rows()

# Flag each physician's role
mdppas_comp <- mdppas_comp %>%
  mutate(
    is_pcp   = spec_broad == "1",
    is_ortho = spec_prim_1_name == "Orthopedic Surgery",
    is_cardio = spec_prim_1_name %in% c("Cardiology",
                                         "Clinical Cardiac Electrophysiology"),
    is_derm  = spec_prim_1_name == "Dermatology"
  )

# For each practice group x year, check whether it contains each type
group_comp <- mdppas_comp %>%
  group_by(group1, Year) %>%
  summarise(
    has_pcp   = any(is_pcp),
    has_ortho = any(is_ortho),
    has_cardio = any(is_cardio),
    has_derm  = any(is_derm),
    .groups = "drop"
  )

# Among PCP-containing groups, share that also include each specialty
pcp_groups <- group_comp %>% filter(has_pcp)

practice_composition <- tibble(
  specialty = c("Orthopedic Surgery", "Cardiology", "Dermatology"),
  pct_of_pcp_groups = c(
    mean(pcp_groups$has_ortho),
    mean(pcp_groups$has_cardio),
    mean(pcp_groups$has_derm)
  ),
  n_pcp_groups = nrow(pcp_groups)
)

write_csv(practice_composition, "results/tables/practice_composition.csv")
message("Practice composition: ",
        sprintf("Ortho %.1f%%, Cardio %.1f%%, Derm %.1f%% of %s PCP groups",
                practice_composition$pct_of_pcp_groups[1] * 100,
                practice_composition$pct_of_pcp_groups[2] * 100,
                practice_composition$pct_of_pcp_groups[3] * 100,
                formatC(practice_composition$n_pcp_groups[1], big.mark = ",")))

rm(mdppas_comp, group_comp, pcp_groups)
gc()


# Per-specialty analysis ---------------------------------------------------

for (current_specialty in names(specialties)) {
  cfg <- specialties[[current_specialty]]
  message("\n=== Analyzing: ", current_specialty, " ===\n")

  ## Referral data for all PCP/specialist pairs
  df_full_referrals <- read_csv(sprintf("data/output/df_full_referrals_%s.csv", current_specialty)) %>%
    filter(doc_hrr == spec_hrr)

  ## Referral data for movers only
  df_initial_referrals <- read_csv(sprintf("data/output/df_initial_referrals_%s.csv", current_specialty))

  ## Referral "choice" data for standard logit
  df_logit <- read_csv(sprintf("data/output/df_logit_movers_%s.csv", current_specialty)) %>%
    mutate(doc_male = as.numeric(doc_sex == "M"), spec_male = as.numeric(spec_sex == "M"),
           exp_spec = (Year - spec_grad_year) / 10,
           doctor = as.factor(doctor),
           specialist = as.factor(specialist))

  ## Referral choice data for Jochmans (2018) logit
  df_logit_twfe <- read_csv(sprintf("data/output/df_jochmans_%s.csv", current_specialty))

  ## Referrals by window
  ref_windows <- read_csv(sprintf("data/output/df_initial_referrals_cuml_%s.csv", current_specialty))

  ## Choice data for Jochmans (2018) by window
  df_logit_windows <- read_csv(sprintf("data/output/df_jochmans_windows_%s.csv", current_specialty))

  ## Standard choice-set data by window (for two-stage per-window estimation)
  base_cols <- c("Year", "doctor", "specialist", "referral",
                 "same_sex", "same_race", "same_prac", "diff_dist",
                 "diff_age", "diff_gradyear", "doc_hrr", "window")
  if (cfg$has_qual) base_cols <- c(base_cols, "spec_qual")

  df_choiceset_windows <- read_csv(
    sprintf("data/output/df_logit_windows_%s.csv", current_specialty),
    col_select = all_of(base_cols)
  ) %>%
    mutate(doctor     = as.factor(doctor),
           specialist = as.factor(specialist))

  # Minor cleanup
  movers <- df_initial_referrals %>%
    distinct(doctor, .keep_all = TRUE) %>%
    select(doctor, origin, destination)

  # Run analysis scripts
  source("code/analysis/1_descriptive_stats.R")
  source("code/analysis/2_logit_twfe.R")
  source("code/analysis/3_referral_windows.R")

  # Appendix scripts
  source("code/analysis/app_quad_comparison.R")
  source("code/analysis/app_link_decomposition.R")
  source("code/analysis/app_robustness_noprac.R")
  source("code/analysis/app_period_windows.R")
  if (cfg$has_qual) source("code/analysis/app_welfare.R")
  source("code/analysis/app_convergence.R")

  # Capture MFX for cross-specialty comparison
  all_mfx[[current_specialty]] <- mfx3 %>% mutate(specialty = current_specialty)
  all_mfx_windows[[current_specialty]] <- mfx_window %>% mutate(specialty = current_specialty)

  message("=== Done: ", current_specialty, " ===\n")
}


# Cross-specialty comparison -----------------------------------------------

source("code/analysis/4_cross_specialty.R")
