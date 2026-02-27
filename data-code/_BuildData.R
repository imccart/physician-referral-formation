# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  1/23/2025
## Date Edited:   6/5/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, sf, spdep, zipcodeR, geodist)

# Import data -------------------------------------------------------------

## zip and crosswalks
df_hrr <- read_csv("data/input/ZipHsaHrr18.csv") %>%
  mutate(zip = sprintf("%05s", zipcode18)) %>%
  select(zip, hrrnum, hrrcity, hrrstate) %>%
  rename(
    hrr = hrrnum,
    hrrcity = hrrcity,
    hrrstate = hrrstate
  )

## shapefile and contiguity matrix
gdf <- st_read("data/input/HRR_ShapeFile.shp") %>%
  filter(!str_starts(HRRCITY, "AK") & !str_starts(HRRCITY, "HI"))

ggplot(data = gdf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "HRR Shapefile Map")

contig_nb  <- poly2nb(as(gdf, "Spatial"), row.names = gdf$HRRNUM)

neighbor_lookup <- contig_nb |>
  map(~ as.integer(gdf$HRRNUM[.x])) |>          # indices → HRR codes
  set_names(as.character(gdf$HRRNUM))           # names = HRR codes

## MDPPAS
df_mdppas <- data.frame()

for (year in 2009:2018) {
  # Read MDPPAS data
  df <- read_csv(sprintf("data/input/MDPPAS/PhysicianData_%d.csv", year), 
                 col_types = cols_only(
                   npi = col_number(),
                   sex = col_character(),
                   birth_dt = col_character(),
                   spec_prim_1 = col_character(),
                   phy_zip_perf1 = col_character(),
                   phy_zip_pos1 = col_character(),
                   Year = col_integer(),
                   group1 = col_character()
                 )) %>%
    rename(
      npi = npi,
      sex = sex,
      birth = birth_dt,
      spec = spec_prim_1,
      zip_perf = phy_zip_perf1,
      zip_pos = phy_zip_pos1,
      year = Year,
      group = group1
    ) %>%
    mutate (
      zip = ifelse(!is.na(zip_pos), zip_pos, zip_perf),  # Use place of service zip if available
    ) %>%
    # Drop NPI duplicates and missing zip codes
    distinct(npi, .keep_all = TRUE) %>%
    drop_na(zip) %>%
    # Fix zip codes and birth dates
    mutate(
      zip = str_pad(zip, 5, pad = "0"),
      birth = str_sub(birth, -4),
      group = as.character(group)
    )
  
  df_mdppas <- bind_rows(df_mdppas, df)
}

df_mdppas  <- as_tibble(df_mdppas)

# physician race data
df_race <- read_csv("data/input/physician-race/final-npi-combined.csv")

# Merge MDPPAS with HRR
df_mdppas <- left_join(df_mdppas, df_hrr, by = "zip")

# Zip Code with Lat/Long
zip_ll <- as_tibble(zipcodeR::zip_code_db) %>%
  select(zip = zipcode, lat, lon = lng)


## Physician Compare Data (only use for graduation year and medical school)
cols_idx  <- c(0, 7, 9, 10, 18, 25, 27) + 1      # → 1, 8, 10, 11, 19, 26, 28
new_cols  <- c("npi", "gender", "med_school", "grad_year",
               "group_pac_id", "zip_code", "hosp_affiliation")

years      <- 2013:2018
pc_files   <- sprintf("data/input/Physician_Compare/%d/%d_Q4.csv", years, years)

df_pc_full <-
  map2_dfr(pc_files, years, \(file, yr) {

    readr::read_csv(
      file,
      col_types = cols(.default = col_character())   # read everything as character
    ) %>%
      select(all_of(cols_idx)) %>%                   # keep only the desired columns
      set_names(new_cols) %>%                        # rename
      mutate(
        year     = yr,
        zip_code = substr(zip_code, 1, 5)            # 5-digit ZIP
      )
  })


df_pc_full <- df_pc_full %>%
  distinct() %>%                                     # drop exact duplicate rows
  mutate(across(everything(),
                ~ na_if(trimws(.x), ""))) %>%        # blank → NA
  mutate(
    year      = as.integer(year),
    grad_year = as.numeric(grad_year),
    npi       = as.numeric(npi)
  )


df_phycompare <- df_pc_full %>% 
  distinct(npi) %>%
  left_join(df_pc_full %>% filter(!is.na(grad_year)) %>%
            arrange(year) %>% group_by(npi) %>% slice_tail(n=1) %>%
            select(npi, grad_year) %>% ungroup(), by="npi") %>%
  left_join(df_pc_full %>% filter(!is.na(med_school) & med_school!="OTHER") %>%
            arrange(year) %>% group_by(npi) %>% slice_tail(n=1) %>%
            select(npi, med_school) %>% ungroup(), by="npi") %>%
  ungroup() %>%
  select(npi, grad_year, med_school)


# Specialty configuration -----------------------------------------------

specialties <- list(
  ortho    = list(file = "data/input/referrals/REFERRALPAIRS_LARGE_ORTHO.csv",    has_qual = TRUE),
  cardioem = list(file = "data/input/referrals/REFERRALPAIRS_LARGE_CARDIOEM.csv", has_qual = FALSE),
  derm     = list(file = "data/input/referrals/REFERRALPAIRS_LARGE_DERM.csv",     has_qual = FALSE)
)

# Construct datasets for analysis (per specialty) ----------------------

for (current_specialty in names(specialties)) {
  cfg <- specialties[[current_specialty]]
  message("=== Building data for: ", current_specialty, " ===")

  df_referrals <- read_csv(cfg$file)

  if (cfg$has_qual) {
    spec_quality <- df_referrals %>%
      group_by(Specialist_ID) %>%
      slice(1) %>%
      select(specialist = Specialist_ID, spec_qual, total_spec_patients)
  } else {
    spec_quality <- tibble(specialist = numeric(), spec_qual = numeric(), total_spec_patients = numeric())
  }

  source("data-code/1_referrals_full.R")
  source("data-code/2_referrals_initial.R")
  source("data-code/3_logit.R")
  source("data-code/4_logit_jochmans.R")
  source("data-code/5_referrals_by_time.R")

  message("=== Done: ", current_specialty, " ===\n")
}

