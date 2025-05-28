# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  1/23/2025
## Date Edited:   5/28/2025


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

# Referral data
df_referrals <- read_csv("data/input/referrals/ReferralPairs.csv")

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

df_phycompare <-
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


df_phycompare <- df_phycompare %>%
  distinct() %>%                                     # drop exact duplicate rows
  mutate(across(everything(),
                ~ na_if(trimws(.x), ""))) %>%        # blank → NA
  mutate(
    year      = as.integer(year),
    grad_year = as.numeric(grad_year),
    npi       = as.numeric(npi)
  )


df_phycompare <- df_phycompare %>%          # object from the previous step
  arrange(year) %>%                         # ensure earlier years come first
  group_by(npi) %>%                         
  slice_tail(n = 1) %>%                     
  ungroup() %>%
  mutate(
    med_school = na_if(med_school, "OTHER")
  ) %>%
  select(npi, grad_year, med_school)


# Construct datasets for analysis -------------------------------------

source("data-code/1_referrals_full.R")  # full referral data set
source("data-code/2_referrals_initial.R")  # referral data set among movers
source("data-code/3_logit.R")  # logistic regression data set
source("data-code/4_logit_jochmans.R")  # logistic regression data for TWFE (Jochmans, 2018)

