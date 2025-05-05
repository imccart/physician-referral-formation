# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  1/23/2025
## Date Edited:   1/23/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, sf, spdep)

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

gdf_sp <- as(gdf, "Spatial")
contig_matrix <- poly2nb(gdf_sp, row.names = gdf$hrrnum)
hrr_neighbors <- nb2listw(contig_matrix, style = "B")

## MDPPAS
df_mdppas <- data.frame()

for (year in 2009:2018) {
  # Read MDPPAS data
  df <- read_csv(sprintf("data/input/MDPPAS/PhysicianData_%d.csv", year), 
                 col_types = cols_only(
                   npi = col_character(),
                   sex = col_character(),
                   birth_dt = col_character(),
                   spec_prim_1 = col_character(),
                   phy_zip_perf1 = col_character(),
                   Year = col_integer(),
                   group1 = col_character()
                 )) %>%
    rename(
      npi = npi,
      sex = sex,
      birth = birth_dt,
      spec = spec_prim_1,
      zip = phy_zip_perf1,
      year = Year,
      group = group1
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

# Merge MDPPAS with HRR
df_mdppas <- left_join(df_mdppas, df_hrr, by = "zip")








df_physicians <- read_csv("path/to/physicians.csv") # Update with correct path
df_ref_all <- read_csv("path/to/movers_referrals.csv") # Update with correct path

# Focus on the following variables
df_physicians <- df_physicians %>%
  select(npi, grad_year, med_school) %>%
  distinct(npi, .keep_all = TRUE) %>%
  mutate(med_school = ifelse(med_school == "OTHER", NA, med_school),
         npi = as.integer(npi))

# Merge with Referrals
df_ref_all <- df_ref_all %>%
  left_join(df_physicians, by = c("doctor" = "npi")) %>%
  rename_with(~paste0(.x, "_doc"), ends_with("_x")) %>%
  left_join(df_physicians, by = c("specialist" = "npi")) %>%
  rename_with(~paste0(.x, "_spec"), ends_with("_y"))

# Create additional variables
df_ref_all <- df_ref_all %>%
  mutate(
    same_sex = ifelse(sex_doc == sex_spec, 1, 0),
    diff_age = abs(as.numeric(birth_doc) - as.numeric(birth_spec)),
    diff_gradyear = abs(as.numeric(grad_year_doc) - as.numeric(grad_year_spec)),
    same_school = ifelse(med_school_doc == med_school_spec, 1, 0),
    same_prac = ifelse(group_doc == group_spec, 1, 0),
    same_zip = ifelse(zip_doc == zip_spec, 1, 0)
  )

# Add same sex details
df_ref_all <- df_ref_all %>%
  mutate(
    same_male = ifelse(sex_doc == "M" & same_sex == 1, 1, 0),
    same_female = ifelse(sex_doc == "F" & same_sex == 1, 1, 0)
  )

# Prepare data for logistic regression
zmat <- list()
rmat <- list()

unique_years <- unique(df_ref_all$year)
unique_hrrs <- unique(df_ref_all$hrr_doc)

for (year in unique_years) {
  for (hrr in unique_hrrs) {
    df <- df_ref_all %>%
      filter(year == year & hrr_doc == hrr)

    n <- n_distinct(df$doctor)
    m <- n_distinct(df$specialist)

    rho <- as.integer(n * (n - 1) * m * (m - 1) / 2)

    docs <- unique(df$doctor)
    specs <- unique(df$specialist)

    # Logistic regression data frame preparation
    zz <- numeric(rho)
    rr <- matrix(0, nrow = rho, ncol = 12) # Adjust dimensions

    # Populate zz and rr as per Python logic
    # Logic for filling zz and rr will depend on further specifications
  }
}

# Save final output for logistic regression
write_csv(df_ref_all, "path/to/df_logit.csv") # Update path as needed
