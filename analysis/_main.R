# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  5/20/2025
## Date Edited:   6/5/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, sf, spdep, ggplot2, modelsummary, fixest, marginaleffects, knitr, kableExtra, scales, broom, purrr, matrixStats)


# Import data -------------------------------------------------------------

df_pairs <- read_csv("data/input/referrals/ReferralPairs_Large.csv")

# Specialist quality only
spec_quality <- df_pairs %>%
  group_by(Specialist_ID) %>%
  slice(1) %>%
  select(specialist=Specialist_ID, spec_qual, total_spec_patients)  

## Referral data for all PCP/specialist pairs
df_full_referrals  <- read_csv("data/output/df_full_referrals.csv") %>%
    filter(doc_hrr==spec_hrr)

## Referral data for movers only
df_initial_referrals <- read_csv("data/output/df_initial_referrals.csv")

## Referral "choice" data for standard logit
df_logit <- read_csv("data/output/df_logit_movers.csv") %>%
    mutate(doc_male=(doc_sex=="M"), spec_male=(spec_sex=="M"), 
           exp_spec=(Year-spec_grad_year)/10)

## Referral choice data for Jochmans (2018) logit
df_logit_twfe <- read_csv("data/output/df_jochmans.csv")

## HRR shapefile
gdf <- st_read("data/input/HRR_ShapeFile.shp") %>%
  filter(!str_starts(HRRCITY, "AK") & !str_starts(HRRCITY, "HI"))

## Referrals by window
ref_windows <- read_csv("data/output/df_initial_referrals_cuml.csv")

## Choice data for Jochmans (2018) by window
df_logit_windows <- read_csv("data/output/df_jochmans_windows.csv")

# Minor cleanup -----------------------------------------------------------

movers <- df_initial_referrals %>%
    group_by(doctor) %>%
    slice(1) %>%
    select(doctor, origin, destination)

