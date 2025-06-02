# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  5/20/2025
## Date Edited:   5/29/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, sf, spdep, ggplot2, modelsummary, fixest, marginaleffects, knitr, kableExtra, scales)


# Import data -------------------------------------------------------------

## Referral data for all PCP/specialist pairs
df_full_referrals  <- read_csv("data/output/df_full_referrals.csv") %>%
    filter(doc_hrr==spec_hrr)

## Movers and their referral pairs
df_movers <- read_csv("data/output/df_movers.csv") 

## Referral data for movers only
df_initial_referrals <- read_csv("data/output/df_initial_referrals.csv") %>% 
    filter(doc_hrr==spec_hrr)

## Referral "choice" data for standard logit
df_logit <- read_csv("data/output/df_logit.csv") %>%
    filter(doc_hrr==spec_hrr) %>%
    mutate(doc_male=(doc_sex=="M"), spec_male=(spec_sex=="M"), 
           exp_spec=(year-spec_grad_year)/10)

## Referral choice data for Jochmans (2018) logit
df_logit_twfe <- read_csv("data/output/df_logit_jochmans.csv") %>%
    filter(doc_hrr==spec_hrr)

## HRR shapefile
gdf <- st_read("data/input/HRR_ShapeFile.shp") %>%
  filter(!str_starts(HRRCITY, "AK") & !str_starts(HRRCITY, "HI"))


# Minor cleanup -----------------------------------------------------------

movers <- df_movers %>%
    group_by(doctor) %>%
    slice(1) %>%
    select(doctor, origin, destination)
