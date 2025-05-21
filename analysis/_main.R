# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  5/20/2025
## Date Edited:   5/20/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, sf, spdep, ggplot2, modelsummary, fixest, marginaleffects, knitr, kableExtra)

## Import full set of referrals/potential referrals
df_full_referrals  <- read_csv("data/output/df_full_referrals.csv") %>%
    rename(doctor=Practice_ID, specialist=Specialist_ID)

## Import full set of referrals/potential referrals (movers only)
df_logit_cond <- read_csv("data/output/movers_referrals.csv") %>%
    mutate(male_doc=(sex_doc=="M"), male_spec=(sex_spec=="M"), 
           exp_spec=(year-grad_year_spec)/10)

## Import final data for twfe logit analysis
df_logit_twfe <- read_csv("data/output/df_logit.csv")
