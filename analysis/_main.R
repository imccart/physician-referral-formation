# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  5/20/2025
## Date Edited:   5/20/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, sf, spdep, ggplot2, modelsummary, fixest, marginaleffects, knitr, kableExtra, scales)


# Import data -------------------------------------------------------------
df_full_referrals  <- read_csv("data/output/df_full_referrals.csv") %>%    # All observed pairs
    filter(doc_hrr==spec_hrr)
df_initial_referrals <- read_csv("data/output/df_initial_referrals.csv") %>% # Initial referrals (movers only)
    filter(doc_hrr==spec_hrr)

df_logit <- read_csv("data/output/df_logit.csv") %>%                    # "Standard" logit data
    filter(doc_hrr==spec_hrr) %>%
    mutate(doc_male=(doc_sex=="M"), spec_male=(spec_sex=="M"), 
           exp_spec=(year-spec_grad_year)/10)

df_logit_twfe <- read_csv("data/output/df_logit_jochmans.csv") %>%          # "TWFE" logit data (Jochmans, 2018)
    filter(doc_hrr==spec_hrr)


