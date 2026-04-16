# 0-setup.R — activate renv and load packages
options(vsc.rstudioapi = FALSE)
source("renv/activate.R")

library(tidyverse)
library(readr)
library(sf)
library(spdep)
# library(zipcodeR)  # only needed for data build, not analysis
library(geodist)
library(ggplot2)
library(modelsummary)
library(fixest)
library(marginaleffects)
library(knitr)
library(kableExtra)
library(scales)
library(broom)
library(purrr)
library(data.table)

options(modelsummary_factory_default = "kableExtra")
