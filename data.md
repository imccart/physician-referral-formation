# Data

This page describes our data sources and the code used to clean and merge the data.

## Overview

Our data are organized into input and output subfolders. The input folder contains our underlying data sources (symlinked from external storage, gitignored), and the output folder contains the analysis-ready datasets ultimately called in the analysis code. Access to the raw data requires a data use agreement with CMS. Details of our input sources and the code to create our data outputs are described below.

## Data Sources

- **Medicare FFS claims** (100% Part A and B, 2009--2018). Inpatient stays and office-based E&M visits used to construct referral relationships between PCPs and orthopedic specialists, following the attribution approach in [Pham et al. (2009)](https://doi.org/10.1001/archinternmed.2009.46) and [Agha et al. (2019)](https://doi.org/10.1093/restud/rdy051).

- **MD-PPAS** (Medicare Data on Provider Practice and Specialty). Physician practice location, specialty codes, and HRR assignment. Used to identify PCP movers (non-contiguous HRR relocations) and to measure geographic proximity between PCPs and specialists via ZIP centroid distance.

- **Physician Compare**. CMS data on physician demographics including gender, birth year, graduation year, and medical school. First available in 2012 but used for time-invariant characteristics, so the late arrival does not substantially affect our time frame.

- **NPPES** (National Plan and Provider Enumeration System). NPI registry used for physician identification and linkage across data sources.

- **Dartmouth Atlas**. HRR shapefile (`HRR_ShapeFile.shp`) and ZIP-to-HRR crosswalk (`ZipHsaHrr18.csv`). Used to define geographic markets and construct the contiguity matrix for identifying non-neighboring HRR moves.

- **Physician race imputations**. From a companion project (`research-data-repo/physician-race/`). Race is not publicly available for physicians at a national scale, so we impute it by combining three classifiers: NamePrism (name-based probabilities), WRU (Bayesian surname-geocoding), and DeepFace (profile photo analysis from Zocdoc). Predictions are aggregated via a random-forest ensemble achieving 89% accuracy against verified race data from Florida voter files, Texas medical license records, and self-reported Zocdoc profiles. See the supplemental appendix for details.

## Code

The [data-code](/data-code/) folder contains the code used to clean and merge the data. The **_BuildData.R** script is the main orchestrator that sources the other scripts in order:

1. **1_referrals_full.R** -- Builds the full PCP-specialist referral panel from Medicare claims, linked with physician characteristics from MD-PPAS, Physician Compare, and NPPES. Outputs `df_full_referrals.csv`.

2. **2_referrals_initial.R** -- Identifies PCP movers: physicians whose HRR changes between consecutive years, excluding moves to geographically neighboring HRRs (using the Dartmouth Atlas contiguity matrix). Outputs `df_initial_referrals.csv`.

3. **3_logit.R** -- Expands PCP-specialist pairs into binary choice sets (1 = referral exists, 0 = potential but unrealized referral) for conditional logit estimation. Outputs `df_logit_movers.csv` (31 MB, movers only) and `df_logit_all.csv` (12 GB, all physicians).

4. **4_logit_jochmans.R** -- Constructs quartet-structured data for the [Jochmans (2018)](https://doi.org/10.1093/restud/rdx063) conditional logit estimator, which eliminates two-way fixed effects via differencing across discordant doctor-specialist quartets. Outputs `df_jochmans.csv`.

5. **5_referrals_by_time.R** -- Creates datasets stratified by years-since-move windows (1 through 6 years post-relocation) for the dynamics analysis. Outputs `df_jochmans_windows.csv` and `df_logit_windows.csv` (865 MB).
