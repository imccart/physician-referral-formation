# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Research project: "Formation of Physician Referral Networks: Drivers and Potential Consequences." Analyzes how PCPs form referral networks with specialists using Medicare data (2009--2018), focusing on physician movers who relocate across Hospital Referral Regions (HRRs).

Paper and appendix live in `papers/`. Presentations live in `presentations/`. The paper tex files reference `../results/tables/` and `../results/figures/` for all generated output. Bibliography uses `BibTeX_Library.bib` and `aer.bst` (copied from `work/administrative/templates/bibliography/`). Overleaf syncs via GitHub.

## Running the Code

**Data construction** (requires large external datasets via symlinks in `data/input/`):
```r
source("data-code/_BuildData.R")   # orchestrates scripts 1-5, outputs to data/output/
```

**Analysis** (requires processed data in `data/output/`):
```r
source("analysis/_main.R")         # orchestrates scripts 1-5, outputs to results/tables/ and results/figures/
```

There is no Makefile. Scripts are run interactively in R. Each orchestrator (`_BuildData.R`, `_main.R`) sources its numbered subscripts in order.

## Architecture

```
data/input/  (symlinked, gitignored)  →  data-code/_BuildData.R  →  data/output/ (gitignored)
                                                                            ↓
                                          analysis/_main.R  ←───────────────┘
                                                ↓
                                           results/tables/*.tex, results/figures/*.png
                                                ↓
                                           papers/*.tex  (refs ../results/)
```

### Data construction pipeline (`data-code/`)

| Script | Purpose | Key output |
|--------|---------|------------|
| `_BuildData.R` | Imports & merges MDPPAS, Physician Compare, referral pairs, race, geography | Environment objects |
| `1_referrals_full.R` | Builds full PCP-specialist referral dataset | `df_full_referrals.csv` |
| `2_referrals_initial.R` | Identifies movers (PCPs who changed to non-neighboring HRR) | `df_initial_referrals.csv` |
| `3_logit.R` | Expands all PCP-specialist pairs into choice sets (0/1) | `df_logit_movers.csv`, `df_logit_all.csv` (12GB) |
| `4_logit_jochmans.R` | Constructs quartet-structured TWFE data (Jochmans 2018) | `df_jochmans.csv` |
| `5_referrals_by_time.R` | Creates datasets by years-since-move windows | `df_jochmans_windows.csv` |

### Analysis pipeline (`analysis/`)

| Script | Purpose | Key output |
|--------|---------|------------|
| `_main.R` | Loads processed data, sources analysis scripts | Environment objects |
| `1_descriptive_stats.R` | Summary statistics, flow maps | `results/figures/fig_movers.png`, `results/tables/desc.tex` |
| `2_logit_twfe.R` | TWFE logit regressions with fixed effects | `results/tables/logit_twfe_mfx.tex` |
| `3_welfare_calcs.R` | Counterfactual welfare calculations | `results/tables/welfare_summary.tex` |
| `4_referral_windows.R` | Marginal effects by years-since-move | `results/figures/mfx_by_window.png` |
| `5_quad_comparison.R` | Balance table: Jochmans sample vs. excluded | `results/tables/quad_comparison.tex` |

## Key Patterns

- **Variable naming**: `doc_` prefix for PCP attributes, `spec_` for specialist, `same_` for match indicators (e.g., `same_sex`, `same_race`), `diff_` for continuous differences
- **Regression**: Uses `fixest::feglm()` for high-dimensional fixed effects logit; manual first-difference MFX for Jochmans models. Considering `alpaca` package (Fernández-Val & Weidner 2016) for bias-corrected FEs and population-average MFX.
- **Table output**: `modelsummary()` with `kableExtra` backend generating LaTeX tables
- **Merging**: Extensive `left_join()` chains preserving all observations, then filtering
- **Core R packages**: `tidyverse`, `fixest`, `marginaleffects`, `modelsummary`, `sf`, `geodist`

## Data Notes

- All data is gitignored. External data lives in `data/input/` (symlinks to shared storage). Processed data in `data/output/` (~13GB total).
- The largest file (`df_logit_all.csv`, 12GB) contains the full choice set for all physicians. The mover-only version (`df_logit_movers.csv`, 31MB) is used for most analysis.
- Geographic matching uses haversine distance between PCP and specialist ZIP centroids.

## Last Session

Date: 2026-02-13

- Paper fixes: completed truncated footnote (medical school sparsity), fixed hard-coded appendix table numbers to `\ref{}`, added estimation sample comparison section to appendix
- Code cleanup: removed redundant tables (`app_logit_mfx.tex`, `logit_twfe_mfx_alt.tex`) and their generating code from `2_logit_twfe.R`; added `source()` calls to `_main.R`; fixed desc.tex metric/value misalignment
- MFX methodology: identified that Jochmans β are structural parameters but MFX need FEs for predicted probabilities; agreed approach is `alpaca` (Fernández-Val & Weidner 2016) for bias-corrected FEs + population-average MFX; needs discussion with Pablo
- Open items: MFX/welfare implementation (#8/#9), clustering SEs (#10), GitHub push for Overleaf (#12), medical school footnote coverage rate (#13)
