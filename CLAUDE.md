# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Research project: "Formation of Physician Referral Networks: Drivers and Potential Consequences." Analyzes how PCPs form referral networks with specialists using Medicare data (2009--2018), focusing on physician movers who relocate across Hospital Referral Regions (HRRs).

Paper and appendix live in `papers/`. Presentations live in `presentations/`. The paper tex files reference `../results/tables/` and `../results/figures/` for all generated output. Bibliography uses `BibTeX_Library.bib` and `aer.bst` (copied from `work/administrative/templates/bibliography/`). Overleaf syncs via GitHub. A GitHub Actions guard (`.github/workflows/guard-overleaf.yml`) prevents Overleaf from modifying `data-code/`, `analysis/`, or `results/`.

## Running the Code

**Data construction** (requires large external datasets via symlinks in `data/input/`):
```r
source("data-code/_BuildData.R")   # orchestrates scripts 1-5, outputs to data/output/
```

**Analysis** (requires processed data in `data/output/`):
```r
source("analysis/_main.R")         # orchestrates scripts 1-3 + appendix scripts
```

There is no Makefile. Scripts are run interactively in R. Each orchestrator (`_BuildData.R`, `_main.R`) sources its subscripts in order. Appendix scripts are prefixed `app_` (no number) and sourced after the main analysis.

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
| `_main.R` | Loads processed data, sources analysis + appendix scripts | Environment objects |
| `1_descriptive_stats.R` | Summary statistics, flow maps | `results/figures/fig_movers.png`, `results/tables/desc.tex` |
| `2_logit_twfe.R` | Two-stage: Jochmans β + FE recovery → structural β and MFX table | `results/tables/logit_twfe_mfx.tex` |
| `3_referral_windows.R` | Two-stage MFX by years-since-move window | `results/figures/mfx_by_window.png` |
| `app_quad_comparison.R` | Balance table: Jochmans sample vs. excluded | `results/tables/quad_comparison.tex` |
| `app_welfare.R` | Counterfactual welfare using two-stage predicted probabilities | `results/tables/welfare_summary.tex` |
| `app_convergence.R` | Convergence stability of two-stage estimation | `results/tables/app_convergence.tex` |

## Key Patterns

- **Variable naming**: `doc_` prefix for PCP attributes, `spec_` for specialist, `same_` for match indicators (e.g., `same_sex`, `same_race`), `diff_` for continuous differences
- **Regression**: Two-stage estimation: (1) Jochmans (2018) quartet estimator via `fixest::feglm()` for consistent β, (2) FE recovery via `fixest::feglm()` with Jochmans Xβ as offset (`glm.iter = 50`). MFX via first-difference on full predicted probabilities (Xβ + α_i + γ_j) with delta-method SEs. Main table presents structural β alongside MFX. Baseline Zeltzer-style logit uses `fixest::feglm()` with doctor FE only.
- **Table output**: `modelsummary()` with `kableExtra` backend generating LaTeX tables. Table `.tex` files must contain only `\begin{tabular}...\end{tabular}` (no `\begin{table}` wrapper) because the paper provides its own `\begin{table}` with captions.
- **Merging**: Extensive `left_join()` chains preserving all observations, then filtering
- **Core R packages**: `tidyverse`, `fixest`, `marginaleffects`, `modelsummary`, `sf`, `geodist`

## Data Notes

- All data is gitignored. External data lives in `data/input/` (symlinks to shared storage). Processed data in `data/output/` (~13GB total).
- The largest file (`df_logit_all.csv`, 12GB) contains the full choice set for all physicians. The mover-only version (`df_logit_movers.csv`, 31MB) is used for most analysis.
- Geographic matching uses haversine distance between PCP and specialist ZIP centroids.

## Static Tables

Four appendix tables in `results/tables/` are **not** generated by the analysis pipeline. They come from `physician-race` repo (`research-data-repo/physician-race/data-code/3_assess.R`) and were manually exported:
- `app_method_accuracy.tex` — overall accuracy by race imputation method
- `app_race_accuracy.tex` — accuracy by race category and method
- `app_conf_full.tex` — confusion matrix, full random forest
- `app_conf_name.tex` — confusion matrix, name-based random forest

## Last Session

Date: 2026-02-14

- Added `.github/workflows/guard-overleaf.yml` — protects `data-code/`, `analysis/`, `results/` from Overleaf sync pushes (adapted from `/kickoff` template)
- **Still open**: dynamics expansion, consequences reframe, heterogeneity analysis, medical school footnote (#13), fill in placeholder docs (data.md, analysis.md, present.md)
