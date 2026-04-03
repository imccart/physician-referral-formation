# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

Research project: "Formation of Physician Referral Networks: Drivers and Potential Consequences." Analyzes how PCPs form referral networks with specialists using Medicare data (2013--2018, with ortho 2009--2018 as robustness), focusing on physician movers who relocate across Hospital Referral Regions (HRRs).

Paper and appendix live in `papers/`. The paper tex files reference `../results/tables/` and `../results/figures/` for all generated output. Bibliography uses `BibTeX_Library.bib` and `aer.bst` (copied from `work/administrative/templates/bibliography/`). Overleaf syncs via GitHub. A GitHub Actions guard (`.github/workflows/guard-overleaf.yml`) prevents Overleaf from modifying `code/` or `results/`.

## Running the Code

**Setup**: `code/0-setup.R` activates renv and loads all packages. Both orchestrators source it automatically.

**Data construction** (requires large external datasets via symlinks in `data/input/`):
```r
source("code/data-build/_BuildData.R")   # orchestrates scripts 1-5, outputs to data/output/
```

**Analysis** (requires processed data in `data/output/`):
```r
source("code/analysis/_main.R")         # orchestrates scripts 1-3 + appendix scripts
```

There is no Makefile. Scripts are run interactively in R. Each orchestrator (`_BuildData.R`, `_main.R`) sources `code/0-setup.R` then its subscripts in order. Appendix scripts are prefixed `app_` (no number) and sourced after the main analysis.

## Architecture

```
data/input/  (symlinked, gitignored)  →  code/data-build/_BuildData.R  →  data/output/ (gitignored)
                                                                                  ↓
                                              code/analysis/_main.R  ←────────────┘
                                                      ↓
                                                 results/tables/*.tex, results/figures/*.png
                                                      ↓
                                                 papers/*.tex  (refs ../results/)
```

### VRDC SAS pipeline (`code/data-build/sas/`)

Runs in CMS Virtual Research Data Center. Macro-parameterized for four specialties (ortho, cardio pacemaker, cardioEM, derm).

| Script | Purpose | Key output |
|--------|---------|------------|
| `0_config.sas` | Global params, library refs, utility macros | Macro variables |
| `1_anchor_events.sas` | Specialist-patient encounters (DRG or CPT) | `{Prefix}Encounters_{year}`, `{Prefix}Patients_Unique` |
| `2_carrier_extract.sas` | Carrier E&M claims for anchor patients | `{Prefix}Carrier_{year}` (2012-2018) |
| `3_pcp_assignment.sas` | 365-day lookback, PCP identification | `{Prefix}PCP_{year}` |
| `4_specialty_filter.sas` | MDPPAS join: PCP is primary care, specialist matches | `{Prefix}ValidPairs_{year}` |
| `5_outcomes.sas` | Readmission rate (ortho/cardio) or volume (derm) | `{Prefix}Quality_All` |
| `6_aggregate_export.sas` | Aggregate to PCP x Specialist x Year, mask, export | `ReferralPairs_Full_*`, `ReferralPairs_Large_*` |

### Data construction pipeline (`code/data-build/`)

Both `_BuildData.R` and `_main.R` loop over a shared `specialties` config (ortho, cardioem, derm). All per-specialty output files are suffixed `_{specialty}` (e.g., `df_full_referrals_ortho.csv`).

| Script | Purpose | Key output (per specialty) |
|--------|---------|------------|
| `_BuildData.R` | Loads shared data (MDPPAS, PhyCompare, race, geography), then loops over specialties | Environment objects |
| `1_referrals_full.R` | Builds full PCP-specialist referral dataset | `df_full_referrals_{spec}.csv` |
| `2_referrals_initial.R` | Identifies movers (PCPs who changed to non-neighboring HRR) | `df_initial_referrals_{spec}.csv`, `df_movers_{spec}.csv` |
| `3_logit.R` | Expands all PCP-specialist pairs into choice sets (0/1) | `df_logit_movers_{spec}.csv` |
| `4_logit_jochmans.R` | Constructs quartet-structured TWFE data (Jochmans 2018) | `df_jochmans_{spec}.csv` |
| `5_referrals_by_time.R` | Creates datasets by years-since-move windows | `df_jochmans_windows_{spec}.csv`, `df_logit_windows_{spec}.csv`, `df_initial_referrals_cuml_{spec}.csv` |

### Analysis pipeline (`code/analysis/`)

| Script | Purpose | Key output (per specialty) |
|--------|---------|------------|
| `_main.R` | Loads per-specialty data, sources analysis + appendix scripts in loop, then sources cross-specialty comparison | Environment objects |
| `1_descriptive_stats.R` | Summary statistics, flow maps | `desc_{spec}.tex`, `link_stats_{spec}.tex`, `inline_stats_{spec}.csv`, `fig_movers_{spec}.png`, `fig_degree_{spec}.png` |
| `2_logit_twfe.R` | Two-stage: Jochmans β + FE recovery → structural β and MFX table | `logit_twfe_mfx_{spec}.tex`, `app_logit_race_mfx_{spec}.tex` |
| `3_referral_windows.R` | Two-stage MFX by years-since-move window | `mfx_by_window_{spec}.png` |
| `4_cross_specialty.R` | Cross-specialty MFX forest plot and dynamics comparison | `mfx_cross_specialty.png`, `mfx_by_window_cross.png` |
| `app_quad_comparison.R` | Balance table: Jochmans sample vs. excluded | `quad_comparison_{spec}.tex` |
| `app_welfare.R` | Counterfactual welfare (ortho only, requires `spec_qual`) | `welfare_summary_{spec}.tex` |
| `app_convergence.R` | Convergence stability of two-stage estimation | `app_convergence_{spec}.tex` |

## Key Patterns

- **Variable naming**: `doc_` prefix for PCP attributes, `spec_` for specialist, `same_` for match indicators (e.g., `same_sex`, `same_race`), `diff_` for continuous differences
- **Regression**: Two-stage estimation: (1) Jochmans (2018) quartet estimator via `fixest::feglm()` for consistent β, (2) FE recovery via `fixest::feglm()` with Jochmans Xβ as offset (`glm.iter = 50`). MFX via first-difference on full predicted probabilities (Xβ + α_i + γ_j) with delta-method SEs. Main table presents structural β alongside MFX. Baseline Zeltzer-style logit uses `fixest::feglm()` with doctor FE only. All MFX computed manually (not via `marginaleffects::avg_slopes()`, which is incompatible with `feglm` FE uncertainty in v0.30.0).
- **Table output**: Manual `kable(..., format = "latex")` for LaTeX tables (avoid `modelsummary` for LaTeX output due to v2.2.0 bugs). Table `.tex` files must contain only `\begin{tabular}...\end{tabular}` (no `\begin{table}` wrapper) because the paper provides its own `\begin{table}` with captions. Never use `kable_styling(latex_options = "hold_position")` as it adds a `\begin{table}` wrapper.
- **Inline stats**: Any summary statistics referenced in the paper text should be written to a file (e.g., `results/tables/inline_stats.csv`) rather than only printed to the console. This prevents hardcoded numbers from going stale.
- **Merging**: Extensive `left_join()` chains preserving all observations, then filtering
- **Core R packages**: `tidyverse`, `fixest`, `marginaleffects`, `modelsummary`, `sf`, `geodist`, `data.table`
- **data.table for quartet construction**: `make_block` (in `code/data-build/4_logit_jochmans.R`) and `make_block_win` (in `code/data-build/5_referrals_by_time.R`) use data.table internally (`CJ`, `merge`, `setnames`, `rbindlist`, `fwrite`) to avoid vctrs bugs in dplyr/tidyselect. Results convert back to tibble at the end.
- **No `map_dfr`/`map2_dfr`/`imap_dfr`**: these trigger vctrs bugs with many empty tibbles. Always use `map() %>% bind_rows()` instead.

## Paper Style

- **No em dashes**: avoid `---` and `—` in paper text. Use commas, colons, parentheses, or "i.e." instead.
- **Appendix references**: use plain "supplemental appendix" text, no `\ref{}` cross-references to the appendix document.
- **Avoid repetition**: don't start consecutive paragraphs with the same phrase (e.g., "In the supplemental appendix, ...").
- **Hardcoded numbers**: always verify paper text against generated tables/CSV. If a stat isn't in a table, it should be written to `inline_stats.csv`.

## Data Notes

- All data is gitignored. External data lives in `data/input/` (symlinks to shared storage). Processed data in `data/output/`.
- Three specialty input files: `REFERRALPAIRS_LARGE_ORTHO.csv` (692K rows), `REFERRALPAIRS_LARGE_CARDIOEM.csv` (2.4M), `REFERRALPAIRS_LARGE_DERM.csv` (2.4M). All share columns `Practice_ID, Specialist_ID, Year, total_pcp_patients, total_spec_patients`. Only ortho has `spec_qual`.
- `df_logit_all.csv` write was removed (12GB, never read back). The mover-only version (`df_logit_movers_{spec}.csv`) is used for analysis.
- Geographic matching uses haversine distance between PCP and specialist ZIP centroids.
- Ortho 2009-2018 robustness (`ReferralPairs_Large.csv`, 1.9M rows) can be run as a fourth specialty entry `ortho_robust`.

## Static Tables

Several tables in `results/tables/` are **not** generated by the analysis pipeline in this repo:

From `physician-race` repo (`research-data-repo/physician-race/data-code/3_assess.R`):
- `app_method_accuracy.tex` — overall accuracy by race imputation method
- `app_race_accuracy.tex` — accuracy by race category and method
- `app_conf_full.tex` — confusion matrix, full random forest
- `app_conf_name.tex` — confusion matrix, name-based random forest

Now generated by `1_descriptive_stats.R`:
- `link_stats_by_window_{spec}.tex` — network summary statistics by referral window (Table 4)

## Last Session

Date: 2026-03-04

- Ran `analysis/_main.R` for all three specialties. All results generated (tables, figures, cross-specialty comparison).
- Fixed `marginaleffects::avg_slopes()` incompatibility with `feglm` FEs (v0.30.0). Replaced with manual MFX via `compute_mfx_zeltzer()` using `fixef()` to build eta. Fixed `spec_male` logical-to-numeric coercion in `_main.R`.
- Reviewed results: same practice and distance are robust across specialties. Gender effect tracks specialist gender composition. Same-practice attenuation over time (the "learning" finding) does NOT replicate in cardioem/derm (flat dynamics). 2013-2018 panel too short for reliable dynamics — DUA extension request submitted to ResDAC for 2008-2012 carrier claims.
- **Next**: wait for carrier claims extension, then re-run with extended panel before finalizing dynamics analysis and remaining referee comments.

## Referee Response Plan (internal review, Feb 2026)

Referee report is at `results/refreport_202602.md`. Four major comments; implementation sequencing below.

### Sequencing
Comment 2 complete. Dynamics analysis (Comment 3) on hold pending carrier claims extension to 2008 (DUA request submitted to ResDAC). Remaining comments (1, 3, 4) deferred until extended panel is available.

### Comment 1: What does "same practice group" capture? (low priority)
- **(a)** Compute share of PCP movers whose choice set contains at least one same-practice specialist. Use `df_logit` where `same_prac == 1`, group by doctor, check `any()`. Write to `inline_stats.csv`. Add sentence in paper.
- **(b)** Robustness Jochmans spec dropping `same_prac`. Full spec only (not all 3), output 2 columns (structural beta + MFX) → `results/tables/app_robustness_noprac.tex`. New appendix section + paper sentence referencing it.
- Both additions in existing scripts (`code/analysis/1_descriptive_stats.R`, `code/analysis/2_logit_twfe.R`), no new files.

### Comment 2: External validity — additional specialties (COMPLETE)
- **Status**: Full pipeline complete for all three specialties. All results in `results/tables/` and `results/figures/`.
- **Specialty rationale**: ortho ~98% male specialists, cardiology ~85% male, dermatology ~50% female — spans the gender spectrum.
- **Key findings**: Same practice and distance are robust universal drivers. Gender matching strongest in ortho, moderate in derm, near-zero in cardioem. Race imprecise everywhere. Cross-specialty forest plot in `mfx_cross_specialty.png`.

### Comment 3: Dynamics interpretation (high priority)
- Referee's concern: cumulative windows may show attenuation via "compositional dilution" (denominator grows as new links are added, mechanically pushing shares toward population averages) rather than genuine behavioral change.
- **(a)** Descriptive decomposition: new vs. persisting links. For windows 2–6, split `win_links` (already in `code/analysis/1_descriptive_stats.R`) into persisting (`semi_join` with k-1) and new (`anti_join`). Compute same_prac, same_sex, same_race, mean distance for each group. Output: `results/tables/app_link_decomposition.tex`. Placement TBD (main paper if results are clean, appendix otherwise). **Do this first** — if new links clearly drive attenuation, 3b becomes robustness rather than essential.
- **(b)** Period-specific (non-cumulative) Jochmans estimation. New data construction in `code/data-build/5_referrals_by_time.R` (period-specific windows + quartet construction via `make_block_win`). New script `code/analysis/app_period_windows.R` (estimation loop mirroring `3_referral_windows.R`). Output: `results/figures/mfx_by_window_period.png` in appendix. Risk: fewer observations per window → possible convergence failures or noisy estimates in later windows.

### Comment 4: Sample composition reporting (low priority, low effort)
- **(a)** Count unique PCPs and specialists in Jochmans quartets: `n_distinct(c(df_logit_twfe$doc1, df_logit_twfe$doc2))` etc. Write to `inline_stats.csv`. Add sentence in paper Section 2.2.
- **(b)** Compare non-separated subsample (where MFX are computed) vs. full choice set on same_prac, same_sex, same_race, mean distance. Uses `stage2_3$valid` to split `dat_fe`. **Inline text** in convergence appendix section, not a formal table.
- Both additions in `code/analysis/2_logit_twfe.R`.

### Other
- Direct quality regression (does homophily predict lower specialist quality?) — lower priority, not tied to referee comments.
