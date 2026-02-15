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
| `1_descriptive_stats.R` | Summary statistics, flow maps | `desc.tex`, `link_stats.tex`, `inline_stats.csv`, `fig_movers.png`, `fig_degree.png` |
| `2_logit_twfe.R` | Two-stage: Jochmans β + FE recovery → structural β and MFX table | `results/tables/logit_twfe_mfx.tex` |
| `3_referral_windows.R` | Two-stage MFX by years-since-move window | `results/figures/mfx_by_window.png` |
| `app_quad_comparison.R` | Balance table: Jochmans sample vs. excluded | `results/tables/quad_comparison.tex` |
| `app_welfare.R` | Counterfactual welfare using two-stage predicted probabilities | `results/tables/welfare_summary.tex` |
| `app_convergence.R` | Convergence stability of two-stage estimation | `results/tables/app_convergence.tex` |

## Key Patterns

- **Variable naming**: `doc_` prefix for PCP attributes, `spec_` for specialist, `same_` for match indicators (e.g., `same_sex`, `same_race`), `diff_` for continuous differences
- **Regression**: Two-stage estimation: (1) Jochmans (2018) quartet estimator via `fixest::feglm()` for consistent β, (2) FE recovery via `fixest::feglm()` with Jochmans Xβ as offset (`glm.iter = 50`). MFX via first-difference on full predicted probabilities (Xβ + α_i + γ_j) with delta-method SEs. Main table presents structural β alongside MFX. Baseline Zeltzer-style logit uses `fixest::feglm()` with doctor FE only.
- **Table output**: Manual `kable(..., format = "latex")` for LaTeX tables (avoid `modelsummary` for LaTeX output due to v2.2.0 bugs). Table `.tex` files must contain only `\begin{tabular}...\end{tabular}` (no `\begin{table}` wrapper) because the paper provides its own `\begin{table}` with captions. Never use `kable_styling(latex_options = "hold_position")` as it adds a `\begin{table}` wrapper.
- **Inline stats**: Any summary statistics referenced in the paper text should be written to a file (e.g., `results/tables/inline_stats.csv`) rather than only printed to the console. This prevents hardcoded numbers from going stale.
- **Merging**: Extensive `left_join()` chains preserving all observations, then filtering
- **Core R packages**: `tidyverse`, `fixest`, `marginaleffects`, `modelsummary`, `sf`, `geodist`

## Paper Style

- **No em dashes**: avoid `---` and `—` in paper text. Use commas, colons, parentheses, or "i.e." instead.
- **Appendix references**: use plain "supplemental appendix" text, no `\ref{}` cross-references to the appendix document.
- **Avoid repetition**: don't start consecutive paragraphs with the same phrase (e.g., "In the supplemental appendix, ...").
- **Hardcoded numbers**: always verify paper text against generated tables/CSV. If a stat isn't in a table, it should be written to `inline_stats.csv`.

## Data Notes

- All data is gitignored. External data lives in `data/input/` (symlinks to shared storage). Processed data in `data/output/` (~13GB total).
- The largest file (`df_logit_all.csv`, 12GB) contains the full choice set for all physicians. The mover-only version (`df_logit_movers.csv`, 31MB) is used for most analysis.
- Geographic matching uses haversine distance between PCP and specialist ZIP centroids.

## Static Tables

Several tables in `results/tables/` are **not** generated by the analysis pipeline in this repo:

From `physician-race` repo (`research-data-repo/physician-race/data-code/3_assess.R`):
- `app_method_accuracy.tex` — overall accuracy by race imputation method
- `app_race_accuracy.tex` — accuracy by race category and method
- `app_conf_full.tex` — confusion matrix, full random forest
- `app_conf_name.tex` — confusion matrix, name-based random forest

Now generated by `1_descriptive_stats.R`:
- `link_stats_by_window.tex` — network summary statistics by referral window (Table 4)

## Last Session

Date: 2026-02-15

- Designed multi-specialty VRDC pipeline for Comment 2 (additional specialties). Full design document in `scratch/multi-specialty-pipeline-notes.md`.
- **Cardiology decision**: Pacemaker implantation (DRGs 242-244). PCI rejected (~70% emergent), CABG rejected (cardiac surgeon, not cardiologist). Pacemaker is nearly all elective, clear PCP->EP referral, ~200k/year, DRGs stable 2009-2018.
- **Dermatology decision**: New patient E&M visits (CPT 99201-99205) as anchor event + carrier lookback for PCP. Novel identification strategy (no published precedent). `rfr_physn_npi` rejected as unreliable.
- **Pipeline architecture**: SAS-only in VRDC (no Stata). Two paths converge: inpatient DRG anchor (ortho, cardio) and carrier CPT anchor (derm), then shared carrier lookback for PCP identification (2-year extraction window, 365-day date filter, most-visited physician with recency tiebreaker).
- **Literature review**: Documented referral identification validation methods. Key finding: no published validation of `rfr_physn_npi` or carrier lookback against gold standard. Added validation TODO to `physician-learning-and-referrals/referrals-and-learning/CLAUDE.md`.
- No code changes; design and documentation only. Comment 2 still blocked on VRDC export.
- **Next steps**: Comment 2 is the priority; other comments wait until multi-specialty results are in. New `scratch/` directory (gitignored) holds design docs.

## Referee Response Plan (internal review, Feb 2026)

Referee report is at `results/refreport_202602.md`. Four major comments; implementation sequencing below.

### Sequencing
Comment 2 (additional specialties) is highest priority because results may differ across specialties, affecting how we frame Comments 1, 3, 4. However, Comment 2 is blocked on VRDC code export. While waiting, Comments 1/3/4 can proceed on ortho-only but may need revision once multi-specialty results are in.

### Comment 1: What does "same practice group" capture? (low priority)
- **(a)** Compute share of PCP movers whose choice set contains at least one same-practice specialist. Use `df_logit` where `same_prac == 1`, group by doctor, check `any()`. Write to `inline_stats.csv`. Add sentence in paper.
- **(b)** Robustness Jochmans spec dropping `same_prac`. Full spec only (not all 3), output 2 columns (structural beta + MFX) → `results/tables/app_robustness_noprac.tex`. New appendix section + paper sentence referencing it.
- Both additions in existing scripts (`1_descriptive_stats.R`, `2_logit_twfe.R`), no new files.

### Comment 2: External validity — additional specialties (BLOCKED, highest priority)
- **Blocked on**: VRDC code export (requested, awaiting approval). Existing ortho pipeline code at `physician-learning-and-referrals/referrals-and-learning/data-code/` is outdated and will be replaced.
- **Design complete** — see `scratch/multi-specialty-pipeline-notes.md` for full pipeline pseudocode, code flags, literature review, and implementation notes.
- **Specialty decisions**:
  - **Cardiology**: Pacemaker implantation (DRGs 242, 243, 244). Inpatient anchor, same pipeline structure as ortho. EP coded as "Cardiology" in MDPPAS. PCI/CABG/valve/ablation all rejected (see scratch notes for rationale).
  - **Dermatology**: New patient E&M visits (CPT 99201-99205) with dermatologist as anchor. Carrier lookback for PCP identification (same logic as ortho/cardio). Novel approach — no published precedent.
  - Rationale for these three specialties: ortho ~98% male specialists, cardiology ~85% male, dermatology ~50% female — spans the gender spectrum.
- **Pipeline**: SAS-only in VRDC. Two anchor paths (inpatient DRG for ortho/cardio, carrier CPT for derm) converge into shared carrier lookback (2-year window, 365-day filter) and MDPPAS specialty filters. Output: one CSV per specialty (`ReferralPairs_Ortho.csv`, `ReferralPairs_Cardiology.csv`, `ReferralPairs_Dermatology.csv`).
- **CMS extract rules**: No raw TIN/EIN (can export masked group ID). Minimum 11 cell count for aggregated statistics.
- **Once code arrives**:
  1. Compare updated VRDC code against pipeline pseudocode; flag discrepancies.
  2. Implement SAS macros parameterized by specialty (DRG/CPT lists, specialty name, table prefix).
  3. Add `rfr_physn_npi` validation script (concordance with carrier lookback PCP).
  4. Export three CSVs to `data/input/referrals/`.
  5. Parameterize `_BuildData.R` and `_main.R` to loop over specialties.
  6. Run Jochmans + MFX + dynamics for each specialty.
- **Outputs**: cross-specialty MFX comparison table, possibly cross-specialty dynamics figure. Placement (main paper vs appendix) TBD based on results.
- **Key concern**: if gender/race effects differ substantially across specialties (e.g., larger gender concordance for dermatology where specialist pool is more diverse), this reshapes the paper's conclusions and affects how we respond to Comments 1, 3, 4.

### Comment 3: Dynamics interpretation (high priority)
- Referee's concern: cumulative windows may show attenuation via "compositional dilution" (denominator grows as new links are added, mechanically pushing shares toward population averages) rather than genuine behavioral change.
- **(a)** Descriptive decomposition: new vs. persisting links. For windows 2–6, split `win_links` (already in `1_descriptive_stats.R` line 323) into persisting (`semi_join` with k-1) and new (`anti_join`). Compute same_prac, same_sex, same_race, mean distance for each group. Output: `results/tables/app_link_decomposition.tex`. Placement TBD (main paper if results are clean, appendix otherwise). **Do this first** — if new links clearly drive attenuation, 3b becomes robustness rather than essential.
- **(b)** Period-specific (non-cumulative) Jochmans estimation. New data construction in `5_referrals_by_time.R` (period-specific windows + quartet construction via `make_block_win`). New script `analysis/app_period_windows.R` (estimation loop mirroring `3_referral_windows.R`). Output: `results/figures/mfx_by_window_period.png` in appendix. Risk: fewer observations per window → possible convergence failures or noisy estimates in later windows.

### Comment 4: Sample composition reporting (low priority, low effort)
- **(a)** Count unique PCPs and specialists in Jochmans quartets: `n_distinct(c(df_logit_twfe$doc1, df_logit_twfe$doc2))` etc. Write to `inline_stats.csv`. Add sentence in paper Section 2.2.
- **(b)** Compare non-separated subsample (where MFX are computed) vs. full choice set on same_prac, same_sex, same_race, mean distance. Uses `stage2_3$valid` to split `dat_fe`. **Inline text** in convergence appendix section, not a formal table.
- Both additions in `2_logit_twfe.R`.

### Other
- Direct quality regression (does homophily predict lower specialist quality?) — lower priority, not tied to referee comments.
