# Analysis

This page describes our analysis of the data.

## Overview

The [analysis](/analysis/) folder contains the code used to analyze the data. The **_main.R** script is the main orchestrator that loads the processed data from `data/output/` and sources the analysis scripts in order. It also sets `options(modelsummary_factory_default = "kableExtra")` to ensure all tables use kableExtra output rather than the tinytable default. All tables are written to `results/tables/` (.tex) and figures to `results/figures/` (.png).

## Main Scripts

- **1_descriptive_stats.R** -- Summary statistics for PCPs and specialists (full sample and movers), network degree distributions, a flow map of PCP movers by origin and destination HRR, and a comparison of established vs. non-established referral links. Outputs `desc.tex`, `link_stats.tex`, `fig_movers.png`, and `fig_degree.png`.

- **2_logit_twfe.R** -- Main estimation. First estimates a baseline conditional logit with PCP fixed effects only (analogous to [Zeltzer 2020](https://doi.org/10.1257/aer.20190294)). Then implements the two-stage approach: Stage 1 estimates structural coefficients via the [Jochmans (2018)](https://doi.org/10.1093/restud/rdx063) quadruple-based estimator (which eliminates two-way FEs via differencing); Stage 2 recovers PCP and specialist fixed effects by holding Jochmans Xβ as an offset in a logit with two-way FEs. Average marginal effects are computed via first-difference on full predicted probabilities (Xβ + α_i + γ_j), with delta-method standard errors using the Jochmans variance-covariance matrix. The main table presents structural β alongside MFX. Outputs `logit_twfe_mfx.tex` and `app_logit_race_mfx.tex`.

- **3_referral_windows.R** -- Dynamics analysis. Repeats the two-stage estimation separately for each referral window (1 through 6 years post-relocation) to trace how the determinants of specialist selection evolve as PCPs accumulate information in their new market. Outputs `mfx_by_window.png`.

## Appendix Scripts

- **app_quad_comparison.R** -- Balance table comparing characteristics of physicians included in the Jochmans estimation sample (discordant quartets) versus those excluded. Outputs `quad_comparison.tex`.

- **app_welfare.R** -- Counterfactual welfare calculations. For each covariate, zeros out its contribution to predicted referral probabilities and computes the resulting change in expected specialist quality, aggregated by HRR. Outputs `welfare_summary.tex`.

- **app_convergence.R** -- Convergence stability test for Stage 2 of the two-stage approach. Compares deviance, FE standard deviations, and marginal effects at iteration counts 50, 100, and 250 to verify that results are stable by iteration 50. Outputs `app_convergence.tex`.
