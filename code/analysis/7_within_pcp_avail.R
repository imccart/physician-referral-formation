# 7_within_pcp_avail.R
# Second identification of the practice-affiliation effect in Section 6, side by
# side with the specialist-move design. Holds the PCP fixed and identifies off
# cross-specialty variation in in-house availability. Collapses the three mover
# choice-set files (same construction as the main analysis) to the mover-by-
# specialty level: pi = share of the mover's specialty-s referrals going in-house,
# a = share of the mover's specialty-s choice set inside their own practice. Under
# random allocation E[pi] = a, so the null slope of pi on a is one; a slope above
# one is over-referral to in-house. A benchmark referral model on distance,
# demographic concordance, and specialist quality (VRDC empirical-Bayes; NOT
# same-practice) gives a predicted in-house share whose slope on availability is
# what proximity, homophily, and quality deliver; the practice-affiliation slope
# is the residual. Two-way decomposition: total = benchmark + affiliation.
# Self-contained; sourced in the _main.R post block after 6_specialist_moves.R.

specs <- c("ortho", "cardioem", "derm")
keep  <- c("Year", "doctor", "specialist", "referral", "same_prac", "doc_hrr",
           "diff_dist", "same_sex", "same_race", "diff_age", "diff_gradyear")
bvars <- c("diff_dist", "same_sex", "same_race", "diff_age", "diff_gradyear")

wpa_raw <- rbindlist(lapply(specs, function(s) {
  d <- fread(sprintf("data/output/df_logit_movers_%s.csv", s), select = keep)
  d[, specialty := s]
  d
}))
wpa_raw <- wpa_raw[complete.cases(wpa_raw[, ..bvars])]

# VRDC EB specialist quality; impute missing to 0 (the EB mean) with a flag.
wpa_q <- fread("data/output/vrdc_specialist_quality_eb.csv",
               select = c("Specialist_NPI", "m_eb_admit_ne", "m_eb_death", "m_eb_spend"))
setnames(wpa_q, c("specialist", "q_admit", "q_death", "q_spend"))
wpa_raw <- merge(wpa_raw, wpa_q, by = "specialist", all.x = TRUE)
wpa_raw[, q_missing := as.integer(is.na(q_admit))]
for (v in c("q_admit", "q_death", "q_spend")) wpa_raw[is.na(get(v)), (v) := 0]

# Benchmark: referral on distance, concordance, and quality, no same-practice.
wpa_bench <- feglm(referral ~ diff_dist + same_sex + same_race + diff_age + diff_gradyear +
                     q_admit + q_death + q_spend + q_missing | doctor + Year,
                   data = wpa_raw, family = binomial("logit"), glm.iter = 200)
wpa_raw[, phat := predict(wpa_bench, newdata = wpa_raw, type = "response")]

# No-quality benchmark (distance and concordance only), so we can show the
# affiliation slope is unchanged whether or not specialist quality enters.
wpa_bench_nq <- feglm(referral ~ diff_dist + same_sex + same_race + diff_age + diff_gradyear |
                        doctor + Year,
                      data = wpa_raw, family = binomial("logit"), glm.iter = 200)
wpa_raw[, phat_nq := predict(wpa_bench_nq, newdata = wpa_raw, type = "response")]
wpa_raw <- wpa_raw[!is.na(phat) & !is.na(phat_nq)]

# Collapse to mover x specialty.
wpa_cell <- wpa_raw[, .(n_ref   = sum(referral),
                        pi_in   = sum(referral == 1 & same_prac == 1) / sum(referral),
                        b_dist  = sum(phat * same_prac) / sum(phat),
                        b_nq    = sum(phat_nq * same_prac) / sum(phat_nq),
                        avail   = mean(same_prac),
                        doc_hrr = doc_hrr[1]),
                    by = .(doctor, specialty)]
wpa_cell <- wpa_cell[n_ref > 0]
wpa_cell[, nspec     := .N, by = doctor]
wpa_cell[, excess    := pi_in - b_dist]
wpa_cell[, excess_nq := pi_in - b_nq]

# Within-mover availability slopes: total, benchmark, affiliation (residual).
wpa_tot    <- feols(pi_in     ~ avail | doctor, wpa_cell, cluster = ~doc_hrr)
wpa_ben    <- feols(b_dist    ~ avail | doctor, wpa_cell, cluster = ~doc_hrr)
wpa_aff    <- feols(excess    ~ avail | doctor, wpa_cell, cluster = ~doc_hrr)
wpa_aff_nq <- feols(excess_nq ~ avail | doctor, wpa_cell, cluster = ~doc_hrr)  # quality out of benchmark
# Level: mean excess over the benchmark among the identifying movers.
wpa_lvl <- feols(excess ~ 1, wpa_cell[nspec >= 2], cluster = ~doc_hrr)

wpa_out <- data.table(
  n_movers    = wpa_cell[nspec >= 2, uniqueN(doctor)],
  n_cells     = wpa_cell[nspec >= 2, .N],
  mean_pi     = wpa_cell[, mean(pi_in)],
  mean_avail  = wpa_cell[, mean(avail)],
  mean_bench  = wpa_cell[, mean(b_dist)],
  slope_total = coef(wpa_tot)[["avail"]],  se_total = se(wpa_tot)[["avail"]],
  slope_bench = coef(wpa_ben)[["avail"]],  se_bench = se(wpa_ben)[["avail"]],
  slope_affil = coef(wpa_aff)[["avail"]],  se_affil = se(wpa_aff)[["avail"]],
  slope_affil_nq = coef(wpa_aff_nq)[["avail"]],  se_affil_nq = se(wpa_aff_nq)[["avail"]],
  excess_pp   = 100 * coef(wpa_lvl)[["(Intercept)"]],
  excess_pp_se = 100 * se(wpa_lvl)[["(Intercept)"]])
write_csv(wpa_out, "results/tables/within_pcp_avail.csv")

# LaTeX (bare tabular): slope decomposition, then level and sample.
fmt_es <- function(b, s) sprintf("%.2f (%.2f)", b, s)
wpa_tbl <- data.frame(
  ` ` = c("Total",
          "Distance, concordance, quality",
          "Practice affiliation (residual)",
          "Excess in-house share (pp)",
          "Movers",
          "Mover-specialty cells"),
  `Estimate` = c(fmt_es(wpa_out$slope_total, wpa_out$se_total),
                 fmt_es(wpa_out$slope_bench, wpa_out$se_bench),
                 fmt_es(wpa_out$slope_affil, wpa_out$se_affil),
                 fmt_es(wpa_out$excess_pp,   wpa_out$excess_pp_se),
                 format(wpa_out$n_movers, big.mark = ","),
                 format(wpa_out$n_cells,  big.mark = ",")),
  check.names = FALSE)
kable(wpa_tbl, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      align = c("l", "r")) %>%
  pack_rows("Slope of in-house referral share on availability (null $=1$)", 1, 3,
            escape = FALSE) %>%
  pack_rows("Levels and sample", 4, 6) %>%
  save_kable("results/tables/within_pcp_avail_all.tex")

message(sprintf("within-PCP availability: total %.2f, benchmark %.2f, affiliation %.2f (se %.2f; %.2f without quality); excess %.1f pp; %s movers",
                wpa_out$slope_total, wpa_out$slope_bench, wpa_out$slope_affil, wpa_out$se_affil,
                wpa_out$slope_affil_nq, wpa_out$excess_pp, format(wpa_out$n_movers, big.mark = ",")))

rm(wpa_raw, wpa_cell); gc()
