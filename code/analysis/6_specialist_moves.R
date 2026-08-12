# 6_specialist_moves.R
# Secondary identification of the practice-affiliation (co-location) effect using
# specialists' OWN moves between practices, holding the PCP fixed. Movers stay the
# subjects: a mover's co-location with a specialist is set by whether the mover
# happened to arrive during that specialist's tenure in the practice they joined,
# which is the intersection of two independent timelines (the mover's arrival and the
# specialist's move) rather than the mover's choice of practice. Movers who arrived
# BEFORE the specialist joined are the placebo. Two-way (PCP + specialist) fixed
# effects, linear probability, clustered by HRR. Also contrasts mobile vs stable
# specialists. Self-contained; loops all three specialties; sourced in the _main.R
# post block after 5_peer_referrals.R. Uses data.table for the large-file operations.

sm_specs <- c("ortho", "cardioem", "derm")
sm_lab   <- c(ortho = "Orthopedic surgery", cardioem = "Cardiology", derm = "Dermatology")
sm_est   <- vector("list", length(sm_specs)); names(sm_est) <- sm_specs
sm_desc  <- vector("list", length(sm_specs)); names(sm_desc) <- sm_specs

# Risk-adjusted specialist quality (VRDC, empirical-Bayes shrunk; the same measure as Sec 6).
sm_qual <- fread("data/output/vrdc_specialist_quality_eb.csv",
                 select = c("Specialist_NPI", "m_eb_spend", "m_eb_admit_ne", "m_eb_death"))
setnames(sm_qual, c("specialist", "q_spend", "q_admit", "q_death"))

for (sm_sp in sm_specs) {
  message("Specialist-move identification: ", sm_sp)

  full <- fread(sprintf("data/output/df_full_referrals_%s.csv", sm_sp),
                select = c("doctor", "specialist", "Year", "spec_group", "spec_hrr",
                           "spec_sex", "spec_grad_year"))
  full[, spec_group := as.character(spec_group)]

  # Specialist tenure spells: modal group per specialist-year, genuine (non-oscillating,
  # >= 2-year) spells only, to screen out billing/TIN relabels.
  syg <- full[!is.na(spec_group) & spec_group != "", .N, by = .(specialist, Year, spec_group)]
  setorder(syg, specialist, Year, -N)
  syg <- syg[, .SD[1], by = .(specialist, Year)][, .(specialist, Year, spec_group)]
  setorder(syg, specialist, Year)
  osc <- syg[, .(oscillates = anyDuplicated(rle(spec_group)$values) > 0), by = specialist]
  clean_spec <- osc[oscillates == FALSE, specialist]
  spells <- syg[specialist %in% clean_spec, .(enter = min(Year), exit = max(Year)),
                by = .(specialist, spec_group)][(exit - enter + 1) >= 2]
  mobile <- spells[, .(ngrp = uniqueN(spec_group)), by = specialist][ngrp >= 2, specialist]

  # Descriptive: mobile vs stable specialists (clean-history specialists only).
  spec_char <- unique(full[, .(specialist, spec_sex, spec_grad_year)])[, .SD[1], by = specialist]
  indeg <- full[, .(in_degree = uniqueN(doctor)), by = specialist]
  spec_char <- merge(spec_char, indeg, by = "specialist", all.x = TRUE)
  spec_char <- spec_char[specialist %in% clean_spec]
  spec_char <- merge(spec_char, sm_qual, by = "specialist", all.x = TRUE)
  spec_char[, `:=`(male = as.numeric(spec_sex == "M"), experience = 2018 - spec_grad_year,
                   grp = fifelse(specialist %in% mobile, "Mobile", "Stable"))]
  sm_desc[[sm_sp]] <- spec_char[, .(specialty = sm_sp, n = .N,
                                    male = as.double(mean(male, na.rm = TRUE)),
                                    experience = as.double(median(experience, na.rm = TRUE)),
                                    in_degree = as.double(median(in_degree, na.rm = TRUE)),
                                    q_spend = as.double(median(q_spend, na.rm = TRUE)),
                                    q_admit = as.double(median(q_admit, na.rm = TRUE)),
                                    q_death = as.double(median(q_death, na.rm = TRUE))), by = grp]

  # Mover-menu sample: arrival choice set restricted to specialists who ever occupied
  # the mover's destination practice; co-location set by arrival timing vs tenure.
  mov <- fread(sprintf("data/output/df_logit_movers_%s.csv", sm_sp),
               select = c("doctor", "Year", "doc_group", "doc_hrr", "specialist", "referral",
                          "same_sex", "same_race", "diff_dist", "diff_age", "diff_gradyear"))
  mov[, doc_group := as.character(doc_group)]
  arr <- mov[, .(t = min(Year)), by = doctor]
  mov <- merge(mov, arr, by = "doctor")[Year == t]

  menu <- merge(mov, spells, by.x = c("specialist", "doc_group"), by.y = c("specialist", "spec_group"))
  menu[, e := t - enter]
  menu[, `:=`(CoPresent  = as.integer(t >= enter & t <= exit),
              co_present = as.integer(t >= enter & t <= exit),
              after_left = as.integer(t > exit),
              before_join = as.integer(t < enter))]

  m1 <- feols(referral ~ CoPresent + diff_dist + same_sex + same_race + diff_age + diff_gradyear |
                specialist + doctor, data = menu, cluster = ~doc_hrr)
  m2 <- feols(referral ~ co_present + after_left + diff_dist + same_sex + same_race + diff_age + diff_gradyear |
                specialist + doctor, data = menu, cluster = ~doc_hrr)

  sm_est[[sm_sp]] <- data.table(
    specialty = sm_sp, n_movers = uniqueN(menu$doctor), n_pairs = nrow(menu),
    copresent = coef(m1)[["CoPresent"]], copresent_se = se(m1)[["CoPresent"]],
    co_present = coef(m2)[["co_present"]], co_present_se = se(m2)[["co_present"]],
    after_left = coef(m2)[["after_left"]], after_left_se = se(m2)[["after_left"]],
    rate_before = menu[before_join == 1, mean(referral)],
    rate_co     = menu[co_present == 1, mean(referral)],
    rate_after  = menu[after_left == 1, mean(referral)])

  rm(full, mov, menu); gc()
}

sm_est_all  <- rbindlist(sm_est)
sm_desc_all <- rbindlist(sm_desc)
write_csv(sm_est_all,  "results/tables/specialist_moves_est.csv")
write_csv(sm_desc_all, "results/tables/specialist_moves_desc.csv")

# LaTeX (bare tabular) for the paper.
fmt_pp <- function(b, s) sprintf("%.1f (%.1f)%s", 100 * b, 100 * s, ifelse(abs(b / s) > 1.96, "$^{*}$", ""))
est_tbl <- sm_est_all[, .(
  Specialty = sm_lab[specialty],
  `Co-location`  = mapply(fmt_pp, copresent, copresent_se),
  `Before-join`  = sprintf("%.1f", 100 * rate_before),
  `Co-present`   = sprintf("%.1f", 100 * rate_co),
  `After-left`   = sprintf("%.1f", 100 * rate_after),
  Movers = format(n_movers, big.mark = ","),
  Pairs  = format(n_pairs, big.mark = ","))]
kable(est_tbl, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      align = c("l", "r", "r", "r", "r", "r", "r")) %>%
  save_kable("results/tables/specialist_moves_all.tex")

desc_tbl <- sm_desc_all[, .(
  Specialty = sm_lab[specialty], Group = grp, N = format(n, big.mark = ","),
  `Male (\\%)` = sprintf("%.0f", 100 * male),
  `Experience (yrs)` = sprintf("%.0f", experience),
  `Referring PCPs` = sprintf("%.0f", in_degree),
  `Adj.\\ spending (\\$)` = sprintf("%.0f", q_spend),
  `Adj.\\ non-elec.\\ adm.` = sprintf("%.3f", q_admit),
  `Adj.\\ mortality` = sprintf("%.3f", q_death))]
kable(desc_tbl, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      align = c("l", "l", rep("r", 7))) %>%
  save_kable("results/tables/specialist_moves_desc.tex")

message("Specialist-move identification tables written.")
