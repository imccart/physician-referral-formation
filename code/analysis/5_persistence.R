# Persistence of the initial network (Section 5) -----------------------------
# Do the ties a PCP forms on arrival persist? Among movers who remain in their
# destination HRR (stable movers), we ask whether a first-year tie is referred to
# again in each later post-move year. Selection-corrected: every first-year
# choice-set pair is kept and a later non-appearance is scored as not-referred (0),
# so we do not condition on tie survival. Linear probability model with specialist,
# PCP, and year fixed effects, two-way clustered on HRR and specialist. Reports the
# pooled effect, the effect net of the tie-forming characteristics, and the decay by
# post-move year. Self-contained; loops over all three specialties. Writes persist.tex.

specs   <- c("ortho", "cardioem", "derm")
covs    <- c("same_prac", "diff_dist", "same_sex", "same_race", "diff_age", "diff_gradyear")
need    <- c("doctor", "specialist", "period", "referral", "doc_hrr", covs)
later_p <- c("Year 2 only", "Year 3 only", "Year 4 only")

# Stable movers: those who never leave their destination HRR in the four post-move years.
stable_movers <- function(sp) {
  movers <- unique(fread(sprintf("data/output/df_initial_referrals_cuml_%s.csv", sp))[
    , .(doctor, move_year, destination)])
  hp <- unique(fread(sprintf("data/output/df_full_referrals_%s.csv", sp),
                     select = c("doctor", "Year", "doc_hrr")))
  m    <- merge(movers, hp, by = "doctor", allow.cartesian = TRUE)
  post <- m[Year > move_year & Year <= move_year + 4 & !is.na(doc_hrr)]
  st   <- post[, .(stable = all(doc_hrr == destination)), by = doctor]
  st[stable == TRUE, doctor]
}

# Selection-corrected first-year panel: keep every first-year pair, score a later
# non-appearance as 0.
build_panel <- function(sp) {
  stable <- stable_movers(sp)
  f   <- sprintf("data/output/df_logit_period_%s.csv", sp)
  idx <- match(need, names(fread(f, nrows = 0)))
  d   <- fread(f, select = idx); setnames(d, names(d), need)
  d   <- d[doctor %in% stable]
  d[, `:=`(doctor = as.character(doctor), specialist = as.character(specialist))]
  yr1 <- d[period == "Year 1 only", c("doctor", "specialist", "doc_hrr", "referral", covs), with = FALSE]
  setnames(yr1, "referral", "First")
  grid <- yr1[rep(seq_len(.N), each = 3)]
  grid[, period := rep(later_p, times = nrow(yr1))]
  lref <- d[period %in% later_p, .(doctor, specialist, period, ref = referral)]
  grid <- merge(grid, lref, by = c("doctor", "specialist", "period"), all.x = TRUE)
  grid[, referral := fifelse(is.na(ref), 0L, as.integer(ref))]
  grid[]
}

get1 <- function(m) c(coef(m)["First"], se(m)["First"])
getsp <- function(sp) {
  g  <- build_panel(sp)
  m1 <- feols(referral ~ First | specialist + doctor + period, g, cluster = ~ doc_hrr + specialist)
  m2 <- feols(as.formula(paste("referral ~ First +", paste(covs, collapse = "+"),
              "| specialist + doctor + period")), g, cluster = ~ doc_hrr + specialist)
  by <- lapply(later_p, function(p)
    feols(as.formula(paste("referral ~ First +", paste(covs, collapse = "+"),
          "| specialist + doctor")), g[period == p], cluster = ~ doc_hrr + specialist))
  setNames(c(get1(m1), get1(m2), get1(by[[1]]), get1(by[[2]]), get1(by[[3]]), nobs(m2)),
           c("d1c", "d1s", "d2c", "d2s", "y2c", "y2s", "y3c", "y3s", "y4c", "y4s", "n"))
}
R <- sapply(specs, getsp)   # rows = stats, cols = ortho / cardioem / derm

co     <- function(k) paste(sprintf("%.3f", R[k, ]), collapse = " & ")
se_row <- function(k) paste(sprintf("(%.3f)", R[k, ]), collapse = " & ")
N      <- paste(format(round(R["n", ]), big.mark = ","), collapse = " & ")

writeLines(c(
  "\\begin{tabular}{lccc}",
  "\\toprule",
  " & \\shortstack{Orthopedic\\\\Surgery} & Cardiology & Dermatology \\\\",
  "\\midrule",
  paste("First-year tie &", co("d1c"), "\\\\"),
  paste(" &", se_row("d1s"), "\\\\"),
  paste("\\quad with characteristics &", co("d2c"), "\\\\"),
  paste(" &", se_row("d2s"), "\\\\"),
  "\\addlinespace",
  "\\multicolumn{4}{l}{\\textit{By post-move year, with characteristics}} \\\\",
  paste("\\quad Year 2 &", co("y2c"), "\\\\"),
  paste(" &", se_row("y2s"), "\\\\"),
  paste("\\quad Year 3 &", co("y3c"), "\\\\"),
  paste(" &", se_row("y3s"), "\\\\"),
  paste("\\quad Year 4 &", co("y4c"), "\\\\"),
  paste(" &", se_row("y4s"), "\\\\"),
  "\\addlinespace",
  paste("Observations &", N, "\\\\"),
  "Specialist, PCP, year fixed effects & Yes & Yes & Yes \\\\",
  "\\bottomrule",
  "\\end{tabular}"), "results/tables/persist.tex")
message("Wrote results/tables/persist.tex")
