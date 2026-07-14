# Meta --------------------------------------------------------------------
## Title:        Estimation-sample comparison (discordant-quadruple sample vs excluded)
## Description:  For each specialty, compares the first-stage identifying sample
##               (providers and pairs entering the discordant quadruples) against the
##               excluded remainder. Writes one combined table across all three
##               specialties: results/tables/quad_comparison_all.tex.
## Self-contained: reads per-specialty outputs from data/output/. Runs after the build,
##                 alongside the other cross-specialty post-scripts.

specs <- c(ortho = "Orthopedics", cardioem = "Cardiology", derm = "Dermatology")

one_spec <- function(sp) {
  twfe  <- read_csv(sprintf("data/output/df_jochmans_%s.csv", sp),         show_col_types = FALSE)
  logit <- read_csv(sprintf("data/output/df_logit_movers_%s.csv", sp),      show_col_types = FALSE)
  init  <- read_csv(sprintf("data/output/df_initial_referrals_%s.csv", sp), show_col_types = FALSE)

  in_npis <- unique(c(twfe$doc1, twfe$doc2, twfe$spec1, twfe$spec2))
  corners <- bind_rows(
    twfe %>% transmute(doctor = doc1, specialist = spec1, Year = year),
    twfe %>% transmute(doctor = doc1, specialist = spec2, Year = year),
    twfe %>% transmute(doctor = doc2, specialist = spec1, Year = year),
    twfe %>% transmute(doctor = doc2, specialist = spec2, Year = year)
  ) %>% distinct() %>% mutate(included = 1L)

  ref_counts <- init %>% group_by(npi = doctor) %>% summarise(deg = n_distinct(specialist), .groups = "drop")

  doc_w <- init %>% select(doctor, starts_with("doc_"), total_pcp_patients, Year) %>%
    rename_with(~ gsub("^doc_", "", .x), .cols = starts_with("doc_")) %>%
    rename(npi = doctor, total_patients = total_pcp_patients) %>% mutate(qual = NA_real_) %>%
    distinct(npi, .keep_all = TRUE)
  spec_w <- init %>% select(specialist, starts_with("spec_"), total_spec_patients, Year) %>%
    rename_with(~ gsub("^spec_", "", .x), .cols = starts_with("spec_")) %>%
    rename(npi = specialist, total_patients = total_spec_patients) %>%
    distinct(npi, .keep_all = TRUE)

  prov <- bind_rows(
      doc_w  %>% mutate(role = "doctor",     spec = as.character(spec)),
      spec_w %>% mutate(role = "specialist", spec = as.character(spec))
    ) %>%
    mutate(include = as.integer(npi %in% in_npis)) %>%
    left_join(ref_counts, by = "npi") %>%
    group_by(role, include) %>%
    summarise(
      age = mean(Year - birth, na.rm = TRUE), exper = mean(Year - grad_year, na.rm = TRUE),
      qual = mean(qual, na.rm = TRUE), male = mean(sex == "M", na.rm = TRUE),
      white = mean(race == "white", na.rm = TRUE), black = mean(race == "black", na.rm = TRUE),
      hisp = mean(race == "hispanic", na.rm = TRUE), asian = mean(race == "asian", na.rm = TRUE),
      patients = mean(total_patients, na.rm = TRUE), refs = mean(deg, na.rm = TRUE),
      n = n_distinct(npi), .groups = "drop"
    )

  pair <- logit %>%
    select(doctor, specialist, Year, same_prac, dist_miles, same_sex, same_race, referral) %>%
    left_join(corners, by = c("doctor", "specialist", "Year")) %>%
    mutate(include = replace_na(included, 0L)) %>%
    group_by(include) %>%
    summarise(
      same_prac = mean(same_prac, na.rm = TRUE), dist = mean(dist_miles, na.rm = TRUE),
      same_sex = mean(same_sex, na.rm = TRUE), same_race = mean(same_race, na.rm = TRUE),
      ref_rate = mean(referral, na.rm = TRUE), n = n(), .groups = "drop"
    )

  list(prov = prov, pair = pair)
}

res <- lapply(names(specs), one_spec)
names(res) <- names(specs)


# Row layout: panel, label, source (doctor/specialist/pair), field, format ---
rows <- tribble(
  ~panel,        ~label,                  ~src,         ~field,      ~fmt,
  "PCPs",        "Mean age",              "doctor",     "age",       "mean",
  "PCPs",        "Years of experience",   "doctor",     "exper",     "mean",
  "PCPs",        "Share male",            "doctor",     "male",      "share",
  "PCPs",        "Share white",           "doctor",     "white",     "share",
  "PCPs",        "Share Black",           "doctor",     "black",     "share",
  "PCPs",        "Share Hispanic",        "doctor",     "hisp",      "share",
  "PCPs",        "Share Asian",           "doctor",     "asian",     "share",
  "PCPs",        "Mean total patients",   "doctor",     "patients",  "mean",
  "PCPs",        "Mean unique referrals", "doctor",     "refs",      "mean",
  "PCPs",        "Number of PCPs",        "doctor",     "n",         "count",
  "Specialists", "Mean age",              "specialist", "age",       "mean",
  "Specialists", "Years of experience",   "specialist", "exper",     "mean",
  "Specialists", "Mean quality",          "specialist", "qual",      "share",
  "Specialists", "Share male",            "specialist", "male",      "share",
  "Specialists", "Share white",           "specialist", "white",     "share",
  "Specialists", "Share Black",           "specialist", "black",     "share",
  "Specialists", "Share Hispanic",        "specialist", "hisp",      "share",
  "Specialists", "Share Asian",           "specialist", "asian",     "share",
  "Specialists", "Mean total patients",   "specialist", "patients",  "mean",
  "Specialists", "Number of specialists", "specialist", "n",         "count",
  "Pairs",       "Share same practice",   "pair",       "same_prac", "share",
  "Pairs",       "Mean distance (miles)", "pair",       "dist",      "mean",
  "Pairs",       "Share same gender",     "pair",       "same_sex",  "share",
  "Pairs",       "Share same race",       "pair",       "same_race", "share",
  "Pairs",       "Referral rate",         "pair",       "ref_rate",  "share",
  "Pairs",       "Number of pairs",       "pair",       "n",         "count"
)

getval <- function(sp, src, field, inc) {
  d <- if (src == "pair") filter(res[[sp]]$pair, include == inc)
       else filter(res[[sp]]$prov, role == src, include == inc)
  v <- d %>% pull(.data[[field]])
  if (length(v) == 0) NA_real_ else v
}
fmt1 <- function(v, fmt) {
  if (is.na(v)) return("")
  if (fmt == "share") sprintf("%.3f", v)
  else if (fmt == "mean") sprintf("%.1f", v)
  else formatC(round(v), format = "d", big.mark = ",")
}

mat <- matrix("", nrow = nrow(rows), ncol = 6)
for (i in seq_len(nrow(rows))) {
  r <- rows[i, ]; k <- 1
  for (sp in names(specs)) {
    mat[i, k]     <- fmt1(getval(sp, r$src, r$field, 1L), r$fmt); k <- k + 1
    mat[i, k]     <- fmt1(getval(sp, r$src, r$field, 0L), r$fmt); k <- k + 1
  }
}

tbl <- tibble(` ` = rows$label,
              o_i = mat[,1], o_e = mat[,2], c_i = mat[,3], c_e = mat[,4], d_i = mat[,5], d_e = mat[,6])

n1 <- sum(rows$panel == "PCPs"); n2 <- sum(rows$panel == "Specialists"); n3 <- sum(rows$panel == "Pairs")

quad_tex <- kable(
    tbl, format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
    align = c("l", rep("r", 6)),
    col.names = c(" ", "Incl.", "Excl.", "Incl.", "Excl.", "Incl.", "Excl.")
  ) %>%
  add_header_above(c(" " = 1, "Orthopedics" = 2, "Cardiology" = 2, "Dermatology" = 2)) %>%
  pack_rows("Panel A: PCPs",        1,           n1) %>%
  pack_rows("Panel B: Specialists", n1 + 1,      n1 + n2) %>%
  pack_rows("Panel C: Pairs",       n1 + n2 + 1, n1 + n2 + n3)

writeLines(as.character(quad_tex), "results/tables/quad_comparison_all.tex")
message("Wrote quad_comparison_all.tex")
