# Assemble the quality-reallocation results table from the bootstrap output.
# One panel per specialty, determinants in rows, and spending, non-elective
# admissions, and mortality in columns, each as the point estimate with its 95%
# bootstrap interval. The table reports the across-PCP effect; the
# practice-affiliation effect among PCPs with an in-practice specialist is
# discussed in the text.

# Practice affiliation is reported among PCPs who have a same-practice specialist
# available (the effect is a mechanical zero for the rest); the other
# determinants are reported across all PCPs.
ci <- read_csv("results/tables/quality_realloc_ci.csv", show_col_types = FALSE) %>%
  filter((determinant == "same_prac" & sample == "tied") |
         (determinant != "same_prac" & sample == "all"))

det  <- c(same_prac = "Practice affiliation", diff_dist = "Distance",
          same_sex = "Gender", same_race = "Race",
          diff_age = "Age", diff_gradyear = "Experience")
spec <- c(ortho = "Orthopedic Surgery", cardioem = "Cardiology", derm = "Dermatology")
outs <- c("spend", "admit_ne", "death")

z <- function(x, d) sub("^-0(\\.0+)?$", "0\\1", formatC(x, format = "f", digits = d))

body <- ci %>%
  filter(determinant %in% names(det), outcome %in% outs) %>%
  # Report each specialty per its own mover referral volume: 10,000 in cardiology
  # and dermatology, 2,000 in orthopedic surgery. Spending (dollars per referral)
  # scales to millions (point * scale / 1e6); non-elective admissions and mortality
  # (per 1,000 patients) scale to that volume (point * scale / 1e3).
  mutate(scale = c(ortho = 2000, cardioem = 10000, derm = 10000)[as.character(specialty)],
         txt = if_else(outcome == "spend",
                       paste0(z(point*scale/1e6, 2), " [", z(lo*scale/1e6, 2), ", ", z(hi*scale/1e6, 2), "]"),
                       paste0(z(point*scale/1e3, 1), " [", z(lo*scale/1e3, 1), ", ", z(hi*scale/1e3, 1), "]")),
         determinant = factor(determinant, names(det)),
         outcome = factor(outcome, outs),
         specialty = factor(specialty, names(spec))) %>%
  arrange(specialty, determinant) %>%
  select(specialty, determinant, outcome, txt) %>%
  pivot_wider(names_from = outcome, values_from = txt) %>%
  mutate(Determinant = det[as.character(determinant)])

hdr <- c("",
         "\\shortstack{Spending\\\\(\\$ millions)}",
         "\\shortstack{Non-elective\\\\admissions}",
         "\\shortstack{Two-year\\\\mortality}")

kable(body %>% select(Determinant, spend, admit_ne, death),
      format = "latex", booktabs = TRUE, linesep = "", escape = FALSE,
      col.names = hdr, align = "lccc") %>%
  pack_rows(index = c("Orthopedic Surgery (per 2,000 referrals)" = 6,
                      "Cardiology (per 10,000 referrals)" = 6,
                      "Dermatology (per 10,000 referrals)" = 6)) %>%
  as.character() %>%
  writeLines("results/tables/quality_realloc_all.tex")

body %>%
  transmute(specialty, Determinant, `Spending ($M)` = spend,
            `Non-elec adm` = admit_ne, Mortality = death) %>%
  print(n = 40)
