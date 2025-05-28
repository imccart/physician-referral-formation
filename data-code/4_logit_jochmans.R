# Construct the Jochmans logistic regression data -----------------------------------

covars <- c("same_sex", "same_male", "same_female", 
            "same_race", "same_white", "same_black", "same_asian", "same_hisp",
            "same_school", "same_prac", "same_zip",
            "diff_age", "diff_gradyear","diff_dist","doc_hrr", "spec_hrr")

make_block <- function(block) {

  yr <- block$Year[1]
  hr <- block$doc_hrr[1]

  docs  <- unique(block$doctor)
  specs <- unique(block$specialist)

  # need at least two doctors & two specialists to form a 2×2 table
  if (length(docs) < 2 || length(specs) < 2) return(tibble())

  # lookup table (doctor, specialist) → y and X
  look <- block %>%
    select(doctor, specialist, referral, all_of(covars))

  # all unordered pairs of doctors / specialists
  doc_pairs  <- t(combn(docs,  2))
  spec_pairs <- t(combn(specs, 2))

  # expand to every doc-pair × spec-pair combination
  grid <- expand_grid(
           dp = seq_len(nrow(doc_pairs)),
           sp = seq_len(nrow(spec_pairs))) |>
          mutate(
            i1 = doc_pairs[dp, 1], i2 = doc_pairs[dp, 2],
            j1 = spec_pairs[sp, 1], j2 = spec_pairs[sp, 2]
          )

  # join a function: attach (y,X) for one of the four "corners"
  join_xy <- function(df, d, s, suf) {
    df |>
      left_join(
        look,
        by = setNames(c("doctor", "specialist"), c(d, s))  # left d/s → right doctor/specialist
      ) |>
      rename(y = referral) |>
      rename_with(~ paste0(.x, suf), all_of(covars)) |>
      rename("{paste0('y', suf)}" := y)
  }

  # attach y11, y12, y21, y22 + corresponding X's
  grid <- grid |>
            join_xy("i1", "j1", "11") |>
            join_xy("i1", "j2", "12") |>
            join_xy("i2", "j1", "21") |>
            join_xy("i2", "j2", "22")

  # keep discordant tables only (opposite rank orders)
  grid <- grid |>
    mutate(
      dy1  = y11 - y12,
      dy2  = y21 - y22,
      keep = dy1 * dy2 < 0
    ) |>
    filter(keep)

  if (nrow(grid) == 0) return(tibble())

  # dependent variable (0/1)
  grid <- grid |>
    mutate(referral = ((dy1 - dy2) / 2 + 1) / 2)

  # covariate contrasts  Δx = (x11 − x12) − (x21 − x22)
  for (v in covars) {
    grid <- grid |>
      mutate("{v}" :=
               (get(paste0(v, "11")) - get(paste0(v, "12"))) -
               (get(paste0(v, "21")) - get(paste0(v, "22"))))
  }

## ---- keep the NPIs that form the 2×2 comparison ------------
  grid |>
    select(doc1=i1, spec1=j1, doc2=i2, spec2=j2, referral, all_of(covars)) |>
    mutate(year = yr, hrr = hr)
}

df_logit <- final_ref %>%
  group_by(Year, doc_hrr) %>%
  group_split() %>%
  map_dfr(make_block)

write_csv(df_logit, "data/output/df_logit_jochmans.csv", na = "")

