# Construct the Jochmans logistic regression data -----------------------------------

covars <- c("same_sex", "same_male", "same_female", 
            "same_race", "same_white", "same_black", "same_asian", "same_hisp",
            "same_school", "same_prac", "same_zip",
            "diff_age", "diff_gradyear","diff_dist","doc_hrr", "spec_hrr")

## chunks large grids to avoid OOM (identical results, just computed in pieces)
make_block <- function(block) {

  yr <- block$Year[1]
  hr <- block$doc_hrr[1]

  docs  <- unique(block$doctor)
  specs <- unique(block$specialist)

  if (length(docs) < 2 || length(specs) < 2) return(tibble())

  look <- block %>%
    select(doctor, specialist, referral, all_of(covars))

  doc_pairs  <- t(combn(docs,  2))
  spec_pairs <- t(combn(specs, 2))

  n_dp <- nrow(doc_pairs)
  n_sp <- nrow(spec_pairs)

  join_xy <- function(df, d, s, suf) {
    df %>%
      left_join(look,
                by = setNames(c("doctor", "specialist"), c(d, s))) %>%
      rename(y = referral) %>%
      rename_with(~ paste0(.x, suf), all_of(covars)) %>%
      rename("{paste0('y', suf)}" := y)
  }

  chunk_size <- if (n_dp * n_sp > 500000) max(1L, floor(500000 / n_sp)) else n_dp
  dp_chunks  <- split(seq_len(n_dp), ceiling(seq_len(n_dp) / chunk_size))

  map(dp_chunks, function(dp_idx) {
    grid <- expand_grid(dp = dp_idx, sp = seq_len(n_sp)) %>%
      mutate(i1 = doc_pairs[dp, 1], i2 = doc_pairs[dp, 2],
             j1 = spec_pairs[sp, 1], j2 = spec_pairs[sp, 2])

    grid <- grid %>%
      join_xy("i1", "j1", "11") %>%
      join_xy("i1", "j2", "12") %>%
      join_xy("i2", "j1", "21") %>%
      join_xy("i2", "j2", "22") %>%
      mutate(dy1  = y11 - y12,
             dy2  = y21 - y22,
             keep = dy1 * dy2 < 0) %>%
      filter(keep)

    if (nrow(grid) == 0) return(tibble())

    grid <- grid %>%
      mutate(referral = ((dy1 - dy2) / 2 + 1) / 2)

    for (v in covars) {
      grid <- grid %>%
        mutate("{v}" :=
                 (get(paste0(v, "11")) - get(paste0(v, "12"))) -
                 (get(paste0(v, "21")) - get(paste0(v, "22"))))
    }

    grid %>%
      select(doc1=i1, spec1=j1, doc2=i2, spec2=j2, referral, all_of(covars)) %>%
      mutate(year = yr, hrr = hr)
  }) %>% bind_rows()
}

df_jochmans <- final_ref_movers %>%
  group_by(Year, doc_hrr) %>%
  group_split() %>%
  map(make_block) %>%
  bind_rows()

df_jochmans <- df_jochmans %>%
  ungroup() %>%
  left_join(spec_quality %>% rename(spec1_qual=spec_qual, spec1_total_patients=total_spec_patients), by = c("spec1"="specialist")) %>%
  left_join(spec_quality %>% rename(spec2_qual=spec_qual, spec2_total_patients=total_spec_patients), by = c("spec2"="specialist"))

write_csv(df_jochmans, sprintf("data/output/df_jochmans_%s.csv", current_specialty), na = "")
rm(final_ref_movers, df_jochmans); gc()

