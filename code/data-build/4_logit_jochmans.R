# Construct the Jochmans logistic regression data -----------------------------------

covars <- c("same_sex", "same_male", "same_female",
            "same_race", "same_white", "same_black", "same_asian", "same_hisp",
            "same_school", "same_prac", "same_zip",
            "diff_age", "diff_gradyear","diff_dist","doc_hrr", "spec_hrr")

## data.table implementation — avoids vctrs bugs entirely
make_block <- function(block) {

  yr <- block$Year[1]
  hr <- block$doc_hrr[1]

  docs  <- unique(block$doctor)
  specs <- unique(block$specialist)

  if (length(docs) < 2 || length(specs) < 2) return(data.table())

  look <- as.data.table(block)[, c("doctor", "specialist", "referral", covars), with = FALSE]

  doc_pairs  <- t(combn(docs,  2))
  spec_pairs <- t(combn(specs, 2))

  n_dp <- nrow(doc_pairs)
  n_sp <- nrow(spec_pairs)

  ## chunk large grids to avoid OOM
  chunk_size <- if (n_dp * n_sp > 500000) max(1L, floor(500000 / n_sp)) else n_dp
  dp_chunks  <- split(seq_len(n_dp), ceiling(seq_len(n_dp) / chunk_size))

  results <- lapply(dp_chunks, function(dp_idx) {
    grid <- CJ(dp = dp_idx, sp = seq_len(n_sp))
    grid[, c("i1", "i2") := .(doc_pairs[dp, 1], doc_pairs[dp, 2])]
    grid[, c("j1", "j2") := .(spec_pairs[sp, 1], spec_pairs[sp, 2])]

    ## four-way join: (i1,j1), (i1,j2), (i2,j1), (i2,j2)
    for (suf in c("11", "12", "21", "22")) {
      dcol <- paste0("i", substr(suf, 1, 1))
      scol <- paste0("j", substr(suf, 2, 2))
      tmp <- copy(look)
      setnames(tmp, c("doctor", "specialist", "referral", covars),
               c(dcol, scol, paste0("y", suf), paste0(covars, suf)))
      grid <- merge(grid, tmp, by = c(dcol, scol), all.x = TRUE)
    }

    ## filter discordant quartets
    grid[, dy1 := y11 - y12]
    grid[, dy2 := y21 - y22]
    grid <- grid[dy1 * dy2 < 0]

    if (nrow(grid) == 0) return(data.table())

    ## compute contrasts
    grid[, referral := ((dy1 - dy2) / 2 + 1) / 2]
    for (v in covars) {
      grid[, (v) := get(paste0(v, "11")) - get(paste0(v, "12")) -
                     get(paste0(v, "21")) + get(paste0(v, "22"))]
    }

    setnames(grid, c("i1", "j1", "i2", "j2"), c("doc1", "spec1", "doc2", "spec2"))
    grid[, year := yr]
    grid[, hrr := hr]
    grid[, c("doc1", "spec1", "doc2", "spec2", "referral", covars, "year", "hrr"), with = FALSE]
  })

  results <- results[vapply(results, nrow, integer(1)) > 0L]
  if (length(results) > 0) rbindlist(results) else data.table()
}

## split by Year x doc_hrr, process, combine
groups <- split(as.data.table(final_ref_movers), by = c("Year", "doc_hrr"))
joch_parts <- lapply(groups, make_block)
joch_parts <- joch_parts[vapply(joch_parts, nrow, integer(1)) > 0L]
df_jochmans <- if (length(joch_parts) > 0) rbindlist(joch_parts) else data.table()
rm(groups, joch_parts); gc()

df_jochmans <- as_tibble(df_jochmans) %>%
  left_join(spec_quality %>% rename(spec1_qual=spec_qual, spec1_total_patients=total_spec_patients), by = c("spec1"="specialist")) %>%
  left_join(spec_quality %>% rename(spec2_qual=spec_qual, spec2_total_patients=total_spec_patients), by = c("spec2"="specialist"))

write_csv(df_jochmans, sprintf("data/output/df_jochmans_%s.csv", current_specialty), na = "")
rm(final_ref_movers, df_jochmans); gc()
