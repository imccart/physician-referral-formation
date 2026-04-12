# Period-specific (non-cumulative) data construction (Referee Comment 3b)
# Reads existing df_logit_windows_{spec}.csv, extracts year-specific subsets,
# builds quartets. No need to re-run the full data build.
#
# Usage: source after 0-setup.R, with current_specialty set.
#   source("code/0-setup.R")
#   for (current_specialty in c("ortho", "cardioem", "derm")) {
#     source("code/data-build/6_period_windows.R")
#   }

message("=== Period-specific windows for: ", current_specialty, " ===")

covars <- c("same_sex", "same_male", "same_female",
            "same_race", "same_white", "same_black", "same_asian", "same_hisp",
            "same_school", "same_prac", "same_zip",
            "diff_age", "diff_gradyear", "diff_dist",
            "doc_hrr", "spec_hrr")

# Columns needed for quartet construction
keep_cols <- c("Year", "doctor", "specialist", "referral",
               covars, "years_since_move", "window")

infile <- sprintf("data/output/df_logit_windows_%s.csv", current_specialty)

# Quartet construction (copied from 5_referrals_by_time.R)
make_block_period <- function(block) {
  yr  <- block$Year[1]
  hr  <- block$doc_hrr[1]
  per <- block$period[1]

  docs  <- unique(block$doctor)
  specs <- unique(block$specialist)

  if (length(docs) < 2 || length(specs) < 2) return(data.table())

  look <- block[, c("doctor", "specialist", "referral", covars), with = FALSE]

  doc_pairs  <- t(combn(docs,  2))
  spec_pairs <- t(combn(specs, 2))

  n_dp <- nrow(doc_pairs)
  n_sp <- nrow(spec_pairs)

  chunk_size <- if (n_dp * n_sp > 500000) max(1L, floor(500000 / n_sp)) else n_dp
  dp_chunks  <- split(seq_len(n_dp), ceiling(seq_len(n_dp) / chunk_size))

  results <- lapply(dp_chunks, function(dp_idx) {
    grid <- CJ(dp = dp_idx, sp = seq_len(n_sp))
    grid[, c("i1", "i2") := .(doc_pairs[dp, 1], doc_pairs[dp, 2])]
    grid[, c("j1", "j2") := .(spec_pairs[sp, 1], spec_pairs[sp, 2])]

    for (suf in c("11", "12", "21", "22")) {
      dcol <- paste0("i", substr(suf, 1, 1))
      scol <- paste0("j", substr(suf, 2, 2))
      tmp <- copy(look)
      setnames(tmp, c("doctor", "specialist", "referral", covars),
               c(dcol, scol, paste0("y", suf), paste0(covars, suf)))
      grid <- merge(grid, tmp, by = c(dcol, scol), all.x = TRUE)
    }

    grid[, dy1 := y11 - y12]
    grid[, dy2 := y21 - y22]
    grid <- grid[dy1 * dy2 < 0]

    if (nrow(grid) == 0) return(data.table())

    grid[, referral := ((dy1 - dy2) / 2 + 1) / 2]
    for (v in covars) {
      grid[, (v) := get(paste0(v, "11")) - get(paste0(v, "12")) -
                     get(paste0(v, "21")) + get(paste0(v, "22"))]
    }

    setnames(grid, c("i1", "j1", "i2", "j2"), c("doc1", "spec1", "doc2", "spec2"))
    grid[, year := yr]
    grid[, hrr := hr]
    grid[, period := per]
    grid[, c("doc1", "spec1", "doc2", "spec2", "referral", covars, "year", "hrr", "period"), with = FALSE]
  })

  results <- results[vapply(results, nrow, integer(1)) > 0L]
  if (length(results) > 0) rbindlist(results) else data.table()
}

# Build period-specific quartets for periods 1-4
# Read one period at a time to limit memory on large files
out_file <- sprintf("data/output/df_jochmans_period_%s.csv", current_specialty)
first_write <- TRUE

for (k in 1:4) {
  ysm <- k - 1
  message("  Period ", k, " (years_since_move = ", ysm, "): reading data...")

  dt_period <- fread(infile, select = keep_cols)
  dt_period <- dt_period[window == "Up to year 4" & years_since_move == ysm]
  dt_period[, c("window", "years_since_move") := NULL]
  dt_period[, period := paste0("Year ", k, " only")]
  gc()

  message("    ", nrow(dt_period), " rows")

  years <- sort(unique(dt_period$Year))

  for (yr in years) {
    dt_sub <- dt_period[Year == yr]
    groups <- split(dt_sub, by = "doc_hrr")
    parts <- lapply(groups, make_block_period)
    parts <- parts[vapply(parts, nrow, integer(1)) > 0L]
    if (length(parts) > 0) {
      result <- rbindlist(parts)
      fwrite(result, out_file, append = !first_write)
      first_write <- FALSE
      rm(result)
    }
    rm(dt_sub, groups, parts); gc()
  }
  rm(dt_period); gc()
}

# Write period-specific choice-set data (for Stage 2 FE recovery)
# Read only the columns needed
cs_cols <- c("Year", "doctor", "specialist", "referral",
             "same_sex", "same_race", "same_prac", "diff_dist",
             "diff_age", "diff_gradyear", "doc_hrr",
             "years_since_move", "window")

df_cs <- fread(infile, select = cs_cols)
df_cs <- df_cs[window == "Up to year 4" & years_since_move <= 3]
df_cs[, period := paste0("Year ", years_since_move + 1, " only")]
df_cs[, c("window", "years_since_move") := NULL]
fwrite(df_cs, sprintf("data/output/df_logit_period_%s.csv", current_specialty))

rm(df_cs); gc()
message("=== Done: ", current_specialty, " period windows ===")
