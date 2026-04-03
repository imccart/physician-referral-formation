# Construct referrals data by time period after move ---------------------------------

## prep: movers who switch HRR exactly once
move_intervals <- df_movers %>%          # one row per (doctor, move_year)
  arrange(npi, year) %>%                 # chronological within doctor
  group_by(npi) %>%
  mutate(
    Year_start = year,
    Year_end   = lead(year, default = 2018 + 1L) - 1L   # up to year before next move
  ) %>%
  ungroup() %>%
  transmute(
    doctor    = npi,
    move_year = Year_start,
    Year_start,
    Year_end,
    origin,
    destination
  )

## merge movers with ALL referrals
df_ref_initial_cuml <- df_full_referrals %>%    # every PCP–spec–year row
  filter(doc_hrr==spec_hrr) %>%
  inner_join(move_intervals,
             by = "doctor",
             relationship="many-to-many") %>%      # add move_year, origin, dest
  filter(origin != spec_hrr) %>%                # drop referrals back to origin
  filter(Year >= Year_start & Year <= Year_end) %>%
  mutate(years_since_move = Year - move_year)

## split into 0- to 5-year "network" windows
ref_windows <- map(1:6, function(k) {
  df_ref_initial_cuml %>%
    filter(years_since_move >= 0,              # keep moves and after
           years_since_move <  k) %>%          # <k ⇒ first k years
    mutate(window = paste0("Up to year ", k))
}) %>%
  bind_rows()


## example: count distinct specialists per PCP in each window
net_size_by_window <- ref_windows %>%
  group_by(window, doctor) %>%
  summarise(n_specs = n_distinct(specialist), .groups = "drop") %>%
  group_by(window) %>%
  summarise(mean_degree = mean(n_specs))

print(net_size_by_window)

write.csv(ref_windows, sprintf("data/output/df_initial_referrals_cuml_%s.csv", current_specialty), row.names=FALSE)
rm(df_ref_initial_cuml, ref_windows); gc()

# Construct logit data analogously ----------------------------------------

df_ref_windows <- final_ref_big %>%
  inner_join(move_intervals,
             by = "doctor",
             relationship="many-to-many") %>%      # add move_year, origin, dest
  filter(origin != spec_hrr) %>%                   # drop referrals back to origin
  filter(Year >= Year_start & Year <= Year_end) %>%
  mutate(years_since_move = Year - move_year)


## split into 1- to 6-year "network" windows
final_ref_windows <- map(1:6, function(k) {
  df_ref_windows %>%
    filter(years_since_move <  k) %>%              # <k ⇒ first k years
    mutate(window = paste0("Up to year ", k))
}) %>%
  bind_rows()

write_csv(final_ref_windows, sprintf("data/output/df_logit_windows_%s.csv", current_specialty), na = "")
rm(df_ref_windows, final_ref_big, df_full_referrals); gc()

# Construct quardruple data for each window ----------------------------

covars <- c("same_sex","same_male","same_female",
            "same_race","same_white","same_black","same_asian","same_hisp",
            "same_school","same_prac","same_zip",
            "diff_age","diff_gradyear","diff_dist",
            "doc_hrr","spec_hrr")

## data.table implementation — avoids vctrs bugs entirely
make_block_win <- function(block) {

  win <- block$window[1]
  yr  <- block$Year[1]
  hr  <- block$doc_hrr[1]

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
    grid[, window := win]
    grid[, c("doc1", "spec1", "doc2", "spec2", "referral", covars, "year", "hrr", "window"), with = FALSE]
  })

  results <- results[vapply(results, nrow, integer(1)) > 0L]
  if (length(results) > 0) rbindlist(results) else data.table()
}

## build quadruple data year-by-year and window-by-window, writing to disk incrementally
dt_windows <- as.data.table(final_ref_windows)
rm(final_ref_windows); gc()

years   <- sort(unique(dt_windows$Year))
windows <- sort(unique(dt_windows$window))
out_file <- sprintf("data/output/df_jochmans_windows_%s.csv", current_specialty)
first_write <- TRUE

for (yr in years) {
  for (win in windows) {
    message("  Quartets for year ", yr, ", ", win)
    dt_sub <- dt_windows[Year == yr & window == win]
    groups <- split(dt_sub, by = "doc_hrr")
    yw_parts <- lapply(groups, make_block_win)
    yw_parts <- yw_parts[vapply(yw_parts, nrow, integer(1)) > 0L]
    if (length(yw_parts) > 0) {
      yw_result <- rbindlist(yw_parts)
      fwrite(yw_result, out_file, append = !first_write)
      first_write <- FALSE
      rm(yw_result)
    }
    rm(dt_sub, groups, yw_parts); gc()
  }
}

rm(dt_windows); gc()

## read back, attach specialist quality, and re-save
df_jochmans_windows <- as_tibble(fread(out_file))
df_jochmans_windows <- df_jochmans_windows %>%
  left_join(spec_quality %>% rename(spec1_qual = spec_qual,
                                    spec1_total_patients = total_spec_patients),
            by = c("spec1" = "specialist")) %>%
  left_join(spec_quality %>% rename(spec2_qual = spec_qual,
                                    spec2_total_patients = total_spec_patients),
            by = c("spec2" = "specialist"))

write_csv(df_jochmans_windows, out_file, na = "")
