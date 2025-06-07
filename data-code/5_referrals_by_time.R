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

## split into 0- to 5-year “network” windows
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

write.csv(ref_windows, "data/output/df_initial_referrals_cuml.csv", row.names=FALSE)


# Construct logit data analogously ----------------------------------------

df_ref_windows <- final_ref_big %>%
  inner_join(move_intervals,
             by = "doctor",
             relationship="many-to-many") %>%      # add move_year, origin, dest
  filter(origin != spec_hrr) %>%                   # drop referrals back to origin
  filter(Year >= Year_start & Year <= Year_end) %>% 
  mutate(years_since_move = Year - move_year) 


## split into 1- to 6-year “network” windows
final_ref_windows <- map(1:6, function(k) {
  df_ref_windows %>%
    filter(years_since_move <  k) %>%              # <k ⇒ first k years
    mutate(window = paste0("Up to year ", k))
}) %>%
  bind_rows()

write_csv(final_ref_windows, "data/output/df_logit_windows.csv", na = "")


# Construct quardruple data for each window ----------------------------

covars <- c("same_sex","same_male","same_female",
            "same_race","same_white","same_black","same_asian","same_hisp",
            "same_school","same_prac","same_zip",
            "diff_age","diff_gradyear","diff_dist",
            "doc_hrr","spec_hrr")

## builder that keeps the window label
make_block_win <- function(block) {

  win <- block$window[1]          # Year0-0 … Year0-4
  yr  <- block$Year[1]
  hr  <- block$doc_hrr[1]

  docs  <- unique(block$doctor)
  specs <- unique(block$specialist)

  if (length(docs) < 2 || length(specs) < 2) return(tibble())

  look <- block %>%
    select(doctor, specialist, referral, all_of(covars))

  doc_pairs  <- t(combn(docs,  2))
  spec_pairs <- t(combn(specs, 2))

  grid <- expand_grid(
           dp = seq_len(nrow(doc_pairs)),
           sp = seq_len(nrow(spec_pairs))) |>
          mutate(i1 = doc_pairs[dp, 1], i2 = doc_pairs[dp, 2],
                 j1 = spec_pairs[sp, 1], j2 = spec_pairs[sp, 2])

  join_xy <- function(df, d, s, suf) {
    df |>
      left_join(look,
                by = setNames(c("doctor","specialist"), c(d,s))) |>
      rename(y = referral) |>
      rename_with(~ paste0(.x, suf), all_of(covars)) |>
      rename("{paste0('y', suf)}" := y)
  }

  grid <- grid |>
            join_xy("i1","j1","11") |>
            join_xy("i1","j2","12") |>
            join_xy("i2","j1","21") |>
            join_xy("i2","j2","22") |>
            mutate(dy1 = y11 - y12,
                   dy2 = y21 - y22,
                   keep = dy1 * dy2 < 0) |>
            filter(keep)

  if (nrow(grid) == 0) return(tibble())

  grid <- grid |>
            mutate(referral = ((dy1 - dy2)/2 + 1)/2)

  for (v in covars) {
    grid <- grid |>
      mutate("{v}" :=
               (get(paste0(v,"11")) - get(paste0(v,"12"))) -
               (get(paste0(v,"21")) - get(paste0(v,"22"))))
  }

  grid |>
    select(doc1=i1,spec1=j1,doc2=i2,spec2=j2,
           referral, all_of(covars)) |>
    mutate(year = yr, hrr = hr, window = win)
}

## build quadruple data for every horizon window
df_jochmans_windows <- final_ref_windows %>%
  group_by(window, Year, doc_hrr) %>%
  group_split() %>%
  map_dfr(make_block_win)

## attach specialist quality and save
df_jochmans_windows <- df_jochmans_windows %>%
  ungroup() %>%
  left_join(spec_quality %>%
              rename(spec1_qual           = spec_qual,
                     spec1_total_patients = total_spec_patients),
            by = c("spec1" = "specialist")) %>%
  left_join(spec_quality %>%
              rename(spec2_qual           = spec_qual,
                     spec2_total_patients = total_spec_patients),
            by = c("spec2" = "specialist"))

write_csv(df_jochmans_windows, "data/output/df_jochmans_windows.csv", na = "")
