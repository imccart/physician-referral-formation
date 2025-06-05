# Construct referrals data by time period after move ---------------------------------

## prep: movers who switch HRR exactly once
df_move_dates <- df_movers %>%          # <- already excludes neighbour moves
  group_by(npi) %>%
  summarise(
    origin      = first(origin),        # unique because n_unique == 1
    destination = first(destination),
    move_year   = first(year),          # year of the unique move
    n_moves     = n_distinct(year),
    .groups     = "drop"
  ) %>%
  filter(n_moves == 1)

## merge movers with ALL referrals
df_ref_initial_cuml <- df_full_referrals %>%    # every PCP–spec–year row
  inner_join(df_move_dates,
             by = c("doctor" = "npi")) %>%      # add move_year, origin, dest
  filter(origin != spec_hrr) %>%                # drop referrals back to origin
  mutate(years_since_move = Year - move_year) %>%  # 0 = year of move
  filter(years_since_move >= 0)                  # keep move year and after

## split into 1- to 5-year “network” windows
ref_windows <- map(1:5, function(k) {
  df_ref_initial_cuml %>%
    filter(years_since_move >= 0,              # keep moves and after
           years_since_move <  k) %>%          # <k ⇒ first k years
    mutate(window = paste0("Up to year ", k))
}) %>%
  bind_rows()

#  ref_windows now contains an extra column `window` taking values
#  "Year0-0", "Year0-1", …, "Year0-4", so you can analyse the
#  referral network in the 1-, 2-, …, 5-year horizon simply by
#  filtering on `window`.

## example: count distinct specialists per PCP in each window
net_size_by_window <- ref_windows %>%
  group_by(window, doctor) %>%
  summarise(n_specs = n_distinct(specialist), .groups = "drop") %>%
  group_by(window) %>%
  summarise(mean_degree = mean(n_specs))

print(net_size_by_window)

write.csv(ref_windows, "data/output/df_initial_referrals_cuml.csv", row.names=FALSE)


# Construct logit data analogously ----------------------------------------

## -- helper that adds covariates & build X’s --------------------
add_covars <- function(df) {
  df %>%
    left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group,
                                            hrr, hrrcity, hrrstate),
                                          ~., .names = "doc_{.col}")) %>%
                select(starts_with("doc_"), npi, year),
              by = c("doctor" = "npi", "Year" = "year")) %>%
    left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group,
                                            hrr, hrrcity, hrrstate),
                                          ~., .names = "spec_{.col}")) %>%
                select(starts_with("spec_"), npi, year),
              by = c("specialist" = "npi", "Year" = "year")) %>%
    left_join(df_race  %>% select(npi, spec_race = rf_pred_race),
              by = c("specialist" = "npi")) %>%
    left_join(df_race  %>% select(npi, doc_race  = rf_pred_race),
              by = c("doctor"     = "npi")) %>%
    left_join(df_phycompare %>%
                select(npi, doc_grad_year  = grad_year,
                             doc_med_school = med_school),
              by = c("doctor"     = "npi")) %>%
    left_join(df_phycompare %>%
                select(npi, spec_grad_year = grad_year,
                             spec_med_school = med_school),
              by = c("specialist" = "npi")) %>%
    left_join(zip_ll, by = c("doc_zip"  = "zip")) %>%
    rename(lat_doc  = lat, lon_doc  = lon) %>%
    left_join(zip_ll, by = c("spec_zip" = "zip")) %>%
    rename(lat_spec = lat, lon_spec = lon) %>%
    mutate(dist_km = geodist::geodist(
                cbind(lon_doc,  lat_doc),
                cbind(lon_spec, lat_spec),
                paired  = TRUE,                # element-wise, not all-pairs
                measure = "haversine") / 1000,  # convert metres → km
         dist_miles = dist_km * 0.621371) %>%
    select(-c(lat_doc, lon_doc, lat_spec, lon_spec, dist_km)) %>%
    left_join(spec_quality, by = "specialist") %>%
    mutate(
      same_sex      = as.integer(doc_sex  == spec_sex),
      same_male     = as.integer(doc_sex  == "M" & same_sex == 1),
      same_female   = as.integer(doc_sex  == "F" & same_sex == 1),
      same_race     = as.integer(doc_race == spec_race),
      same_black    = as.integer(doc_race == "black"    & same_race == 1),
      same_asian    = as.integer(doc_race == "asian"    & same_race == 1),
      same_hisp     = as.integer(doc_race == "hispanic" & same_race == 1),
      same_white    = as.integer(doc_race == "white"    & same_race == 1),
      same_school   = as.integer(doc_med_school == spec_med_school),
      same_prac     = as.integer(doc_group      == spec_group),
      same_zip      = as.integer(doc_zip        == spec_zip),
      diff_age      = abs(as.numeric(doc_birth)     - as.numeric(spec_birth)),
      diff_gradyear = abs(as.numeric(doc_grad_year) - as.numeric(spec_grad_year)),
      diff_dist     = abs(dist_miles)
    )
}

## -- expand & attach referrals for EACH window -----------------
df_ref_windows <- df_full_referrals %>%
  filter(doc_hrr==spec_hrr) %>%     
  group_by(Year, doc_hrr) %>%
  summarise(
    combos = list(expand_grid(
      doctor     = unique(doctor),
      specialist = unique(specialist)
    )),
    .groups = "drop"
  ) %>%
  select(Year, combos) %>%      # discard hrr_doc; keep only the list-col
  unnest(combos)

df_ref_windows <- df_ref_windows %>%
  inner_join(df_move_dates,
             by = c("doctor" = "npi")) %>%         # add move_year, origin, dest
  mutate(years_since_move = Year - move_year) %>%  # 0 = year of move
  filter(years_since_move >= 0) %>%                # keep move year and after
  add_covars() %>%
  filter(origin != spec_hrr) %>%                   # drop referrals back to origin
  left_join(
    df_full_referrals %>%                          # only observed referrals
      distinct(doctor, specialist, Year) %>%
      mutate(referral = 1L),
    by = c("doctor", "specialist", "Year")
  ) %>%
  mutate(referral = coalesce(referral, 0L))       # convert NA → 0


## split into 1- to 5-year “network” windows
final_ref_windows <- map(1:5, function(k) {
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
