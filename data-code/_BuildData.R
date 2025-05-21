# Meta --------------------------------------------------------------------
## Title:         Formation of Physician Referral Networks
## Author:        Ian McCarthy
## Date Created:  1/23/2025
## Date Edited:   5/21/2025


# Preliminaries -----------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(tidyverse, readr, sf, spdep)

# Import data -------------------------------------------------------------

## zip and crosswalks
df_hrr <- read_csv("data/input/ZipHsaHrr18.csv") %>%
  mutate(zip = sprintf("%05s", zipcode18)) %>%
  select(zip, hrrnum, hrrcity, hrrstate) %>%
  rename(
    hrr = hrrnum,
    hrrcity = hrrcity,
    hrrstate = hrrstate
  )

## shapefile and contiguity matrix
gdf <- st_read("data/input/HRR_ShapeFile.shp") %>%
  filter(!str_starts(HRRCITY, "AK") & !str_starts(HRRCITY, "HI"))

ggplot(data = gdf) +
  geom_sf() +
  theme_minimal() +
  labs(title = "HRR Shapefile Map")

contig_nb  <- poly2nb(as(gdf, "Spatial"), row.names = gdf$HRRNUM)

neighbor_lookup <- contig_nb |>
  map(~ as.integer(gdf$HRRNUM[.x])) |>          # indices → HRR codes
  set_names(as.character(gdf$HRRNUM))           # names = HRR codes

## MDPPAS
df_mdppas <- data.frame()

for (year in 2009:2018) {
  # Read MDPPAS data
  df <- read_csv(sprintf("data/input/MDPPAS/PhysicianData_%d.csv", year), 
                 col_types = cols_only(
                   npi = col_number(),
                   sex = col_character(),
                   birth_dt = col_character(),
                   spec_prim_1 = col_character(),
                   phy_zip_perf1 = col_character(),
                   Year = col_integer(),
                   group1 = col_character()
                 )) %>%
    rename(
      npi = npi,
      sex = sex,
      birth = birth_dt,
      spec = spec_prim_1,
      zip = phy_zip_perf1,
      year = Year,
      group = group1
    ) %>%
    # Drop NPI duplicates and missing zip codes
    distinct(npi, .keep_all = TRUE) %>%
    drop_na(zip) %>%
    # Fix zip codes and birth dates
    mutate(
      zip = str_pad(zip, 5, pad = "0"),
      birth = str_sub(birth, -4),
      group = as.character(group)
    )
  
  df_mdppas <- bind_rows(df_mdppas, df)
}

df_mdppas  <- as_tibble(df_mdppas)

# Referral data
df_referrals <- read_csv("data/input/referrals/ReferralPairs.csv")

# physician race data
df_race <- read_csv("data/input/physician-race/final-npi-combined.csv")

# Merge MDPPAS with HRR
df_mdppas <- left_join(df_mdppas, df_hrr, by = "zip")


# Construct full referral data set ---------------------------------------------

df_full_referrals <- df_referrals %>%
  left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group, hrr, hrrcity, hrrstate), ~., .names = "doc_{.col}")) %>% select(starts_with("doc_"), npi, year), 
              by = c("Practice_ID" = "npi", "Year"="year")) %>%
  left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group, hrr, hrrcity, hrrstate), ~., .names = "spec_{.col}")) %>% select(starts_with("spec_"), npi, year), 
              by = c("Specialist_ID" = "npi", "Year"="year"))
  left_join(df_race %>% select(npi, spec_race=rf_pred_race),
            by=c("Specialist_ID"="npi")) %>%
  left_join(df_race %>% select(npi, doc_race=rf_pred_race),
            by=c("Practice_ID"="npi"))




write_csv(df_full_referrals, "data/output/df_full_referrals.csv", na = "")


# Identify movers ---------------------------------------------------------

df_movers <- map_dfr(2010:2018, function(yr) {

  df_mdppas %>%                              # cols: npi, hrr, year
    filter(year %in% c(yr - 1, yr)) %>%
    arrange(npi, year) %>%
    group_by(npi) %>%
    summarise(
      origin      = first(hrr),
      destination = last(hrr),
      n_unique    = n_distinct(hrr),
      .groups     = "drop"
    ) %>%
    filter(n_unique == 2) %>%                # moved exactly once
    rowwise() %>%                            # row-wise neighbour test
    filter(!destination %in% neighbor_lookup[[as.character(origin)]]) %>%
    ungroup() %>%
    mutate(year = yr) %>%
    select(npi, year, origin, destination)
})



# Idenfity initial referrals -----------------------------------------------------

## 1.  keep only referrals made by physicians who moved that year

df_ref_initial <-
  map_dfr(2010:2018, \(yr) {

    ref_movers <- df_referrals %>%                     # cols: Practice_ID, Specialist_ID, Year
      inner_join(df_movers %>% filter(year == yr),     # cols: npi, year, origin, destination
                 by = c("Practice_ID" = "npi",
                        "Year"        = "year")) %>%
      transmute(
        year        = Year,            # rename & order columns
        doctor      = Practice_ID,
        specialist  = Specialist_ID,
        origin,
        destination
      )

    message(sprintf(
      "Year: %d, Movers: %d, Specialists: %d",
      yr,
      n_distinct(ref_movers$doctor),
      n_distinct(ref_movers$specialist)
    ))

    ref_movers
  })

## 2.  add characteristics for referring doctors and for specialists
df_ref_initial <-
  df_ref_initial %>%
  left_join(df_mdppas,                       # characteristics for doctors
            by = c("doctor" = "npi", "year" = "year")) %>%
  left_join(df_mdppas,                       # characteristics for specialists
            by = c("specialist" = "npi", "year" = "year"),
            suffix = c("_doc", "_spec"))


df_ref_initial <-
  df_ref_initial %>%
  drop_na() %>%                                    # drop any rows with missing values
  filter(origin != hrr_spec)


# Form all possible referral pairs ----------------------------------------

## 1.  Every doctor–specialist combination within each (year, HRR)
df_ref_all <-
  df_ref_initial %>%                             # cols: year, hrr_doc, doctor, specialist
  group_by(year, hrr_doc) %>%
  summarise(
    combos = list(expand_grid(
      doctor     = unique(doctor),
      specialist = unique(specialist)
    )),
    .groups = "drop"
  ) %>%
  select(year, combos) %>%      # discard hrr_doc; keep only the list-col
  unnest(combos)  


## 2.  add characteristics for doctors and specialists
keep_vars <- c("npi", "sex", "birth", "group", "zip", "year", "hrr")

df_ref_all <- df_ref_all %>%
  left_join(df_mdppas %>% select(all_of(keep_vars)),           # referring doctor
            by = c("doctor" = "npi", "year"  = "year")) %>%
  left_join(df_mdppas %>% select(all_of(keep_vars)),           # specialist
            by = c("specialist" = "npi", "year" = "year"),
            suffix = c("_doc", "_spec"))


## 3.  indicator for whether the pair is an *observed* referral
df_ref_all <- df_ref_all %>%
  left_join(
    df_ref_initial %>%                    # only observed referrals
      distinct(doctor, specialist, year) %>%
      mutate(referral = 1L),
    by = c("doctor", "specialist", "year")
  ) %>%
  mutate(referral = coalesce(referral, 0L))       # convert NA → 0



# Import physician compare data --------------------------------------------


## 0.  column indices and names
##     (Python used zero-based indices; add 1 for R’s one-based)
cols_idx  <- c(0, 7, 9, 10, 18, 25, 27) + 1      # → 1, 8, 10, 11, 19, 26, 28
new_cols  <- c("npi", "gender", "med_school", "grad_year",
               "group_pac_id", "zip_code", "hosp_affiliation")

years      <- 2013:2018
pc_files   <- sprintf("data/input/Physician_Compare/%d/%d_Q4.csv", years, years)


## 1.  read, rename, and tag each year’s file
df_physicians <-
  map2_dfr(pc_files, years, \(file, yr) {

    readr::read_csv(
      file,
      col_types = cols(.default = col_character())   # read everything as character
    ) %>%
      select(all_of(cols_idx)) %>%                   # keep only the desired columns
      set_names(new_cols) %>%                        # rename
      mutate(
        year     = yr,
        zip_code = substr(zip_code, 1, 5)            # 5-digit ZIP
      )
  })


## 2.  clean up duplicates / blanks and coerce types
df_physicians <- df_physicians %>%
  distinct() %>%                                     # drop exact duplicate rows
  mutate(across(everything(),
                ~ na_if(trimws(.x), ""))) %>%        # blank → NA
  mutate(
    year      = as.integer(year),
    grad_year = as.numeric(grad_year),
    npi       = as.numeric(npi)
  )


## 3. final clean up
df_physicians <- df_physicians %>%          # object from the previous step
  arrange(year) %>%                         # ensure earlier years come first
  group_by(npi) %>%                         
  slice_tail(n = 1) %>%                     
  ungroup() %>%
  mutate(
    med_school = na_if(med_school, "OTHER")
  ) %>%
  select(npi, grad_year, med_school) %>%
  left_join(df_race %>%
              select(npi, race_common_pred=common_pred, race_true=true_race, 
                     race_rf=rf_pred_race, race_zocdoc=zocdoc_race),
            by="npi")

# Merge referrals with physician data -------------------------------------
final_ref <-
  df_ref_all %>%
  left_join(
    df_physicians %>%                 # rename vars -> *_doc
      rename_with(~ paste0(.x, "_doc"), -npi),
    by = c("doctor" = "npi")
  ) %>%
  left_join(
    df_physicians %>%                 # rename vars -> *_spec
      rename_with(~ paste0(.x, "_spec"), -npi),
    by = c("specialist" = "npi")
  ) %>%
  mutate(
    same_sex      = as.integer(sex_doc  == sex_spec),
    same_race     = as.integer(race_rf_doc == race_rf_spec),
    diff_age      = abs(as.numeric(birth_doc)     - as.numeric(birth_spec)),
    diff_gradyear = abs(as.numeric(grad_year_doc) - as.numeric(grad_year_spec)),
    same_school   = as.integer(med_school_doc == med_school_spec),
    same_prac     = as.integer(group_doc      == group_spec),
    same_zip      = as.integer(zip_doc        == zip_spec),
    same_male     = as.integer(sex_doc  == sex_spec & sex_doc=="M"),
    same_female   = as.integer(sex_doc  == sex_spec & sex_doc=="F"),
    same_black    = as.integer(race_rf_doc == race_rf_spec & race_rf_doc=="black"),
    same_asian    = as.integer(race_rf_doc == race_rf_spec & race_rf_doc=="asian"),
    same_hisp    = as.integer(race_rf_doc == race_rf_spec & race_rf_doc=="hispanic"),
    same_white    = as.integer(race_rf_doc == race_rf_spec & race_rf_doc=="white")
  )

write.csv(final_ref, "data/output/movers_referrals.csv", row.names=FALSE)




# Construct the logistic regression data -----------------------------------


covars <- c("same_sex", "same_race", "diff_age", "diff_gradyear",
            "same_school", "same_prac", "same_zip",
            "same_male", "same_female", "same_white", "same_black", "same_asian", "same_hisp")

make_block <- function(block) {

  yr <- block$year[1]
  hr <- block$hrr_doc[1]

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
  group_by(year, hrr_doc) %>%
  group_split() %>%
  map_dfr(make_block)

write_csv(df_logit, "data/output/df_logit.csv", na = "")

