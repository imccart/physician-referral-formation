# Construct logit data set for everyone -------------------------------------------
df_ref_big <-
  df_full_referrals %>%
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

df_ref_big <- df_ref_big %>%
  left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group, hrr, hrrcity, hrrstate), ~., .names = "doc_{.col}")) %>% select(starts_with("doc_"), npi, year), 
              by = c("doctor" = "npi", "Year"="year")) %>%
  left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group, hrr, hrrcity, hrrstate), ~., .names = "spec_{.col}")) %>% select(starts_with("spec_"), npi, year), 
              by = c("specialist" = "npi", "Year"="year")) %>%
  left_join(df_race %>% select(npi, spec_race=rf_pred_race),
            by=c("specialist"="npi")) %>%
  left_join(df_race %>% select(npi, doc_race=rf_pred_race),
            by=c("doctor"="npi")) %>%
  left_join(df_phycompare %>% select(npi, doc_grad_year=grad_year, doc_med_school=med_school),
            by=c("doctor"="npi")) %>%           
  left_join(df_phycompare %>% select(npi, spec_grad_year=grad_year, spec_med_school=med_school),
            by=c("specialist"="npi")) %>%            
  left_join(zip_ll, by = c("doc_zip"  = "zip"))  %>%
  rename(lat_doc  = lat, lon_doc  = lon) %>%
  left_join(zip_ll, by = c("spec_zip" = "zip"))  %>%
  rename(lat_spec = lat, lon_spec = lon) %>%
  mutate(
    dist_km = geodist::geodist(
                cbind(lon_doc,  lat_doc),
                cbind(lon_spec, lat_spec),
                paired  = TRUE,                # element-wise, not all-pairs
                measure = "haversine") / 1000,  # convert metres → km
         dist_miles = dist_km * 0.621371) %>%
  select(-c(lat_doc, lon_doc, lat_spec, lon_spec, dist_km)) %>%
  left_join(
    df_full_referrals %>%                    # only observed referrals
      distinct(doctor, specialist, Year) %>%
      mutate(referral = 1L),
    by = c("doctor", "specialist", "Year")
  ) %>%
  mutate(referral = coalesce(referral, 0L))       # convert NA → 0


## Final data for logit regression
final_ref_big <-
  df_ref_big %>%
  mutate(
    same_sex      = as.integer(doc_sex  == spec_sex),
    same_male     = as.integer(doc_sex  == spec_sex & doc_sex=="M"),
    same_female   = as.integer(doc_sex  == spec_sex & doc_sex=="F"),
    same_race     = as.integer(doc_race == spec_race),
    same_black    = as.integer(doc_race == spec_race & doc_race=="black"),
    same_asian    = as.integer(doc_race == spec_race & doc_race=="asian"),
    same_hisp     = as.integer(doc_race == spec_race & doc_race=="hispanic"),
    same_white    = as.integer(doc_race == spec_race & doc_race=="white"),
    same_school   = as.integer(doc_med_school == spec_med_school),
    same_prac     = as.integer(doc_group      == spec_group),
    same_zip      = as.integer(doc_zip        == spec_zip),
    diff_age      = abs(as.numeric(doc_birth)     - as.numeric(spec_birth)),
    diff_gradyear = abs(as.numeric(doc_grad_year) - as.numeric(spec_grad_year)),
    diff_dist     = abs(dist_miles)
  ) %>%
  left_join(spec_quality, by = "specialist")

# Construct logit data set for movers ---------------------------------------------

final_ref_movers <- final_ref_big %>%
  inner_join(df_movers, by = c("doctor"="npi", "Year" = "year")) %>%
  filter(origin != spec_hrr)                   # drop referrals back to origin

write.csv(final_ref_movers, sprintf("data/output/df_logit_movers_%s.csv", current_specialty), row.names=FALSE)



