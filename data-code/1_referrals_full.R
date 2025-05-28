# Construct full referral data set ---------------------------------------------

df_full_referrals <- df_referrals %>%
  left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group, hrr, hrrcity, hrrstate), ~., .names = "doc_{.col}")) %>% select(starts_with("doc_"), npi, year), 
              by = c("Practice_ID" = "npi", "Year"="year")) %>%
  left_join(df_mdppas %>% mutate(across(c(sex, birth, spec, zip, group, hrr, hrrcity, hrrstate), ~., .names = "spec_{.col}")) %>% select(starts_with("spec_"), npi, year), 
              by = c("Specialist_ID" = "npi", "Year"="year")) %>%
  left_join(df_race %>% select(npi, spec_race=rf_pred_race),
            by=c("Specialist_ID"="npi")) %>%
  left_join(df_race %>% select(npi, doc_race=rf_pred_race),
            by=c("Practice_ID"="npi")) %>%
  left_join(df_phycompare %>% select(npi, doc_grad_year=grad_year, doc_med_school=med_school),
            by=c("Practice_ID"="npi")) %>%           
  left_join(df_phycompare %>% select(npi, spec_grad_year=grad_year, spec_med_school=med_school),
            by=c("Specialist_ID"="npi")) %>%            
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
  rename(doctor=Practice_ID, specialist=Specialist_ID)

write_csv(df_full_referrals, "data/output/df_full_referrals.csv", na = "")
