# Construct referral data only among movers ---------------------------------

## Identify movers in MD-PPAS data
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
    filter(n_unique == 2) %>%                # moved exactly once in two year period
    rowwise() %>%                            # row-wise neighbour test
    filter(!destination %in% neighbor_lookup[[as.character(origin)]]) %>%
    ungroup() %>%
    mutate(year = yr) %>%
    select(npi, year, origin, destination)
})

## Merge movers into full data, avoiding referrals back to origin HRR (e.g., splits between origin and destination)
df_ref_initial <-
  df_full_referrals %>%
  inner_join(df_movers, 
            by = c("doctor"="npi", "Year" = "year")) %>%
  filter(origin != spec_hrr, doc_hrr==spec_hrr)

df_mover_counts <- df_ref_initial %>% distinct(doctor, Year) %>% group_by(Year) %>%
  summarise(n_movers = n(), .groups = "drop")


write.csv(df_mover_counts, sprintf("data/output/df_movers_%s.csv", current_specialty), row.names=FALSE)
write.csv(df_ref_initial, sprintf("data/output/df_initial_referrals_%s.csv", current_specialty), row.names=FALSE)
