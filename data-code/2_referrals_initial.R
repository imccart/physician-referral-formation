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
    filter(n_unique == 2) %>%                # moved exactly once
    rowwise() %>%                            # row-wise neighbour test
    filter(!destination %in% neighbor_lookup[[as.character(origin)]]) %>%
    ungroup() %>%
    mutate(year = yr) %>%
    select(npi, year, origin, destination)
})

df_ref_movers <-
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

## Merge movers into full data
df_ref_initial <-
  df_full_referrals %>%
  inner_join(df_ref_movers, 
            by = c("doctor", "specialist", "Year" = "year")) %>%
  filter(origin != spec_hrr)

write.csv(df_ref_initial, "data/output/df_initial_referrals.csv", row.names=FALSE)
