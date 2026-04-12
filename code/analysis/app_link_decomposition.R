## Appendix: Link Decomposition (Referee Comment 3a)
## For each window k = 2,...,4, split links into "persisting" (present in k-1)
## and "new" (not in k-1). Report same_prac, same_sex, same_race, mean distance.

win_links_decomp <- ref_windows %>%
  distinct(window, doctor, specialist,
           doc_group, spec_group,
           doc_sex, spec_sex,
           doc_race, spec_race,
           dist_miles,
           years_since_move) %>%
  mutate(k = as.integer(sub("Up to year ", "", window))) %>%
  filter(k <= 4)

decomp_stats <- function(df) {
  df %>%
    summarise(
      n_links     = n(),
      same_prac   = mean(doc_group == spec_group, na.rm = TRUE),
      same_gender = mean(doc_sex == spec_sex, na.rm = TRUE),
      same_race   = mean(doc_race == spec_race, na.rm = TRUE),
      mean_dist   = mean(dist_miles, na.rm = TRUE),
      .groups = "drop"
    )
}

decomp_results <- map(2:4, function(kk) {
  prev_links <- win_links_decomp %>%
    filter(k == kk - 1) %>%
    distinct(doctor, specialist)

  curr_links <- win_links_decomp %>%
    filter(k == kk)

  persisting <- curr_links %>%
    semi_join(prev_links, by = c("doctor", "specialist")) %>%
    decomp_stats() %>%
    mutate(window = kk, type = "Persisting")

  new <- curr_links %>%
    anti_join(prev_links, by = c("doctor", "specialist")) %>%
    decomp_stats() %>%
    mutate(window = kk, type = "New")

  bind_rows(persisting, new)
}) %>% bind_rows()

# Year 1 (all links are "new" by definition)
yr1 <- win_links_decomp %>%
  filter(k == 1) %>%
  decomp_stats() %>%
  mutate(window = 1, type = "New")

decomp_results <- bind_rows(yr1, decomp_results)

# Reshape for LaTeX table
decomp_wide <- decomp_results %>%
  mutate(across(c(same_prac, same_gender, same_race),
                ~ scales::percent(.x, accuracy = 0.1)),
         mean_dist = round(mean_dist, 1)) %>%
  select(window, type, same_prac, same_gender, same_race, mean_dist, n_links) %>%
  arrange(window, desc(type))

# Build LaTeX table with grouped rows
tab_rows <- decomp_wide %>%
  mutate(label = paste0("Year ", window, " -- ", type)) %>%
  select(label, same_prac, same_gender, same_race, mean_dist, n_links) %>%
  mutate(n_links = formatC(n_links, format = "d", big.mark = ","))

tab_rows %>%
  mutate(across(everything(), ~ gsub("%", "\\\\%", .x))) %>%
  kable(format = "latex", booktabs = TRUE, linesep = "",
        align = c("l", "r", "r", "r", "r", "r"),
        escape = FALSE,
        col.names = c(" ",
                      "\\shortstack[r]{Same \\\\ practice}",
                      "\\shortstack[r]{Same \\\\ gender}",
                      "\\shortstack[r]{Same \\\\ race}",
                      "\\shortstack[r]{Mean \\\\ distance}",
                      "\\shortstack[r]{N \\\\ links}")) %>%
  writeLines(sprintf("results/tables/app_link_decomposition_%s.tex", current_specialty))
