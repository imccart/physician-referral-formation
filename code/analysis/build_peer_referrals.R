
# build_peer_referrals.R — Merge peer referral counts onto df_logit and df_logit_twfe
# Sourced per specialty after data loading in _main.R
# Requires: df_full_referrals, df_logit, df_logit_twfe, current_specialty

message("Building peer referral counts for ", current_specialty, "...")

# Step 1: Count distinct PCPs per (group, specialist, year) at t-1
peer_counts <- df_full_referrals %>%
  filter(!is.na(doc_group) & doc_group != "") %>%
  group_by(doc_group, specialist, Year) %>%
  summarise(n_referring_pcps = n_distinct(doctor), .groups = "drop") %>%
  mutate(Year = Year + 1) %>%
  rename(peer_referrals = n_referring_pcps)

# Step 2: Merge onto standard choice set (df_logit)
# df_logit has specialist as factor; peer_counts has it as numeric
peer_counts <- peer_counts %>% mutate(specialist = as.factor(specialist))

df_logit <- df_logit %>%
  left_join(peer_counts, by = c("doc_group", "specialist", "Year")) %>%
  mutate(peer_referrals = replace_na(peer_referrals, 0L))

# Step 3: Merge onto quadruple data (df_logit_twfe)
# Need doctor-level group lookup (from the choice set data)
doc_groups <- df_logit %>%
  select(doctor, doc_group, Year) %>%
  distinct(doctor, Year, .keep_all = TRUE) %>%
  mutate(doctor = as.numeric(as.character(doctor)))

# Build (doctor, specialist, year) -> peer_referrals lookup
# For each doctor, look up their group, then find peer_referrals for that group+specialist+year
# Use numeric specialist to match jochmans data (spec1/spec2 are numeric)
peer_counts_num <- peer_counts %>% mutate(specialist = as.numeric(as.character(specialist)))

peer_lookup <- doc_groups %>%
  inner_join(peer_counts_num, by = c("doc_group", "Year"), relationship = "many-to-many") %>%
  select(doctor, specialist, Year, peer_referrals)

# Look up raw peer_referrals for each of the 4 positions in the quadruple
twfe_pr <- df_logit_twfe %>%
  left_join(peer_lookup, by = c("doc1" = "doctor", "spec1" = "specialist", "year" = "Year")) %>%
  rename(pr_11 = peer_referrals) %>%
  left_join(peer_lookup, by = c("doc1" = "doctor", "spec2" = "specialist", "year" = "Year")) %>%
  rename(pr_12 = peer_referrals) %>%
  left_join(peer_lookup, by = c("doc2" = "doctor", "spec1" = "specialist", "year" = "Year")) %>%
  rename(pr_21 = peer_referrals) %>%
  left_join(peer_lookup, by = c("doc2" = "doctor", "spec2" = "specialist", "year" = "Year")) %>%
  rename(pr_22 = peer_referrals) %>%
  mutate(
    across(c(pr_11, pr_12, pr_21, pr_22), ~ replace_na(.x, 0L)),
    peer_referrals = (pr_11 - pr_12) - (pr_21 - pr_22)
  ) %>%
  select(-pr_11, -pr_12, -pr_21, -pr_22)

df_logit_twfe <- twfe_pr

rm(peer_counts, peer_counts_num, doc_groups, peer_lookup, twfe_pr)

message("  peer_referrals added to df_logit and df_logit_twfe")
