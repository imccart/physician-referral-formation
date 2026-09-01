# 7_practice_composition.R
# Distinct (practice group, year, specialty) roster for the practices that appear as
# a study PCP's practice, written for the coordination secondary analysis
# (code/analysis/8_coordination.R). Uses the in-session df_mdppas; restricts to mover
# practices to keep the artifact small. Sourced once by _BuildData.R after the
# per-specialty build loop.

mover_groups <- c("ortho", "cardioem", "derm") %>%
  map(~ read_csv(sprintf("data/output/df_logit_movers_%s.csv", .x),
                 col_select = doc_group,
                 col_types = cols(doc_group = col_character())) %>%
        pull(doc_group)) %>%
  unlist() %>% unique()

df_mdppas %>%
  filter(!is.na(group), !is.na(spec), group %in% mover_groups) %>%
  distinct(doc_group = group, Year = year, spec) %>%
  write_csv("data/output/practice_composition.csv")

message(sprintf("practice_composition.csv written for %s practices",
                format(length(mover_groups), big.mark = ",")))
