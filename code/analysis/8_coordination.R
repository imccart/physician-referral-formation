# 8_coordination.R
# Secondary result on the practice-affiliation effect: is the in-house referral pull
# larger where the PCP's practice also houses the specialties a patient of this type
# is likely to need? If organizational convenience partly reflects a coordination
# benefit, the affiliation effect should rise with the number of coordination-relevant
# specialties in the practice. We run the paper's link-level conditional logit (the
# Zeltzer-style specification in 2_logit_twfe.R, doctor + year fixed effects, clustered on
# the PCP) and add shared-practice interacted with the standardized count of coordination-
# relevant specialties in the PCP's practice. Coordination-relevant specialties are the
# cross-domain partners that manage the patient's other conditions (co_set), excluding the
# focal specialty's own diagnostic/surgical subspecialties. Result is reported inline in
# the paper (Section 6.3), not as a table. Self-contained; loops all three specialties;
# sourced in the _main.R post block after 7_within_pcp_avail.R.

co_specs <- c("ortho", "cardioem", "derm")
co_lab   <- c(ortho = "Orthopedic surgery", cardioem = "Cardiology", derm = "Dermatology")

# Coordination-relevant (complementary) specialties per focal specialty (spec_prim_1
# codes). Defined from a combination of clinical co-management sense and the specialties
# that disproportionately share patients with each focal specialty in the CMS shared-
# patient data (lift, from research-data-repo/physician-shared-patients). Paper footnote
# lists these.
co_set <- list(
  cardioem = c("39","46","29","77"),                          # nephro, endo, pulm, vasc surg (cross-domain; cardiac/thoracic surgery excluded as within cardiovascular domain)
  derm     = c("66","83","90"),                               # rheum, heme-onc, med onc (cross-domain systemic; within-skin path/plastics/surg-onc and periocular/H&N ophtho/ENT excluded)
  ortho    = c("25","72"))                                    # PM&R, pain mgmt

comp <- read_csv("data/output/practice_composition.csv",
                 col_types = cols(doc_group = col_character(), Year = col_integer(),
                                  spec = col_character()))

co_est <- vector("list", length(co_specs)); names(co_est) <- co_specs

for (sp in co_specs) {
  message("Coordination test: ", sp)

  d <- read_csv(sprintf("data/output/df_logit_movers_%s.csv", sp),
                col_types = cols(doc_group = col_character(), doc_hrr = col_character())) %>%
    mutate(spec_male = as.numeric(spec_sex == "M"),
           exp_spec  = (Year - spec_grad_year) / 10)

  # count of coordination-relevant specialties in the PCP's practice, standardized
  cnt <- comp %>%
    group_by(doc_group, Year) %>%
    summarise(coord = sum(spec %in% co_set[[sp]]), .groups = "drop")
  d <- d %>%
    left_join(cnt, by = c("doc_group", "Year")) %>%
    mutate(coord = replace_na(coord, 0),
           z_coord = as.numeric(scale(coord)))

  # paper's link-level conditional logit (logit_race3) + coordination interaction.
  # The PCP fixed effect already absorbs practice size and any other PCP-level trait,
  # so no breadth or size term is needed.
  m <- feglm(referral ~ same_sex + same_race + spec_male + exp_spec + same_prac +
               dist_miles + diff_age + diff_gradyear + same_prac:z_coord | Year + doctor,
             data = d, vcov = ~doctor, family = binomial("logit"), glm.iter = 200)

  b <- coef(m); s <- sqrt(diag(vcov(m)))
  co_est[[sp]] <- tibble(
    specialty = sp,
    n_movers  = d %>% distinct(doctor) %>% nrow(),
    n_obs     = nobs(m),
    affil     = b[["same_prac"]],         affil_se = s[["same_prac"]],
    coord     = b[["same_prac:z_coord"]], coord_se = s[["same_prac:z_coord"]])
}

co_all <- bind_rows(co_est)
write_csv(co_all, "results/tables/coordination_est.csv")
# Result reported inline in the paper (Section 6.3), not as a table.

message(sprintf("Coordination interaction (log-odds per SD): %s",
                paste(sprintf("%s coord %.3f (se %.3f)",
                              co_all$specialty, co_all$coord, co_all$coord_se),
                      collapse = "; ")))
