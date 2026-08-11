# Empirical-Bayes shrinkage of the specialist quality measures.
#
# The VRDC export reports, for each specialist, the patient count and the
# mean and standard deviation of four residualized outcomes (non-elective
# admissions, all-cause admissions, two-year mortality, and total spending).
# Raw means are imprecise for low-volume specialists, so we shrink each mean
# toward its specialty average by the ratio of between-specialist variance to
# total variance, estimated by method of moments within specialty. Cardiology
# (as distinct from the combined cardiology-EP group) is dropped: its cells
# are thin and it does not appear in the referral networks.

quality <- read_csv("data/output/vrdc_specialist_quality.csv", show_col_types = FALSE) %>%
  filter(specialty != "cardio")

long <- quality %>%
  select(Specialist_NPI, specialty, n_pat,
         m_admit_ne, sd_admit_ne, m_admit, sd_admit,
         m_death, sd_death, m_spend, sd_spend) %>%
  pivot_longer(-c(Specialist_NPI, specialty, n_pat),
               names_to = c(".value", "outcome"),
               names_pattern = "(m|sd)_(.*)") %>%
  mutate(v = sd^2 / n_pat)                              # sampling variance of the mean

shrunk <- long %>%
  group_by(specialty, outcome) %>%
  mutate(mu    = weighted.mean(m, 1 / v),
         tau2  = pmax(0, var(m) - mean(v)),             # between-specialist variance
         m_eb  = mu + tau2 / (tau2 + v) * (m - mu),
         se_eb = sqrt(tau2 / (tau2 + v) * v)) %>%
  ungroup()

shrunk %>%
  select(Specialist_NPI, specialty, n_pat, outcome, m_eb, se_eb) %>%
  pivot_wider(names_from = outcome, values_from = c(m_eb, se_eb),
              names_glue = "{.value}_{outcome}") %>%
  write_csv("data/output/vrdc_specialist_quality_eb.csv")
