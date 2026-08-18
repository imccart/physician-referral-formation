
# 5_peer_referrals.R — Peer referrals: practice affiliation vs peer information
# Tests whether same_prac operates through peer information (other PCPs in the
# practice already refer to that specialist) vs organizational structure.
# Conditional logit with PCP and year fixed effects, plus the full-sample
# specialist fixed effect (specialist_fe.R) entered as an offset so the mechanism
# analysis conditions on specialist attractiveness, consistent with the two-stage
# main design. Self-contained: reads its own data and loops all three specialties.
# Sourced once from the _main.R post block.

run_specs <- c("ortho", "cardioem", "derm")

for (peer_spec in run_specs) {

message("Peer referral analysis: ", peer_spec)

# Read data
df_full <- read_csv(sprintf("data/output/df_full_referrals_%s.csv", peer_spec),
                    show_col_types = FALSE)
df_logit_peer <- read_csv(sprintf("data/output/df_logit_movers_%s.csv", peer_spec),
                          show_col_types = FALSE)

# Step 1: Build peer referral counts from full referral data ----------------
# Count distinct PCPs per (group, specialist, year) in the PRIOR year (t-1)
# This captures the practice's existing referral network before the mover arrives

peer_counts <- df_full %>%
  filter(!is.na(doc_group) & doc_group != "") %>%
  group_by(doc_group, specialist, Year) %>%
  summarise(n_referring_pcps = n_distinct(doctor), .groups = "drop") %>%
  mutate(Year = Year + 1) %>%    # shift forward so it merges as t-1 counts onto year t
  rename(peer_referrals = n_referring_pcps)


# Step 2: Merge onto mover choice set --------------------------------------
# For mover PCP i in group g, specialist j, year t:
#   peer_referrals = number of other PCPs in group g who referred to j in year t-1

df_peer <- df_logit_peer %>%
  left_join(peer_counts, by = c("doc_group", "specialist", "Year")) %>%
  mutate(peer_referrals = replace_na(peer_referrals, 0L))


# Step 2b: Merge the full-sample specialist fixed effect (specialist_fe.R) ---
# Entered below as an offset (coefficient fixed at 1), the same device the
# two-stage recovery uses. Specialists absent from the table (never estimable)
# default to 0, the center of the winsorized table.

spec_fe_tab <- read_csv(sprintf("data/output/specialist_fe_%s.csv", peer_spec),
                        show_col_types = FALSE) %>%
  transmute(spec_key = as.character(specialist), gamma_j)
df_peer <- df_peer %>%
  mutate(spec_key = as.character(specialist)) %>%
  left_join(spec_fe_tab, by = "spec_key") %>%
  mutate(gamma_spec = coalesce(gamma_j, 0)) %>%
  select(-spec_key, -gamma_j)
message("  Specialist FE match: ",
        format(sum(df_peer$gamma_spec != 0), big.mark = ","), " of ",
        format(nrow(df_peer), big.mark = ","), " choice-set rows")


# Step 3: Three conditional logit specs, specialist FE held fixed as offset --

covars_a <- c("same_sex", "same_race", "same_prac", "dist_miles",
              "diff_age", "diff_gradyear")

covars_b <- c("same_sex", "same_race", "same_prac", "dist_miles",
              "diff_age", "diff_gradyear", "peer_referrals")

covars_c <- c("same_sex", "same_race", "dist_miles",
              "diff_age", "diff_gradyear", "peer_referrals")

peer_a <- feglm(
  referral ~ same_sex + same_race + same_prac + dist_miles +
    diff_age + diff_gradyear | Year + doctor,
  data = df_peer,
  offset = ~gamma_spec,
  vcov = ~doctor,
  family = binomial(link = "logit")
)

peer_b <- feglm(
  referral ~ same_sex + same_race + same_prac + dist_miles +
    diff_age + diff_gradyear + peer_referrals | Year + doctor,
  data = df_peer,
  offset = ~gamma_spec,
  vcov = ~doctor,
  family = binomial(link = "logit")
)

peer_c <- feglm(
  referral ~ same_sex + same_race + dist_miles +
    diff_age + diff_gradyear + peer_referrals | Year + doctor,
  data = df_peer,
  offset = ~gamma_spec,
  vcov = ~doctor,
  family = binomial(link = "logit")
)


# Step 4: Manual MFX via first-difference + delta-method SEs ---------------
# eta includes the specialist offset (gamma_spec) alongside Xbeta and the FEs.

compute_mfx_peer <- function(model, data, spec_covars) {
  beta <- coef(model)[spec_covars]
  V    <- vcov(model)[spec_covars, spec_covars]

  Xmat    <- as.matrix(data[, spec_covars])
  na_mask <- complete.cases(Xmat)
  dat_v   <- data[na_mask, ]
  Xbeta   <- drop(Xmat[na_mask, ] %*% beta)

  fes     <- fixef(model)
  doc_fe  <- fes$doctor[as.character(dat_v$doctor)]
  year_fe <- fes$Year[as.character(dat_v$Year)]
  eta     <- Xbeta + doc_fe + year_fe + dat_v$gamma_spec

  ok    <- !is.na(eta)
  eta   <- eta[ok]
  dat_v <- dat_v[ok, ]

  # Referral-probability weight, matching the main two-stage MFX (2_logit_twfe.R):
  # predicted probability with same-practice off, normalized to sum to 1, so the peer
  # marginal effects are on the same scale as the headline estimates rather than a flat
  # mean diluted over the near-impossible pairs. In the column that drops same_prac, eta
  # already excludes it, so nothing is subtracted.
  sp_contrib <- if ("same_prac" %in% spec_covars) beta[["same_prac"]] * dat_v[["same_prac"]] else 0
  w_base <- plogis(eta - sp_contrib)
  w_base <- w_base / sum(w_base)

  bin_vars <- c("same_sex", "same_race", "same_prac", "new_specialist")

  map(spec_covars, function(v) {
    is_bin <- v %in% bin_vars
    delta  <- 1

    if (is_bin) {
      eta1 <- eta - beta[v] * dat_v[[v]] + beta[v]
      eta0 <- eta - beta[v] * dat_v[[v]]
    } else {
      eta1 <- eta + beta[v] * delta
      eta0 <- eta
    }

    p1 <- plogis(eta1)
    p0 <- plogis(eta0)
    dp <- sum(w_base * (p1 - p0))

    grad_i <- matrix(0, nrow = length(eta), ncol = length(spec_covars))
    colnames(grad_i) <- spec_covars
    for (k in spec_covars) {
      if (is_bin) {
        deta1_dk <- if (k == v) rep(1, length(eta)) else dat_v[[k]]
        deta0_dk <- if (k == v) rep(0, length(eta)) else dat_v[[k]]
      } else {
        deta1_dk <- dat_v[[k]] + if (k == v) delta else 0
        deta0_dk <- dat_v[[k]]
      }
      grad_i[, k] <- (p1 * (1 - p1)) * deta1_dk - (p0 * (1 - p0)) * deta0_dk
    }

    g_bar <- colSums(grad_i * w_base)
    se <- sqrt(as.numeric(t(g_bar) %*% V %*% g_bar))

    tibble(term = v, estimate = dp, std.error = se)
  }) %>% bind_rows()
}

mfx_a <- compute_mfx_peer(peer_a, df_peer, covars_a)
mfx_b <- compute_mfx_peer(peer_b, df_peer, covars_b)
mfx_c <- compute_mfx_peer(peer_c, df_peer, covars_c)


# Step 5: Build output table -----------------------------------------------

mfx_terms <- c("same_sex", "same_race", "same_prac",
               "dist_miles", "diff_age", "diff_gradyear",
               "peer_referrals")

mfx_labels <- c("Same gender", "Same race", "Same practice group",
                "Distance (miles)", "Age difference", "Experience difference",
                "Peer referrals")

fmt_peer <- function(x) {
  if (is.na(x) || length(x) == 0) return("")
  s <- sprintf("%.3f", abs(x))
  if (x < 0) paste0("-", s) else s
}

extract_peer <- function(mfx_obj, terms) {
  df <- as.data.frame(mfx_obj)
  out <- setNames(rep("", length(terms)), terms)
  se  <- setNames(rep("", length(terms)), terms)
  for (v in terms) {
    row <- df[df$term == v, ]
    if (nrow(row) == 1) {
      out[v] <- fmt_peer(row$estimate)
      se[v]  <- paste0("(", sprintf("%.3f", row$std.error), ")")
    }
  }
  list(est = out, se = se)
}

m_a <- extract_peer(mfx_a, mfx_terms)
m_b <- extract_peer(mfx_b, mfx_terms)
m_c <- extract_peer(mfx_c, mfx_terms)

tbl_rows <- map(seq_along(mfx_terms), function(i) {
  bind_rows(
    tibble(term = mfx_labels[i],
           `(1)` = m_a$est[i], `(2)` = m_b$est[i], `(3)` = m_c$est[i]),
    tibble(term = "",
           `(1)` = m_a$se[i],  `(2)` = m_b$se[i],  `(3)` = m_c$se[i])
  )
}) %>% bind_rows()

footer <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Year FE", "Yes", "Yes", "Yes",
  "Doctor FE", "Yes", "Yes", "Yes",
  "Specialist FE", "Yes", "Yes", "Yes",
  "Observations", format(nobs(peer_a), big.mark = ","),
                  format(nobs(peer_b), big.mark = ","),
                  format(nobs(peer_c), big.mark = ",")
)

tbl_all <- bind_rows(tbl_rows, footer)

kable(tbl_all, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
      align = c("l", "r", "r", "r"),
      col.names = c("", "(1)", "(2)", "(3)")) %>%
  save_kable(sprintf("results/tables/peer_referrals_%s.tex", peer_spec))

message("  Peer referral table saved: peer_referrals_", peer_spec, ".tex")


# Cleanup
rm(df_full, df_logit_peer, peer_counts, df_peer, spec_fe_tab,
   peer_a, peer_b, peer_c,
   mfx_a, mfx_b, mfx_c, m_a, m_b, m_c, tbl_rows, footer, tbl_all)
gc()

} # end peer_spec loop
