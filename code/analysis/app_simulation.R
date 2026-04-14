
# app_simulation.R — Monte Carlo validation of the two-stage estimation procedure
# Standalone: can be sourced directly or from _main.R
# Not specialty-specific (does not depend on current_specialty)

set.seed(20260414)

# --- DGP parameters ---
N_hrr       <- 15
N_pcp_hrr   <- 25
N_spec_hrr  <- 10
N_years     <- 5
N_mc        <- 100

true_beta <- c(same_prac = 2.5, same_gender = 0.5, dist = -0.1)


# --- Simulation function ---

run_one_sim <- function(sim_id, fe_loading = 0.5, gender_loading = 0.3, fe_sd = 1.0) {

  # Generate PCPs and specialists, assign to HRRs
  # FEs are correlated with covariates: PCPs/specialists in central locations
  # and male specialists have higher FEs, creating OVB when specialist FEs are omitted
  n_pcp  <- N_hrr * N_pcp_hrr
  n_spec <- N_hrr * N_spec_hrr
  n_prac <- N_hrr * 5

  pcp_prac_raw   <- sample(seq_len(n_prac), n_pcp, replace = TRUE)
  pcp_gender_raw <- rbinom(n_pcp, 1, 0.5)
  pcp_x_raw      <- runif(n_pcp, 0, 100)
  pcp_y_raw      <- runif(n_pcp, 0, 100)
  pcp_central    <- 1 - sqrt((pcp_x_raw - 50)^2 + (pcp_y_raw - 50)^2) / 70

  pcp_df <- tibble(
    doctor = seq_len(n_pcp),
    hrr    = rep(seq_len(N_hrr), each = N_pcp_hrr),
    pcp_gender = pcp_gender_raw,
    pcp_x      = pcp_x_raw,
    pcp_y      = pcp_y_raw,
    pcp_prac   = pcp_prac_raw,
    alpha      = fe_loading * pcp_central + rnorm(n_pcp, 0, fe_sd)
  )

  spec_prac_raw   <- sample(seq_len(n_prac), n_spec, replace = TRUE)
  spec_gender_raw <- rbinom(n_spec, 1, 0.5)
  spec_x_raw      <- runif(n_spec, 0, 100)
  spec_y_raw      <- runif(n_spec, 0, 100)
  spec_central    <- 1 - sqrt((spec_x_raw - 50)^2 + (spec_y_raw - 50)^2) / 70

  spec_df <- tibble(
    specialist = seq_len(n_spec),
    hrr        = rep(seq_len(N_hrr), each = N_spec_hrr),
    spec_gender = spec_gender_raw,
    spec_x      = spec_x_raw,
    spec_y      = spec_y_raw,
    spec_prac   = spec_prac_raw,
    gamma       = fe_loading * spec_central + gender_loading * spec_gender_raw + rnorm(n_spec, 0, fe_sd)
  )

  # Build choice sets: each PCP sees all specialists in their HRR, repeated across years
  choice_sets <- map(seq_len(N_years), function(yr) {
    inner_join(
      pcp_df %>% select(doctor, hrr, pcp_gender, pcp_x, pcp_y, pcp_prac, alpha),
      spec_df %>% select(specialist, hrr, spec_gender, spec_x, spec_y, spec_prac, gamma),
      by = "hrr",
      relationship = "many-to-many"
    ) %>% mutate(Year = yr)
  }) %>% bind_rows()

  # Construct covariates
  choice_sets <- choice_sets %>%
    mutate(
      same_prac   = as.numeric(pcp_prac == spec_prac),
      same_gender = as.numeric(pcp_gender == spec_gender),
      dist        = sqrt((pcp_x - spec_x)^2 + (pcp_y - spec_y)^2) / 10
    )

  # True linear index and referral draws
  Xbeta <- choice_sets$same_prac * true_beta["same_prac"] +
           choice_sets$same_gender * true_beta["same_gender"] +
           choice_sets$dist * true_beta["dist"]

  eta_true <- Xbeta + choice_sets$alpha + choice_sets$gamma
  choice_sets$prob <- plogis(eta_true)
  choice_sets$referral <- rbinom(nrow(choice_sets), 1, choice_sets$prob)

  # --- True MFX (computed from DGP) ---
  true_mfx <- c(
    same_prac   = mean(plogis(eta_true - choice_sets$same_prac * true_beta["same_prac"] + true_beta["same_prac"]) -
                       plogis(eta_true - choice_sets$same_prac * true_beta["same_prac"])),
    same_gender = mean(plogis(eta_true - choice_sets$same_gender * true_beta["same_gender"] + true_beta["same_gender"]) -
                       plogis(eta_true - choice_sets$same_gender * true_beta["same_gender"])),
    dist        = mean(plogis(eta_true + true_beta["dist"]) - plogis(eta_true))
  )

  # --- Approach 1: Naive logit (PCP FE only) ---
  naive_mod <- feglm(
    referral ~ same_prac + same_gender + dist | doctor,
    data = choice_sets,
    family = binomial("logit")
  )
  naive_beta <- coef(naive_mod)

  fes_naive <- fixef(naive_mod)
  doc_fe_naive <- fes_naive$doctor[as.character(choice_sets$doctor)]
  Xb_naive <- choice_sets$same_prac * naive_beta["same_prac"] +
              choice_sets$same_gender * naive_beta["same_gender"] +
              choice_sets$dist * naive_beta["dist"]
  eta_naive <- Xb_naive + doc_fe_naive
  ok_naive <- !is.na(eta_naive)

  naive_mfx <- c(
    same_prac   = mean(plogis(eta_naive[ok_naive] - choice_sets$same_prac[ok_naive] * naive_beta["same_prac"] + naive_beta["same_prac"]) -
                       plogis(eta_naive[ok_naive] - choice_sets$same_prac[ok_naive] * naive_beta["same_prac"])),
    same_gender = mean(plogis(eta_naive[ok_naive] - choice_sets$same_gender[ok_naive] * naive_beta["same_gender"] + naive_beta["same_gender"]) -
                       plogis(eta_naive[ok_naive] - choice_sets$same_gender[ok_naive] * naive_beta["same_gender"])),
    dist        = mean(plogis(eta_naive[ok_naive] + naive_beta["dist"]) - plogis(eta_naive[ok_naive]))
  )

  # --- Approach 2: Jochmans two-stage ---

  covars_sim <- c("same_prac", "same_gender", "dist")

  # Build quadruples using base R joins (data.table setnames/merge corrupts state over many reps)
  cs <- choice_sets %>%
    select(doctor, specialist, referral, same_prac, same_gender, dist, hrr, Year)

  joch_list <- list()
  for (cell in unique(paste(cs$hrr, cs$Year))) {
    block <- cs[paste(cs$hrr, cs$Year) == cell, ]
    docs  <- unique(block$doctor)
    specs <- unique(block$specialist)
    if (length(docs) < 2 || length(specs) < 2) next

    dp <- t(combn(docs, 2))
    sp <- t(combn(specs, 2))
    grid <- expand.grid(dp_idx = seq_len(nrow(dp)), sp_idx = seq_len(nrow(sp)))
    grid$i1 <- dp[grid$dp_idx, 1]; grid$i2 <- dp[grid$dp_idx, 2]
    grid$j1 <- sp[grid$sp_idx, 1]; grid$j2 <- sp[grid$sp_idx, 2]

    # Look up outcomes and covariates for all four (doctor, specialist) pairs
    lookup <- as.data.frame(block[, c("doctor", "specialist", "referral", "same_prac", "same_gender", "dist")])
    rownames(lookup) <- paste(lookup$doctor, lookup$specialist)

    key11 <- paste(grid$i1, grid$j1)
    key12 <- paste(grid$i1, grid$j2)
    key21 <- paste(grid$i2, grid$j1)
    key22 <- paste(grid$i2, grid$j2)

    y11 <- lookup[key11, "referral"]; y12 <- lookup[key12, "referral"]
    y21 <- lookup[key21, "referral"]; y22 <- lookup[key22, "referral"]

    dy1 <- y11 - y12
    dy2 <- y21 - y22
    disc <- which(dy1 * dy2 < 0)
    if (length(disc) == 0) next

    joch_list[[length(joch_list) + 1]] <- tibble(
      i1 = grid$i1[disc], j1 = grid$j1[disc],
      i2 = grid$i2[disc], j2 = grid$j2[disc],
      referral    = ((dy1[disc] - dy2[disc]) / 2 + 1) / 2,
      same_prac   = lookup[key11[disc], "same_prac"] - lookup[key12[disc], "same_prac"] -
                    lookup[key21[disc], "same_prac"] + lookup[key22[disc], "same_prac"],
      same_gender = lookup[key11[disc], "same_gender"] - lookup[key12[disc], "same_gender"] -
                    lookup[key21[disc], "same_gender"] + lookup[key22[disc], "same_gender"],
      dist        = lookup[key11[disc], "dist"] - lookup[key12[disc], "dist"] -
                    lookup[key21[disc], "dist"] + lookup[key22[disc], "dist"],
      year = block$Year[1],
      hrr_id = block$hrr[1]
    )
  }

  if (length(joch_list) == 0) return(NULL)
  df_joch <- bind_rows(joch_list)

  if (nrow(df_joch) < 10) return(NULL)

  joch_mod <- tryCatch(
    feglm(
      referral ~ same_prac + same_gender + dist | year,
      data = df_joch,
      family = binomial("logit")
    ),
    error = function(e) NULL
  )

  if (is.null(joch_mod)) return(NULL)
  joch_beta <- coef(joch_mod)

  # Stage 2: recover FEs
  Xb_joch <- choice_sets$same_prac * joch_beta["same_prac"] +
             choice_sets$same_gender * joch_beta["same_gender"] +
             choice_sets$dist * joch_beta["dist"]

  dat_fe <- choice_sets %>% mutate(.Xbeta = Xb_joch)

  fe_mod <- tryCatch(
    feglm(
      referral ~ 1 | doctor + specialist,
      data = dat_fe,
      offset = ~.Xbeta,
      family = binomial("logit"),
      glm.iter = 200
    ),
    error = function(e) NULL
  )

  if (is.null(fe_mod)) return(NULL)

  fes_joch <- fixef(fe_mod)
  doc_fe_joch  <- fes_joch$doctor[as.character(dat_fe$doctor)]
  spec_fe_joch <- fes_joch$specialist[as.character(dat_fe$specialist)]
  eta_joch     <- Xb_joch + doc_fe_joch + spec_fe_joch
  ok_joch      <- !is.na(eta_joch)

  joch_mfx <- c(
    same_prac   = mean(plogis(eta_joch[ok_joch] - choice_sets$same_prac[ok_joch] * joch_beta["same_prac"] + joch_beta["same_prac"]) -
                       plogis(eta_joch[ok_joch] - choice_sets$same_prac[ok_joch] * joch_beta["same_prac"])),
    same_gender = mean(plogis(eta_joch[ok_joch] - choice_sets$same_gender[ok_joch] * joch_beta["same_gender"] + joch_beta["same_gender"]) -
                       plogis(eta_joch[ok_joch] - choice_sets$same_gender[ok_joch] * joch_beta["same_gender"])),
    dist        = mean(plogis(eta_joch[ok_joch] + joch_beta["dist"]) - plogis(eta_joch[ok_joch]))
  )

  # Return results
  tibble(
    sim = sim_id,
    variable = rep(names(true_beta), 3),
    method   = rep(c("True", "Naive", "Two-stage"), each = 3),
    beta     = c(true_beta, naive_beta[names(true_beta)], joch_beta[names(true_beta)]),
    mfx      = c(true_mfx, naive_mfx, joch_mfx)
  )
}


# --- Run Monte Carlo across scenarios ---

scenarios <- list(
  list(label = "Baseline",          fe_loading = 0.5, gender_loading = 0.3, fe_sd = 1.0),
  list(label = "Higher correlation", fe_loading = 1.5, gender_loading = 1.0, fe_sd = 1.0),
  list(label = "Higher corr + var", fe_loading = 1.5, gender_loading = 1.0, fe_sd = 2.0)
)

all_summaries <- list()

for (sc in scenarios) {
  message("Scenario: ", sc$label, " (loading=", sc$fe_loading,
          ", gender=", sc$gender_loading, ", sd=", sc$fe_sd, ")")

  mc_results <- map(seq_len(N_mc), function(s) {
    if (s %% 10 == 0) message("  Replication ", s, " / ", N_mc)
    run_one_sim(s, fe_loading = sc$fe_loading,
                gender_loading = sc$gender_loading, fe_sd = sc$fe_sd)
  }) %>% bind_rows()

  n_successful <- n_distinct(mc_results$sim)
  message("  Completed ", n_successful, " / ", N_mc, " replications")

  all_summaries[[sc$label]] <- mc_results %>%
    group_by(variable, method) %>%
    summarise(
      beta_mean = mean(beta),
      beta_sd   = sd(beta),
      mfx_mean  = mean(mfx),
      mfx_sd    = sd(mfx),
      .groups = "drop"
    ) %>%
    mutate(scenario = sc$label)
}

mc_summary <- bind_rows(all_summaries)


# --- Build LaTeX table ---

var_labels <- c(same_prac = "Same practice", same_gender = "Same gender", dist = "Distance")
var_order  <- c("same_prac", "same_gender", "dist")

fmt_sim <- function(x) {
  s <- sprintf("%.3f", abs(x))
  if (x < 0) paste0("-", s) else s
}

# Build one table per scenario
for (sc_name in names(all_summaries)) {
  sc_summary <- all_summaries[[sc_name]]

  tbl_rows <- map(var_order, function(v) {
    true_row   <- sc_summary %>% filter(variable == v, method == "True")
    naive_row  <- sc_summary %>% filter(variable == v, method == "Naive")
    twostg_row <- sc_summary %>% filter(variable == v, method == "Two-stage")

    bind_rows(
      tibble(
        term        = var_labels[v],
        true_b      = fmt_sim(true_row$beta_mean),
        naive_b     = fmt_sim(naive_row$beta_mean),
        twostage_b  = fmt_sim(twostg_row$beta_mean),
        true_m      = fmt_sim(true_row$mfx_mean),
        naive_m     = fmt_sim(naive_row$mfx_mean),
        twostage_m  = fmt_sim(twostg_row$mfx_mean)
      ),
      tibble(
        term        = "",
        true_b      = "",
        naive_b     = paste0("[", fmt_sim(naive_row$beta_sd), "]"),
        twostage_b  = paste0("[", fmt_sim(twostg_row$beta_sd), "]"),
        true_m      = paste0("[", fmt_sim(true_row$mfx_sd), "]"),
        naive_m     = paste0("[", fmt_sim(naive_row$mfx_sd), "]"),
        twostage_m  = paste0("[", fmt_sim(twostg_row$mfx_sd), "]")
      )
    )
  }) %>% bind_rows()

  footer <- tibble(
    term       = "Replications",
    true_b     = as.character(N_mc),
    naive_b    = as.character(N_mc),
    twostage_b = as.character(N_mc),
    true_m     = as.character(N_mc),
    naive_m    = as.character(N_mc),
    twostage_m = as.character(N_mc)
  )

  tbl_all <- bind_rows(tbl_rows, footer)

  # File suffix from scenario name
  sc_suffix <- tolower(gsub("[^a-zA-Z]", "_", sc_name))

  kable(tbl_all,
        format   = "latex",
        booktabs = TRUE,
        linesep  = "",
        escape   = FALSE,
        align    = c("l", rep("r", 6)),
        col.names = c("", "True", "Naive", "Two-stage",
                       "True", "Naive", "Two-stage")) %>%
    add_header_above(c(" " = 1,
                       "$\\\\beta$ (log-odds)" = 3,
                       "Avg. marginal effects" = 3),
                     escape = FALSE) %>%
    save_kable(sprintf("results/tables/app_simulation_%s.tex", sc_suffix))

  message("  Table saved: app_simulation_", sc_suffix, ".tex")
}

message("All simulation tables written.")
