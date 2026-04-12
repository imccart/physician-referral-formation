## Appendix: Period-specific (non-cumulative) estimation (Referee Comment 3b)
## Mirrors 3_referral_windows.R but uses year-specific data instead of cumulative.
## Reads df_jochmans_period_{spec}.csv and df_logit_period_{spec}.csv.

message("  Period-specific window estimation...")

covars_p <- c("same_sex", "same_race", "same_prac",
              "diff_dist", "diff_age", "diff_gradyear")

# Read period-specific data
df_joch_period <- read_csv(
  sprintf("data/output/df_jochmans_period_%s.csv", current_specialty))

df_cs_period <- read_csv(
  sprintf("data/output/df_logit_period_%s.csv", current_specialty),
  col_select = c("Year", "doctor", "specialist", "referral",
                 "same_sex", "same_race", "same_prac", "diff_dist",
                 "diff_age", "diff_gradyear", "doc_hrr", "period")
) %>%
  mutate(doctor = as.factor(doctor), specialist = as.factor(specialist))

periods <- sort(unique(df_joch_period$period))

mfx_period <- map(periods, function(per) {
  message("  Period: ", per)

  dat_joch <- df_joch_period %>% filter(period == per)

  if (nrow(dat_joch) < 50) {
    message("    Too few quartets (", nrow(dat_joch), "), skipping")
    return(tibble())
  }

  joch_p <- tryCatch(
    fixest::feglm(
      referral ~ same_sex + same_race + same_prac +
                 diff_dist + diff_age + diff_gradyear | year,
      data = dat_joch,
      vcov = ~hrr,
      family = binomial("logit")
    ),
    error = function(e) {
      message("    Jochmans failed: ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(joch_p)) return(tibble())

  beta_p <- coef(joch_p)[covars_p]

  # Stage 2: FE recovery
  dat_cs <- df_cs_period %>% filter(period == per)
  Xmat_p <- as.matrix(dat_cs[, covars_p])
  na_p   <- complete.cases(Xmat_p)
  dat_cs <- dat_cs[na_p, ]
  Xbeta_p <- drop(Xmat_p[na_p, ] %*% beta_p)
  dat_cs <- dat_cs %>% mutate(.Xbeta = Xbeta_p)

  fe_p <- tryCatch(
    fixest::feglm(
      referral ~ 1 | doctor + specialist,
      data = dat_cs,
      offset = ~.Xbeta,
      family = binomial("logit"),
      glm.iter = 200
    ),
    error = function(e) {
      message("    FE recovery failed: ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(fe_p)) return(tibble())

  fes_p   <- fixef(fe_p)
  doc_fe  <- fes_p$doctor[as.character(dat_cs$doctor)]
  spec_fe <- fes_p$specialist[as.character(dat_cs$specialist)]
  eta_p   <- Xbeta_p + doc_fe + spec_fe
  valid   <- !is.na(eta_p)

  dat_v <- dat_cs[valid, ]
  eta_v <- eta_p[valid]

  V <- vcov(joch_p, type = "HC1")
  V <- V[covars_p, covars_p]

  # MFX with delta-method SEs
  mfx <- map(covars_p, function(v) {
    is_bin <- v %in% c("same_sex", "same_prac", "same_race")
    delta  <- if (v == "diff_dist") 5 else 1

    if (is_bin) {
      eta1 <- eta_v - beta_p[v] * dat_v[[v]] + beta_p[v]
      eta0 <- eta_v - beta_p[v] * dat_v[[v]]
    } else {
      eta1 <- eta_v + beta_p[v] * delta
      eta0 <- eta_v
    }

    p1 <- plogis(eta1)
    p0 <- plogis(eta0)
    dp <- mean(p1 - p0)

    grad_i <- matrix(0, nrow = length(eta_v), ncol = length(covars_p))
    colnames(grad_i) <- covars_p

    for (k in covars_p) {
      if (is_bin) {
        deta1_dk <- if (k == v) rep(1, length(eta_v)) else dat_v[[k]]
        deta0_dk <- if (k == v) rep(0, length(eta_v)) else dat_v[[k]]
      } else {
        deta1_dk <- dat_v[[k]] + if (k == v) delta else 0
        deta0_dk <- dat_v[[k]]
      }
      grad_i[, k] <- (p1 * (1 - p1)) * deta1_dk - (p0 * (1 - p0)) * deta0_dk
    }

    g_bar <- colMeans(grad_i)
    se <- sqrt(as.numeric(t(g_bar) %*% V %*% g_bar))

    tibble(term = v, estimate = dp, std.error = se)
  }) %>% bind_rows()

  gc()
  mfx %>% mutate(model = per)
}) %>% bind_rows()


## Plot: period-specific MFX by window ----

plot_vars <- c("same_prac", "same_sex", "same_race",
               "diff_dist", "diff_age", "diff_gradyear")

mfx_period_plot <- mfx_period %>%
  filter(term %in% plot_vars) %>%
  mutate(
    horizon   = as.integer(str_extract(model, "\\d+")),
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

base_shapes <- c(21, 22, 24, 23, 25, 7, 8, 3)
shape_vec   <- setNames(base_shapes[seq_along(plot_vars)], plot_vars)
nice_lab <- function(v)
  stringr::str_replace_all(v,
    c("^same_prac$"  = "Same practice",
      "^same_sex$"   = "Same gender",
      "^same_race$"  = "Same race",
      "^diff_dist$"  = "Distance (+5 mi)",
      "^diff_age$"   = "Age gap",
      "^diff_gradyear$" = "Experience gap"))
shape_labs <- sapply(plot_vars, nice_lab, USE.NAMES = FALSE)

dodge <- position_dodge(width = .30)

ggplot(mfx_period_plot,
       aes(x = horizon, y = estimate,
           ymin = conf.low, ymax = conf.high,
           group = term, shape = term)) +
  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .3) +
  geom_linerange(position = dodge,
                 colour = "grey50", alpha = .6, linewidth = .5) +
  geom_point(position = dodge,
             size = 1.5, colour = "black", fill = "white") +
  scale_shape_manual(values = shape_vec,
                     breaks = plot_vars,
                     labels = shape_labs,
                     name   = "Covariate") +
  scale_x_continuous(breaks = 1:4, labels = paste(1:4, "yr")) +
  labs(x = "Year since move (period-specific)",
       y = "Average marginal effect") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position.inside = c(1, 1),
    legend.justification  = c(1, 1),
    legend.box.background = element_rect(colour = "grey70"),
    legend.key            = element_blank()
  )

ggsave(sprintf("results/figures/mfx_by_window_period_%s.png", current_specialty),
       width = 6.5, height = 4, dpi = 300)
