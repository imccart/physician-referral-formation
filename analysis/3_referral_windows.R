# Dynamics: two-stage estimation per referral window -------------------------
# Stage 1: Jochmans β per window (df_logit_windows = quartet data by window)
# Stage 2: FE recovery on standard choice-set data (df_choiceset_windows)
# MFX: first-difference with delta-method SEs

covars_w <- c("same_sex", "same_race", "same_prac",
              "diff_dist", "diff_age", "diff_gradyear")

windows <- sort(unique(df_logit_windows$window))

mfx_window <- map_dfr(windows, function(win) {
  message("Window: ", win)

  # Stage 1: Jochmans on quartet data for this window
  dat_joch <- df_logit_windows %>% filter(window == win)

  joch_w <- tryCatch(
    fixest::feglm(
      referral ~ same_sex + same_race + same_prac +
                 diff_dist + diff_age + diff_gradyear | year,
      data = dat_joch,
      vcov = ~hrr,
      family = binomial("logit")
    ),
    error = function(e) {
      message("  Jochmans failed for window ", win, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(joch_w)) return(tibble())

  beta_w <- coef(joch_w)[covars_w]

  # Stage 2: FE recovery on choice-set data for this window
  dat_cs <- df_choiceset_windows %>% filter(window == win)
  Xmat_w <- as.matrix(dat_cs[, covars_w])
  na_w   <- complete.cases(Xmat_w)
  dat_cs <- dat_cs[na_w, ]
  Xbeta_w <- drop(Xmat_w[na_w, ] %*% beta_w)
  dat_cs <- dat_cs %>% mutate(.Xbeta = Xbeta_w)

  fe_w <- tryCatch(
    fixest::feglm(
      referral ~ 1 | doctor + specialist,
      data = dat_cs,
      offset = ~.Xbeta,
      family = binomial("logit"),
      glm.iter = 50
    ),
    error = function(e) {
      message("  FE recovery failed for window ", win, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(fe_w)) return(tibble())

  fes_w   <- fixef(fe_w)
  doc_fe  <- fes_w$doctor[as.character(dat_cs$doctor)]
  spec_fe <- fes_w$specialist[as.character(dat_cs$specialist)]
  eta_w   <- Xbeta_w + doc_fe + spec_fe
  valid   <- !is.na(eta_w)

  dat_v <- dat_cs[valid, ]
  eta_v <- eta_w[valid]

  V <- vcov(joch_w, type = "HC1")
  V <- V[covars_w, covars_w]

  # MFX with delta-method SEs (reuses logic from 2_logit_twfe.R)
  mfx <- map_dfr(covars_w, function(v) {
    is_bin <- v %in% c("same_sex", "same_prac", "same_race")
    delta  <- if (v == "diff_dist") 5 else 1

    if (is_bin) {
      eta1 <- eta_v - beta_w[v] * dat_v[[v]] + beta_w[v]
      eta0 <- eta_v - beta_w[v] * dat_v[[v]]
    } else {
      eta1 <- eta_v + beta_w[v] * delta
      eta0 <- eta_v
    }

    p1 <- plogis(eta1)
    p0 <- plogis(eta0)
    dp <- mean(p1 - p0)

    grad_i <- matrix(0, nrow = length(eta_v), ncol = length(covars_w))
    colnames(grad_i) <- covars_w

    for (k in covars_w) {
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
  })

  gc()
  mfx %>% mutate(model = win)
})


## Plotting ----

plot_vars <- c(
  "same_prac", "same_sex", "same_race",
  "diff_dist", "diff_age", "diff_gradyear"
)

horizon_from_label <- function(label) {
  as.integer(str_extract(label, "\\d+"))
}

mfx_plot <- mfx_window %>%
  filter(term %in% plot_vars) %>%
  mutate(
    horizon   = horizon_from_label(as.character(model)),
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

base_shapes <- c(21, 22, 24, 23, 25, 7, 8, 3)
shape_vec   <- setNames(base_shapes[seq_along(plot_vars)],
                        plot_vars)
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

ggplot(mfx_plot,
       aes(x = horizon, y = estimate,
           ymin = conf.low, ymax = conf.high,
           group = term, shape = term)) +

  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .3) +

  geom_linerange(position = dodge,
                 colour = "grey50", alpha = .35, linewidth = .9) +

  geom_point(position = dodge,
             size = 3, colour = "black", fill = "white") +

  scale_shape_manual(values = shape_vec,
                     breaks = plot_vars,
                     labels = shape_labs,
                     name   = "Covariate") +

  scale_x_continuous(breaks = 1:6,
                     labels = paste(1:6, "yr")) +

  labs(x = "Years since move",
       y = "Average marginal effect") +

  theme_minimal(base_size = 11) +
  theme(
    legend.position.inside = c(1, 1),
    legend.justification  = c(1, 1),
    legend.box.background = element_rect(colour = "grey70"),
    legend.key            = element_blank()
  )

ggsave(sprintf("results/figures/mfx_by_window_%s.png", current_specialty), width = 6.5, height = 4, dpi = 300)
