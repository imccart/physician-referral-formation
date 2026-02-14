# Analysis by referral window to examine variation in referral determinants over time

bin_vars  <- c("same_sex", "same_prac", "same_race")

cont_inc  <- c(
  diff_age       = 1,   # +1 year
  diff_dist      = 5,  # +5 miles
  diff_gradyear  = 1    # +1 year
)

plot_vars <- c(
  "same_prac", "same_sex", "same_race",
  "diff_dist", "diff_age", "diff_gradyear"
)

## helper to extract the numeric horizon k from labels
horizon_from_label <- function(label) {
  as.integer(str_extract(label, "\\d+"))
}

## average probability differences and calculate standard errors
link_effect_se <- function(model, data, var, delta, is_binary = TRUE) {

  beta <- coef(model)
  keep <- names(beta)

  build_X <- function(d) {
    mm <- model.matrix(delete.response(terms(model)), d)
    mm[, keep, drop = FALSE]          # keep only columns that match beta
  }

  d1 <- data
  d0 <- data

  if (is_binary) {
    d1[[var]] <- 1L
    d0[[var]] <- 0L
  } else {
    d1[[var]] <- d1[[var]] + delta
  }

  X1 <- build_X(d1)
  X0 <- build_X(d0)

  p1 <- plogis(drop(X1 %*% beta))
  p0 <- plogis(drop(X0 %*% beta))

  dp <- mean(p1 - p0, na.rm = TRUE)   # average first–difference

  g_bar <- colMeans((p1 * (1 - p1)) * X1 -
                    (p0 * (1 - p0)) * X0)

  se <- sqrt(
    t(g_bar) %*% vcov(model, type = "HC1") %*% g_bar
  )

  tibble(
    term      = var,
    estimate  = dp,
    std.error = as.numeric(se)
  )
}

## loop over referral windows
windows <- sort(unique(df_logit_windows$window))

mfx_window <- map_dfr(windows, function(win) {

  dat_w <- df_logit_windows %>%
    filter(window == win)

  mod_w <- feglm(
    referral ~ same_sex + same_race + same_prac +
               diff_dist + diff_age + diff_gradyear |
               year,
    data   = dat_w,
    vcov   = "HC1",
    family = binomial("logit")
  )

  # binary covariates
  eff_bin <- map_dfr(
    bin_vars,
    link_effect_se,
    model     = mod_w,
    data      = dat_w,
    delta     = 1,
    is_binary = TRUE
  )

  # continuous covariates
  eff_cont <- imap_dfr(
    cont_inc,
    ~ link_effect_se(
        model     = mod_w,
        data      = dat_w,
        var       = .y,
        delta     = .x,
        is_binary = FALSE
      )
  )

  bind_rows(eff_bin, eff_cont) %>%
    mutate(
      window  = win,
      horizon = horizon_from_label(as.character(win))
    )
})

## keep focal variables and add 95 % CIs
mfx_plot <- mfx_window %>%
  filter(term %in% plot_vars) %>%
  mutate(
    conf.low  = estimate - 1.96 * std.error,
    conf.high = estimate + 1.96 * std.error
  )

base_shapes <- c(21, 22, 24, 23, 25, 7, 8, 3)           # easily distinguishable
shape_vec   <- setNames(base_shapes[seq_along(plot_vars)],
                        plot_vars)
nice_lab <- function(v)
  stringr::str_replace_all(v,
    c("^same_prac$"  = "Same practice",
      "^same_sex$"   = "Same gender",
      "^same_race$"  = "Same race",
      "^diff_dist$"  = "Distance (miles)",
      "^diff_age$"   = "Age gap",
      "^diff_gradyear$" = "Experience gap"))
shape_labs <- sapply(plot_vars, nice_lab, USE.NAMES = FALSE)

## plot marginal effects vs horizon length
dodge <- position_dodge(width = .30)

ggplot(mfx_plot,
       aes(x = horizon, y = estimate,
           ymin = conf.low, ymax = conf.high,
           group = term, shape = term)) +

  geom_hline(yintercept = 0, linetype = "dashed", linewidth = .3) +

  ## CIs
  geom_linerange(position = dodge,
                 colour = "grey50", alpha = .35, linewidth = .9) +

  ## point estimates
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
    legend.position.inside = c(1, 1),     # inside, top-right
    legend.justification  = c(1, 1),
    legend.box.background = element_rect(colour = "grey70"),
    legend.key            = element_blank()
  )

ggsave("results/figures/mfx_by_window.png", width = 6.5, height = 4, dpi = 300)
