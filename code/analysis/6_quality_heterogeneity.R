
# 6_quality_heterogeneity.R — Quality heterogeneity in referral formation
# Tests whether practice affiliation and other heuristics vary by specialist quality.
# Ortho only (only specialty with spec_qual). Zeltzer-style conditional logit.
#
# Standalone: does not depend on current_specialty.
# Reads data directly from data/output/.

message("Quality heterogeneity analysis (ortho only)...")

df_qual <- read_csv("data/output/df_logit_movers_ortho.csv",
                    show_col_types = FALSE)

# Drop rows with missing quality
df_qual <- df_qual %>% filter(!is.na(spec_qual))

# Compute specialist-level quality (average across years)
spec_quality <- df_qual %>%
  group_by(specialist) %>%
  summarise(mean_qual = mean(spec_qual, na.rm = TRUE), .groups = "drop")

# Terciles: lower readmission = higher quality
# Q1 = best (lowest readmission), Q3 = worst (highest readmission)
spec_quality <- spec_quality %>%
  mutate(qual_tercile = ntile(mean_qual, 3),
         qual_label = case_when(
           qual_tercile == 1 ~ "Q1 (best)",
           qual_tercile == 2 ~ "Q2 (middle)",
           qual_tercile == 3 ~ "Q3 (worst)"
         ))

df_qual <- df_qual %>%
  left_join(spec_quality %>% select(specialist, qual_tercile, qual_label),
            by = "specialist") %>%
  filter(!is.na(qual_tercile))

message("  Observations by tercile:")
message("    Q1 (best):   ", sum(df_qual$qual_tercile == 1))
message("    Q2 (middle): ", sum(df_qual$qual_tercile == 2))
message("    Q3 (worst):  ", sum(df_qual$qual_tercile == 3))


# Estimate Zeltzer-style conditional logit by tercile ----------------------

covars_q <- c("same_sex", "same_race", "same_prac", "dist_miles",
              "diff_age", "diff_gradyear")

estimate_tercile <- function(q) {
  dat <- df_qual %>% filter(qual_tercile == q)

  mod <- tryCatch(
    feglm(
      referral ~ same_sex + same_race + same_prac + dist_miles +
        diff_age + diff_gradyear | Year + doctor,
      data = dat,
      vcov = ~doctor,
      family = binomial(link = "logit")
    ),
    error = function(e) {
      message("  Estimation failed for tercile ", q, ": ", conditionMessage(e))
      return(NULL)
    }
  )

  if (is.null(mod)) return(list(mod = NULL, mfx = tibble(), n = nrow(dat)))

  # MFX: same approach as compute_mfx_zeltzer in 2_logit_twfe.R
  beta <- coef(mod)[covars_q]
  V    <- vcov(mod)[covars_q, covars_q]

  Xmat    <- as.matrix(dat[, covars_q])
  na_mask <- complete.cases(Xmat)
  dat_v   <- dat[na_mask, ]
  Xbeta   <- drop(Xmat[na_mask, ] %*% beta)

  fes     <- fixef(mod)
  doc_fe  <- fes$doctor[as.character(dat_v$doctor)]
  year_fe <- fes$Year[as.character(dat_v$Year)]
  eta     <- Xbeta + doc_fe + year_fe

  ok    <- !is.na(eta)
  eta   <- eta[ok]
  dat_v <- dat_v[ok, ]

  bin_vars <- c("same_sex", "same_race", "same_prac")

  mfx <- map(covars_q, function(v) {
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
    dp <- mean(p1 - p0)

    grad_i <- matrix(0, nrow = length(eta), ncol = length(covars_q))
    colnames(grad_i) <- covars_q
    for (k in covars_q) {
      if (is_bin) {
        deta1_dk <- if (k == v) rep(1, length(eta)) else dat_v[[k]]
        deta0_dk <- if (k == v) rep(0, length(eta)) else dat_v[[k]]
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

  list(mod = mod, mfx = mfx, n = sum(ok))
}

res_q1 <- estimate_tercile(1)
res_q2 <- estimate_tercile(2)
res_q3 <- estimate_tercile(3)


# Build output table -------------------------------------------------------

coef_labels <- c(
  same_sex      = "Same gender",
  same_race     = "Same race",
  same_prac     = "Same practice group",
  dist_miles    = "Distance (miles)",
  diff_age      = "Age difference",
  diff_gradyear = "Experience difference"
)

fmt_q <- function(x) {
  if (is.na(x) || length(x) == 0) return("")
  s <- sprintf("%.3f", abs(x))
  if (x < 0) paste0("-", s) else s
}

extract_q <- function(mfx_obj, terms) {
  df <- as.data.frame(mfx_obj)
  out <- setNames(rep("", length(terms)), terms)
  se  <- setNames(rep("", length(terms)), terms)
  for (v in terms) {
    row <- df[df$term == v, ]
    if (nrow(row) == 1) {
      out[v] <- fmt_q(row$estimate)
      se[v]  <- paste0("(", sprintf("%.3f", row$std.error), ")")
    }
  }
  list(est = out, se = se)
}

m1 <- extract_q(res_q1$mfx, names(coef_labels))
m2 <- extract_q(res_q2$mfx, names(coef_labels))
m3 <- extract_q(res_q3$mfx, names(coef_labels))

tbl_rows <- map(seq_along(coef_labels), function(i) {
  v <- names(coef_labels)[i]
  bind_rows(
    tibble(term = coef_labels[i],
           `(1)` = m1$est[v], `(2)` = m2$est[v], `(3)` = m3$est[v]),
    tibble(term = "",
           `(1)` = m1$se[v],  `(2)` = m2$se[v],  `(3)` = m3$se[v])
  )
}) %>% bind_rows()

footer <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Year FE", "Yes", "Yes", "Yes",
  "Doctor FE", "Yes", "Yes", "Yes",
  "Observations", format(res_q1$n, big.mark = ","),
                  format(res_q2$n, big.mark = ","),
                  format(res_q3$n, big.mark = ",")
)

tbl_all <- bind_rows(tbl_rows, footer)

kable(tbl_all, format = "latex", booktabs = TRUE, escape = FALSE, linesep = "",
      align = c("l", "r", "r", "r"),
      col.names = c("", "Q1 (best)", "Q2 (middle)", "Q3 (worst)")) %>%
  add_header_above(c(" " = 1, "Specialist quality tercile" = 3),
                   escape = FALSE) %>%
  save_kable("results/tables/quality_heterogeneity_ortho.tex")

message("Quality heterogeneity table saved: quality_heterogeneity_ortho.tex")

# Cleanup
rm(df_qual, spec_quality, res_q1, res_q2, res_q3,
   m1, m2, m3, tbl_rows, footer, tbl_all)
gc()
