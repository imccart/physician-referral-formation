
# Baseline logit with race (Zeltzer) ----------------------------------------

logit_race1 <- feglm(
  referral ~ same_sex + same_race + spec_male + exp_spec | Year + doctor,
  data = df_logit,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_race2 <- feglm(
  referral ~ same_sex + same_race + spec_male + exp_spec + same_prac + dist_miles | Year + doctor,
  data = df_logit,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_race3 <- feglm(
  referral ~ same_sex + same_race + spec_male + exp_spec + same_prac + dist_miles + diff_age + diff_gradyear | Year + doctor,
  data = df_logit,
  vcov = "HC1",
  family = binomial(link = "logit")
)


mfx_logit_race1 <- avg_slopes(logit_race1)
mfx_logit_race2 <- avg_slopes(logit_race2)
mfx_logit_race3 <- avg_slopes(logit_race3)

# Organize models into a list
models_logit_race <- list(
  "(1)" = mfx_logit_race1,
  "(2)" = mfx_logit_race2,
  "(3)" = mfx_logit_race3
)

# Custom coefficient labels to match your table rows
coef_labels <- c(
  "same_sex" = "Same gender",
  "spec_male" = "Male specialist",   
  "exp_spec" = "Specialist experience (10 years)",
  "same_prac" = "Same practice group",
  "same_race" = "Same race",
  "dist_miles" = "Distance (miles)",  
  "diff_age" = "Similar age",
  "diff_gradyear" = "Similar experience"
)

# Additional rows for FE indicators and summary stats
add_rows <- tribble(
  ~term, ~`(1)`, ~`(2)`, ~`(3)`,
  "Year FE", "Yes", "Yes", "Yes",
  "Doctor FE", "Yes", "Yes","Yes",
  "Specialist FE", "No", "No", "No",
  "Observations", format(nobs(logit_race1), big.mark=","), 
                  format(nobs(logit_race2), big.mark=","), 
                  format(nobs(logit_race3), big.mark=","), 
  "Pseudo-$R^2$", format(logit_race1$pseudo_r2, digits = 2),
                  format(logit_race2$pseudo_r2, digits = 2),
                  format(logit_race3$pseudo_r2, digits = 2)
)


# Create the LaTeX table
options(modelsummary_format_numeric_latex = "plain")
summary_logit_race <- modelsummary(
  models_logit_race,
  coef_map = coef_labels,
  stars = FALSE,
  gof_omit = ".*",
  add_rows = add_rows,
  output = "latex_tabular"
)

writeLines(as.character(summary_logit_race), "results/tables/app_logit_race_mfx.tex")



# Jochmans Logit ---------------------------------------------------

logit_twfe1 <- feglm(
  referral ~ same_sex + same_prac + diff_dist  | year,
  data = df_logit_twfe,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_twfe2 <- feglm(
  referral ~ same_sex + same_prac + diff_dist + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = "HC1",
  family = binomial(link = "logit")
)

logit_twfe3 <- feglm(
  referral ~ same_sex + same_prac + diff_dist + same_race + diff_age + diff_gradyear | year,
  data = df_logit_twfe,
  vcov = "HC1",
  family = binomial(link = "logit")
)

## Manual MFX for Jochmans logit models
specs <- list(
  `(1)` = list(
      model      = logit_twfe1,
      binary     = c("same_sex", "same_prac"),
      continuous = c(diff_dist  =  5)   # + 5 miles
  ),
  `(2)` = list(
      model      = logit_twfe2,
      binary     = c("same_sex", "same_prac"),
      continuous = c(diff_age  =  1,
                     diff_dist = 5,
                     diff_gradyear = 1)
  ),
  `(3)` = list(
      model      = logit_twfe3,
      binary     = c("same_sex", "same_prac", "same_race"),
      continuous = c(diff_age  = 1,
                     diff_dist = 5,
                     diff_gradyear = 1)
  )
)

## compute average first–difference and SE for ONE variable
link_effect <- function(model, data, var, delta, binary = TRUE) {

  beta <- coef(model)
  keep <- names(beta)

  build_X <- function(d) {
    mm <- model.matrix(delete.response(terms(model)), d)
    mm[, keep, drop = FALSE]              # ensure conformable
  }

  # counterfactual data frames
  data_cf1 <- data
  data_cf0 <- data

  if (binary) {
    data_cf1[[var]] <- 1L
    data_cf0[[var]] <- 0L
  } else {
    data_cf1[[var]] <- data_cf1[[var]] + delta
  }

  X1 <- build_X(data_cf1)
  X0 <- build_X(data_cf0)

  eta1 <- drop(X1 %*% beta)
  eta0 <- drop(X0 %*% beta)
  p1   <- plogis(eta1)
  p0   <- plogis(eta0)

  dp <- mean(p1 - p0, na.rm = TRUE)

  # delta-method gradient (see Jochmans 2018 appendix)
  grad_i <- (p1 * (1 - p1)) * X1 - (p0 * (1 - p0)) * X0
  g_bar  <- colMeans(grad_i)

  se <- sqrt( t(g_bar) %*% vcov(model, type = "HC1") %*% g_bar )

  tibble(term      = var,
         estimate  = dp,
         std.error = as.numeric(se))
}

## Loop over models and covariates
effects_twfe <- imap_dfr(specs, function(info, label) {
  mod   <- info$model
  dat   <- df_logit %>% rename(year = Year)  # supply the estimation data
  # binary variables
  eff_bin <- map_dfr(
    info$binary,
    link_effect,
    model  = mod,
    data   = dat,
    delta  = 1,
    binary = TRUE
  )

  # continuous covariates
  eff_con <- imap_dfr(
    info$continuous,
    ~ link_effect(
        model  = mod,
        data   = dat,
        var    = .y,
        delta  = .x,
        binary = FALSE
      )
  )

  bind_rows(eff_bin, eff_con) %>%
    mutate(model = label)
})


coef_labels <- c(
  same_sex       = "Same gender",
  same_prac      = "Same practice group",
  same_race      = "Same race",
  diff_dist      = "Differential distance (+5 miles)",
  diff_age       = "Similar age (+1 yr)",
  diff_gradyear  = "Similar experience (+1 yr)"
)

vars_order <- names(coef_labels)            # preserve this order

make_block <- function(var) {

  lbl <- coef_labels[[var]]

  # pull estimates & SEs (may be empty if var not in that model)
  ests <- effects_twfe %>%
            filter(term == var) %>%
            select(model, estimate, std.error)

  cell <- function(mod, what) {
    val <- ests[[what]][ests$model == mod]
    if (length(val) == 0 || is.na(val)) return(" ")
    if (what == "estimate") comma(val, accuracy = 0.001)
    else                    paste0("(", comma(val, accuracy = 0.001), ")")
  }

  tibble(
    term   = lbl,
    `(1)`  = cell("(1)", "estimate"),
    `(2)`  = cell("(2)", "estimate"),
    `(3)`  = cell("(3)", "estimate")
  ) %>%
  bind_rows(
    tibble(
      term   = "",                        # indent SE row
      `(1)`  = cell("(1)", "std.error"),
      `(2)`  = cell("(2)", "std.error"),
      `(3)`  = cell("(3)", "std.error")
    )
  )
}

table_body <- lapply(vars_order, make_block) %>% bind_rows()

add_rows <- tribble(
  ~term,            ~`(1)`, ~`(2)`, ~`(3)`,
  "Year FE",        "Yes",  "Yes",  "Yes",
  "Doctor FE",      "Yes",  "Yes",  "Yes",
  "Specialist FE",  "Yes",  "Yes",  "Yes",
  "Observations",
        format(nobs(logit_twfe1), big.mark = ","),
        format(nobs(logit_twfe2), big.mark = ","),
        format(nobs(logit_twfe3), big.mark = ","),
  "Pseudo-$R^2$",
        format(logit_twfe1$pseudo_r2, digits = 2),
        format(logit_twfe2$pseudo_r2, digits = 2),
        format(logit_twfe3$pseudo_r2, digits = 2)
)

table_out <- bind_rows(table_body, add_rows)

kable(table_out,
      format     = "latex",
      booktabs   = TRUE,
      align      = c("l", rep("r", 3)),
      col.names  = c("", "(1)", "(2)", "(3)")) %>%
  kable_styling(latex_options = "hold_position") %>%
  save_kable("results/tables/logit_twfe_mfx.tex")
