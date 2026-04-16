
# paper_tables.R — Build combined cross-specialty tables for paper and appendix
# Reads per-specialty CSVs and .tex files, produces combined versions
# Can be sourced from _main.R or run standalone after all specialties have run

if (!exists("map", mode = "function")) {
  library(tidyverse)
}

specs <- c("ortho", "cardioem", "derm")
spec_labels <- c(ortho = "Orthopedic Surgery", cardioem = "Cardiology",
                 derm = "Dermatology")


# Helper: read lines between \begin{tabular} and \end{tabular}, exclusive
read_tabular_body <- function(path) {
  lines <- readLines(path)
  start <- grep("\\\\midrule", lines)[1]
  end   <- grep("\\\\bottomrule", lines)[1]
  if (is.na(start) || is.na(end)) return(character(0))
  lines[(start + 1):(end - 1)]
}

# Helper: read full tabular content (everything between \begin{tabular} and \end{tabular})
read_tabular_full <- function(path) {
  lines <- readLines(path)
  start <- grep("\\\\begin\\{tabular\\}", lines)[1]
  end   <- grep("\\\\end\\{tabular\\}", lines)[1]
  lines[start:end]
}


# ============================================================
# 1. Descriptive stats — stacked panels
# ============================================================

desc_panels <- map(specs, function(s) {
  body <- read_tabular_body(sprintf("results/tables/desc_%s.tex", s))
  c(sprintf("\\addlinespace[0.5em]"),
    sprintf("\\multicolumn{3}{l}{\\textbf{%s}}\\\\", spec_labels[s]),
    body)
}) %>% unlist()

desc_combined <- c(
  "\\begin{tabular}{lrr}",
  "\\toprule",
  "  & All referrals & PCP movers\\\\",
  "\\midrule",
  desc_panels,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(desc_combined, "results/tables/desc_all.tex")


# ============================================================
# 2. Link stats — side-by-side (6 cols)
# ============================================================

link_rows <- c("Same practice group", "Same gender", "Same race",
               "Distance (miles)", "Experience (yr)")

link_data <- map(specs, function(s) {
  lines <- readLines(sprintf("results/tables/link_stats_%s.tex", s))
  body <- lines[(grep("\\\\midrule", lines)[1] + 1):(grep("\\\\bottomrule", lines)[1] - 1)]
  # Parse each row: label & val1 & val2
  map(body, function(line) {
    parts <- trimws(strsplit(gsub("\\\\\\\\", "", line), "&")[[1]])
    list(label = parts[1], estab = parts[2], non_estab = parts[3])
  })
}) %>% set_names(specs)

link_combined <- c(
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "\\multicolumn{1}{c}{ } & \\multicolumn{2}{c}{Orthopedic Surgery} & \\multicolumn{2}{c}{Cardiology} & \\multicolumn{2}{c}{Dermatology} \\\\",
  "\\cmidrule(l{3pt}r{3pt}){2-3} \\cmidrule(l{3pt}r{3pt}){4-5} \\cmidrule(l{3pt}r{3pt}){6-7}",
  " & \\shortstack[r]{Estab.\\\\links} & \\shortstack[r]{Non-estab.\\\\links} & \\shortstack[r]{Estab.\\\\links} & \\shortstack[r]{Non-estab.\\\\links} & \\shortstack[r]{Estab.\\\\links} & \\shortstack[r]{Non-estab.\\\\links}\\\\",
  "\\midrule"
)
for (i in seq_along(link_rows)) {
  vals <- map_chr(specs, function(s) {
    paste0(link_data[[s]][[i]]$estab, " & ", link_data[[s]][[i]]$non_estab)
  })
  link_combined <- c(link_combined,
    paste0(link_rows[i], " & ", paste(vals, collapse = " & "), "\\\\"))
}
link_combined <- c(link_combined, "\\bottomrule", "\\end{tabular}")
writeLines(link_combined, "results/tables/link_stats_all.tex")


# ============================================================
# 3. Main estimation table — full-spec OR + MFX (6 cols)
# ============================================================

est_files <- sprintf("results/tables/estimates_%s.csv", specs)
has_estimates <- all(file.exists(est_files))

if (!has_estimates) {
  message("Skipping estimation tables (estimates CSVs not found -- re-run 2_logit_twfe.R)")
} else {

est <- map(specs, function(s) {
  read_csv(sprintf("results/tables/estimates_%s.csv", s), show_col_types = FALSE)
}) %>% bind_rows()

vars_order <- c("same_sex", "same_prac", "same_race",
                "diff_dist", "diff_age", "diff_gradyear", "peer_referrals")
var_labels <- c(same_sex = "Same gender", same_prac = "Same practice group",
                same_race = "Same race", diff_dist = "Distance",
                diff_age = "Age difference", diff_gradyear = "Experience difference",
                peer_referrals = "Peer referrals")

fmt <- function(x) ifelse(is.na(x), " ", sprintf("%.3f", x))
fmt_se_fn <- function(x) ifelse(is.na(x), " ", paste0("(", sprintf("%.3f", abs(x)), ")"))

# Full specification only (spec 4 = includes peer referrals)
full <- est %>% filter(spec == 4)

main_rows <- character(0)
for (v in vars_order) {
  or_vals <- map_chr(specs, function(s) {
    row <- full %>% filter(term == v, specialty == s)
    if (nrow(row) == 0) " " else fmt(row$or)
  })
  or_ses <- map_chr(specs, function(s) {
    row <- full %>% filter(term == v, specialty == s)
    if (nrow(row) == 0) " " else {
      # Delta-method SE for OR: se(OR) = OR * se(beta)
      or_se <- row$or * row$beta_se
      fmt_se_fn(or_se)
    }
  })
  mfx_vals <- map_chr(specs, function(s) {
    row <- full %>% filter(term == v, specialty == s)
    if (nrow(row) == 0) " " else fmt(row$mfx)
  })
  mfx_ses <- map_chr(specs, function(s) {
    row <- full %>% filter(term == v, specialty == s)
    if (nrow(row) == 0) " " else fmt_se_fn(row$mfx_se)
  })
  main_rows <- c(main_rows,
    paste0(var_labels[v], " & ", paste(or_vals, collapse = " & "), " & ",
           paste(mfx_vals, collapse = " & "), "\\\\"),
    paste0(" & ", paste(or_ses, collapse = " & "), " & ",
           paste(mfx_ses, collapse = " & "), "\\\\"))
}

# Footer
n_quartet <- map_chr(specs, function(s) {
  row <- full %>% filter(specialty == s) %>% slice(1)
  format(row$n_quartet, big.mark = ",")
})
n_choice <- map_chr(specs, function(s) {
  row <- full %>% filter(specialty == s) %>% slice(1)
  format(row$n_choiceset, big.mark = ",")
})

main_table <- c(
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "\\multicolumn{1}{c}{ } & \\multicolumn{3}{c}{Odds ratios} & \\multicolumn{3}{c}{Avg. marginal effects} \\\\",
  "\\cmidrule(l{3pt}r{3pt}){2-4} \\cmidrule(l{3pt}r{3pt}){5-7}",
  " & Ortho & Cardio & Derm & Ortho & Cardio & Derm\\\\",
  "\\midrule",
  main_rows,
  "\\midrule",
  paste0("Year FE & Yes & Yes & Yes & Yes & Yes & Yes\\\\"),
  paste0("Doctor FE & Yes & Yes & Yes & Yes & Yes & Yes\\\\"),
  paste0("Specialist FE & Yes & Yes & Yes & Yes & Yes & Yes\\\\"),
  "\\midrule\\\\",
  paste0("Quartet obs & ", paste(n_quartet, collapse = " & "), " &  &  & \\\\"),
  paste0("Choice-set obs &  &  &  & ", paste(n_choice, collapse = " & "), "\\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(main_table, "results/tables/logit_twfe_main.tex")


# ============================================================
# 4. Progressive specs — OR (12 cols, appendix)
# ============================================================

n_specs <- 4
prog_or_rows <- character(0)
for (v in vars_order) {
  vals <- map_chr(specs, function(s) {
    map_chr(1:n_specs, function(sp) {
      row <- est %>% filter(term == v, specialty == s, spec == sp)
      if (nrow(row) == 0) " " else fmt(row$or)
    }) %>% paste(collapse = " & ")
  }) %>% paste(collapse = " & ")
  ses <- map_chr(specs, function(s) {
    map_chr(1:n_specs, function(sp) {
      row <- est %>% filter(term == v, specialty == s, spec == sp)
      if (nrow(row) == 0) " " else fmt_se_fn(row$or * row$beta_se)
    }) %>% paste(collapse = " & ")
  }) %>% paste(collapse = " & ")
  prog_or_rows <- c(prog_or_rows,
    paste0(var_labels[v], " & ", vals, "\\\\"),
    paste0(" & ", ses, "\\\\"))
}

n_cols <- n_specs * length(specs)
col_nums <- paste(paste0("(", 1:n_cols, ")"), collapse = " & ")

prog_or_table <- c(
  sprintf("\\begin{tabular}{l%s}", paste(rep("r", n_cols), collapse = "")),
  "\\toprule",
  sprintf("\\multicolumn{1}{c}{ } & \\multicolumn{%d}{c}{Ortho} & \\multicolumn{%d}{c}{Cardio} & \\multicolumn{%d}{c}{Derm} \\\\", n_specs, n_specs, n_specs),
  sprintf("\\cmidrule(l{3pt}r{3pt}){2-%d} \\cmidrule(l{3pt}r{3pt}){%d-%d} \\cmidrule(l{3pt}r{3pt}){%d-%d}",
          1 + n_specs, 2 + n_specs, 1 + 2*n_specs, 2 + 2*n_specs, 1 + 3*n_specs),
  paste0(" & ", col_nums, "\\\\"),
  "\\midrule",
  prog_or_rows,
  "\\midrule",
  paste0("Year FE & ", paste(rep("Yes", n_cols), collapse = " & "), "\\\\"),
  paste0("Doctor FE & ", paste(rep("Yes", n_cols), collapse = " & "), "\\\\"),
  paste0("Specialist FE & ", paste(rep("Yes", n_cols), collapse = " & "), "\\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(prog_or_table, "results/tables/logit_twfe_or_progressive.tex")


# ============================================================
# 5. Progressive specs — MFX (12 cols, appendix)
# ============================================================

prog_mfx_rows <- character(0)
for (v in vars_order) {
  vals <- map_chr(specs, function(s) {
    map_chr(1:n_specs, function(sp) {
      row <- est %>% filter(term == v, specialty == s, spec == sp)
      if (nrow(row) == 0) " " else fmt(row$mfx)
    }) %>% paste(collapse = " & ")
  }) %>% paste(collapse = " & ")
  ses <- map_chr(specs, function(s) {
    map_chr(1:n_specs, function(sp) {
      row <- est %>% filter(term == v, specialty == s, spec == sp)
      if (nrow(row) == 0) " " else fmt_se_fn(row$mfx_se)
    }) %>% paste(collapse = " & ")
  }) %>% paste(collapse = " & ")
  prog_mfx_rows <- c(prog_mfx_rows,
    paste0(var_labels[v], " & ", vals, "\\\\"),
    paste0(" & ", ses, "\\\\"))
}

prog_mfx_table <- c(
  sprintf("\\begin{tabular}{l%s}", paste(rep("r", n_cols), collapse = "")),
  "\\toprule",
  sprintf("\\multicolumn{1}{c}{ } & \\multicolumn{%d}{c}{Ortho} & \\multicolumn{%d}{c}{Cardio} & \\multicolumn{%d}{c}{Derm} \\\\", n_specs, n_specs, n_specs),
  sprintf("\\cmidrule(l{3pt}r{3pt}){2-%d} \\cmidrule(l{3pt}r{3pt}){%d-%d} \\cmidrule(l{3pt}r{3pt}){%d-%d}",
          1 + n_specs, 2 + n_specs, 1 + 2*n_specs, 2 + 2*n_specs, 1 + 3*n_specs),
  paste0(" & ", col_nums, "\\\\"),
  "\\midrule",
  prog_mfx_rows,
  "\\midrule",
  paste0("Year FE & ", paste(rep("Yes", n_cols), collapse = " & "), "\\\\"),
  paste0("Doctor FE & ", paste(rep("Yes", n_cols), collapse = " & "), "\\\\"),
  paste0("Specialist FE & ", paste(rep("Yes", n_cols), collapse = " & "), "\\\\"),
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(prog_mfx_table, "results/tables/logit_twfe_mfx_progressive.tex")

} # end if (has_estimates)


# ============================================================
# 6. Dynamics summary stats — stacked panels
# ============================================================

dynamics_panels <- map(specs, function(s) {
  body <- read_tabular_body(sprintf("results/tables/link_stats_by_window_%s.tex", s))
  c(sprintf("\\addlinespace[0.5em]"),
    sprintf("\\multicolumn{5}{l}{\\textbf{%s}}\\\\", spec_labels[s]),
    body)
}) %>% unlist()

dynamics_combined <- c(
  "\\begin{tabular}{lrrrr}",
  "\\toprule",
  "  & Year 1 & Year 2 & Year 3 & Year 4\\\\",
  "\\midrule",
  dynamics_panels,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(dynamics_combined, "results/tables/dynamics_all.tex")


# ============================================================
# 7. Peer referrals — 9 cols (3 specs × 3 specialties)
# ============================================================

peer_data <- map(specs, function(s) {
  lines <- readLines(sprintf("results/tables/peer_referrals_%s.tex", s))
  body_start <- grep("\\\\midrule", lines)[1] + 1
  body_end   <- grep("\\\\bottomrule", lines)[1] - 1
  body <- lines[body_start:body_end]
  # Parse: each row is "label & v1 & v2 & v3\\"
  map(body, function(line) {
    parts <- trimws(strsplit(gsub("\\\\\\\\", "", line), "&")[[1]])
    parts
  })
}) %>% set_names(specs)

# Rows should align across specialties (same variable order)
n_rows <- length(peer_data$ortho)
peer_combined_rows <- character(0)
for (i in seq_len(n_rows)) {
  label <- peer_data$ortho[[i]][1]
  vals <- map_chr(specs, function(s) {
    paste(peer_data[[s]][[i]][-1], collapse = " & ")
  }) %>% paste(collapse = " & ")
  peer_combined_rows <- c(peer_combined_rows,
    paste0(label, " & ", vals, "\\\\"))
}

peer_combined <- c(
  "\\begin{tabular}{lrrrrrrrrr}",
  "\\toprule",
  "\\multicolumn{1}{c}{ } & \\multicolumn{3}{c}{Ortho} & \\multicolumn{3}{c}{Cardio} & \\multicolumn{3}{c}{Derm} \\\\",
  "\\cmidrule(l{3pt}r{3pt}){2-4} \\cmidrule(l{3pt}r{3pt}){5-7} \\cmidrule(l{3pt}r{3pt}){8-10}",
  " & (1) & (2) & (3) & (4) & (5) & (6) & (7) & (8) & (9)\\\\",
  "\\midrule",
  peer_combined_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(peer_combined, "results/tables/peer_referrals_all.tex")


# ============================================================
# 8. Appendix: Link decomposition — stacked panels
# ============================================================

decomp_panels <- map(specs, function(s) {
  body <- read_tabular_body(sprintf("results/tables/app_link_decomposition_%s.tex", s))
  c(sprintf("\\addlinespace[0.5em]"),
    sprintf("\\multicolumn{6}{l}{\\textbf{%s}}\\\\", spec_labels[s]),
    body)
}) %>% unlist()

decomp_combined <- c(
  "\\begin{tabular}{lrrrrr}",
  "\\toprule",
  "  & \\shortstack[r]{Same \\\\ practice} & \\shortstack[r]{Same \\\\ gender} & \\shortstack[r]{Same \\\\ race} & \\shortstack[r]{Mean \\\\ distance} & \\shortstack[r]{N \\\\ links}\\\\",
  "\\midrule",
  decomp_panels,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(decomp_combined, "results/tables/app_link_decomposition_all.tex")


# ============================================================
# 10. Appendix: Robustness no-practice — 6 cols (β + MFX × 3)
# ============================================================

noprac_data <- map(specs, function(s) {
  lines <- readLines(sprintf("results/tables/app_robustness_noprac_%s.tex", s))
  body_start <- grep("\\\\midrule", lines)[1] + 1
  body_end   <- grep("\\\\bottomrule", lines)[1] - 1
  body <- lines[body_start:body_end]
  map(body, function(line) {
    parts <- trimws(strsplit(gsub("\\\\\\\\", "", line), "&")[[1]])
    parts
  })
}) %>% set_names(specs)

n_rows_np <- length(noprac_data$ortho)
noprac_rows <- character(0)
for (i in seq_len(n_rows_np)) {
  label <- noprac_data$ortho[[i]][1]
  vals <- map_chr(specs, function(s) {
    paste(noprac_data[[s]][[i]][-1], collapse = " & ")
  }) %>% paste(collapse = " & ")
  noprac_rows <- c(noprac_rows,
    paste0(label, " & ", vals, "\\\\"))
}

noprac_combined <- c(
  "\\begin{tabular}{lrrrrrr}",
  "\\toprule",
  "\\multicolumn{1}{c}{ } & \\multicolumn{2}{c}{Ortho} & \\multicolumn{2}{c}{Cardio} & \\multicolumn{2}{c}{Derm} \\\\",
  "\\cmidrule(l{3pt}r{3pt}){2-3} \\cmidrule(l{3pt}r{3pt}){4-5} \\cmidrule(l{3pt}r{3pt}){6-7}",
  " & $\\beta$ & MFX & $\\beta$ & MFX & $\\beta$ & MFX\\\\",
  "\\midrule",
  noprac_rows,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(noprac_combined, "results/tables/app_robustness_noprac_all.tex")


# ============================================================
# 11. Appendix: Convergence — stacked panels
# ============================================================

conv_panels <- map(specs, function(s) {
  body <- read_tabular_body(sprintf("results/tables/app_convergence_%s.tex", s))
  c(sprintf("\\addlinespace[0.5em]"),
    sprintf("\\multicolumn{6}{l}{\\textbf{%s}}\\\\", spec_labels[s]),
    body)
}) %>% unlist()

# Read header from ortho version (skip \begin{tabular} and any blank lines before it)
conv_header <- readLines("results/tables/app_convergence_ortho.tex")
conv_header_begin <- grep("\\\\begin\\{tabular\\}", conv_header)[1] + 1
conv_header_end <- grep("\\\\midrule", conv_header)[1]
conv_header_lines <- conv_header[conv_header_begin:conv_header_end]

conv_combined <- c(
  "\\begin{tabular}{lrrrrr}",
  conv_header_lines,
  conv_panels,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(conv_combined, "results/tables/app_convergence_all.tex")


# ============================================================
# 12. Appendix: Link-level conditional logit — stacked or 9 cols
# ============================================================

linkcl_panels <- map(specs, function(s) {
  body <- read_tabular_body(sprintf("results/tables/app_logit_race_mfx_%s.tex", s))
  c(sprintf("\\addlinespace[0.5em]"),
    sprintf("\\multicolumn{4}{l}{\\textbf{%s}}\\\\", spec_labels[s]),
    body)
}) %>% unlist()

# Read header from ortho version (skip \begin{tabular} and any blank lines before it)
linkcl_header <- readLines("results/tables/app_logit_race_mfx_ortho.tex")
linkcl_header_begin <- grep("\\\\begin\\{tabular\\}", linkcl_header)[1] + 1
linkcl_header_end <- grep("\\\\midrule", linkcl_header)[1]
linkcl_header_lines <- linkcl_header[linkcl_header_begin:linkcl_header_end]

linkcl_combined <- c(
  "\\begin{tabular}{lrrr}",
  linkcl_header_lines,
  linkcl_panels,
  "\\bottomrule",
  "\\end{tabular}"
)
writeLines(linkcl_combined, "results/tables/app_logit_race_mfx_all.tex")


message("All combined tables written to results/tables/")
