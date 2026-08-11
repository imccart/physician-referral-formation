# Mover-selection appendix tables (data.table used for the multi-million-row referral panel):
#   (1) mover_markets.tex     -- each mover's origin vs destination market
#   (2) mover_disposition.tex -- movers (pre-move) vs non-movers referral behavior
# Reads the MD-PPAS practice-size/composition lookup from results/tables/prac_comp_lookup.csv.
# Sourced by _main.R in the post-estimation app-scripts block; data.table is provided by the
# driver's pacman::p_load (self-contained: reads its own CSVs, loops all three specialties).

specs <- c("ortho","cardioem","derm")
size  <- fread("results/tables/prac_comp_lookup.csv"); size[, gk := as.character(group1)]
size  <- unique(size, by = c("gk","Year"))

d <- rbindlist(lapply(specs, function(sp){
  x <- fread(sprintf("data/output/df_full_referrals_%s.csv", sp),
             select = c("doctor","specialist","Year","total_pcp_patients","doc_group","spec_group",
                        "doc_sex","spec_sex","doc_race","spec_race","doc_hrr","spec_hrr",
                        "doc_birth","doc_grad_year"))
  x[, `:=`(same_prac = as.integer(doc_group==spec_group), same_sex = as.integer(doc_sex==spec_sex),
           same_race = as.integer(doc_race==spec_race), xhrr = as.integer(doc_hrr!=spec_hrr),
           gk = as.character(doc_group), specialty = sp)]
  merge(x, size[, .(gk, Year, size)], by = c("gk","Year"), all.x = TRUE)
}))

movers <- unique(rbindlist(lapply(specs, function(sp)
  fread(sprintf("data/output/df_logit_movers_%s.csv", sp),
        select = c("doctor","Year","origin","destination"))[, .(move_year = min(Year),
        origin = origin[1], destination = destination[1]), by = doctor])), by = "doctor")

## ---- Table 1: origin vs destination markets ----
pcp_any <- d[, .(any_wp = as.integer(any(same_prac==1, na.rm=TRUE))), by = .(doc_hrr, doctor)]
hrr <- d[, .(wp_rate = mean(same_prac, na.rm=TRUE), mean_prac_size = mean(size, na.rm=TRUE),
             n_spec = uniqueN(specialist), n_pcp = uniqueN(doctor)), by = doc_hrr]
hrr[, spec_per_pcp := n_spec / n_pcp]
hrr <- merge(hrr, pcp_any[, .(share_pcp_wp = mean(any_wp)), by = doc_hrr], by = "doc_hrr")
V <- c("wp_rate","share_pcp_wp","mean_prac_size","n_spec","n_pcp","spec_per_pcp")
mo <- merge(movers, hrr, by.x="origin", by.y="doc_hrr"); setnames(mo, V, paste0("o_",V))
mo <- merge(mo, hrr, by.x="destination", by.y="doc_hrr"); setnames(mo, V, paste0("d_",V))
mkt <- sapply(V, function(v) c(mean(mo[[paste0("o_",v)]], na.rm=TRUE), mean(mo[[paste0("d_",v)]], na.rm=TRUE)))

n3 <- function(x) format(round(x), big.mark=",")
writeLines(c(
"\\begin{tabular}{lcc}", "\\toprule", " & Origin & Destination \\\\", "\\midrule",
"\\multicolumn{3}{l}{\\textit{Referral facilitation}} \\\\",
sprintf("\\quad Within-practice referral share & %.3f & %.3f \\\\", mkt[1,"wp_rate"], mkt[2,"wp_rate"]),
sprintf("\\quad Share of PCPs with a same-practice specialist & %.3f & %.3f \\\\", mkt[1,"share_pcp_wp"], mkt[2,"share_pcp_wp"]),
sprintf("\\quad Mean practice size & %s & %s \\\\", n3(mkt[1,"mean_prac_size"]), n3(mkt[2,"mean_prac_size"])),
"\\addlinespace", "\\multicolumn{3}{l}{\\textit{Market size and density}} \\\\",
sprintf("\\quad Specialists in market & %s & %s \\\\", n3(mkt[1,"n_spec"]), n3(mkt[2,"n_spec"])),
sprintf("\\quad PCPs in market & %s & %s \\\\", n3(mkt[1,"n_pcp"]), n3(mkt[2,"n_pcp"])),
sprintf("\\quad Specialists per PCP & %.1f & %.1f \\\\", mkt[1,"spec_per_pcp"], mkt[2,"spec_per_pcp"]),
"\\bottomrule", "\\end{tabular}"), "results/tables/mover_markets.tex")

## ---- Table 2: movers (pre-move) vs non-movers disposition ----
dd <- merge(d, movers[, .(doctor, move_year)], by = "doctor", all.x = TRUE)
dd[, is_mover := as.integer(!is.na(move_year))]
dd <- dd[is.na(move_year) | Year < move_year]
py <- dd[, .(n_spec = uniqueN(specialist), wp = mean(same_prac,na.rm=TRUE), xhrr = mean(xhrr,na.rm=TRUE),
             ss = mean(same_sex,na.rm=TRUE), sr = mean(same_race,na.rm=TRUE), panel = total_pcp_patients[1],
             group = doc_group[1], is_mover = is_mover[1], female = as.integer(doc_sex[1]=="F"),
             birth = as.integer(doc_birth[1]), gradyear = as.integer(doc_grad_year[1])),
         by = .(doctor, Year, specialty)]   # within specialty, so counts are per-specialty
py[, gk := as.character(group)]; py <- merge(py, size[, .(gk,Year,size)], by=c("gk","Year"), all.x=TRUE)
pcp <- py[panel > 0, .(is_mover = is_mover[1], n_spec = mean(n_spec),
      rate = mean(n_spec/panel), wp = mean(wp), xhrr = mean(xhrr,na.rm=TRUE), ss = mean(ss,na.rm=TRUE),
      sr = mean(sr,na.rm=TRUE), female = female[1], age = mean(Year)-birth[1],
      experience = mean(Year)-gradyear[1], lpanel = log(mean(panel)), lsize = log(mean(size,na.rm=TRUE))),
      by = .(doctor, specialty)]

ctrl <- c("is_mover","female","age","experience","lpanel","lsize","specialty")
row <- function(labtxt, v, dec) {
  mm <- mean(pcp[is_mover==1][[v]], na.rm=TRUE); nm <- mean(pcp[is_mover==0][[v]], na.rm=TRUE)
  b  <- coef(lm(reformulate(ctrl, v), pcp))["is_mover"]
  sprintf("%s & %.*f & %.*f & %+.*f \\\\", labtxt, dec, mm, dec, nm, dec, b)
}
writeLines(c(
"\\begin{tabular}{lccc}", "\\toprule",
" & Movers & Non-movers & Adj.\\ diff. \\\\", "\\midrule",
row("Within-practice referral share","wp",3),
row("Cross-region referral share","xhrr",3),
row("Same-gender referral share","ss",3),
row("Same-race referral share","sr",3),
row("Distinct specialists per year","n_spec",2),
row("Specialists per patient","rate",3),
"\\bottomrule", "\\end{tabular}"), "results/tables/mover_disposition.tex")

cat("wrote mover_markets.tex and mover_disposition.tex\n")
