# =============================================================================
# 02_build.R – build the person-interval dataset (participant-level; stays local)
# Output: object `iv` in memory; data/derived/intervals.rds (do NOT share)
#         outputs/02_flow.csv (aggregate counts)
# =============================================================================

message("02: building person-intervals")
V <- cfg$v; idv <- V$id; dv <- V$day

# Last observation carried forward within participant (data sorted by id, day)
locf <- function(x, id) {
  if (length(x) < 2) return(x)
  for (i in 2:length(x)) if (is.na(x[i]) && id[i] == id[i - 1]) x[i] <- x[i - 1]
  x
}
person_first <- function(d, var) {
  d <- d[order(d[[idv]], d[[dv]]), ]
  x <- cols(d, var)[[1]]; ok <- !is.na(x) & x != ""
  d <- d[ok, ]; x <- x[ok]
  setNames(x[!duplicated(d[[idv]])], d[[idv]][!duplicated(d[[idv]])])
}
person_last <- function(d, var) {
  d <- d[order(d[[idv]], -d[[dv]]), ]
  x <- cols(d, var)[[1]]; ok <- !is.na(x) & x != ""
  d <- d[ok, ]; x <- x[ok]
  setNames(x[!duplicated(d[[idv]])], d[[idv]][!duplicated(d[[idv]])])
}

# ---- About You: employment, age, demographics -------------------------------
ab <- tabs$about
ab <- ab[order(ab[[idv]], ab[[dv]]), ]
ab$emp    <- keep_codes(ab[[V$employment]], 1:4)          # 5 = prefer not -> NA
age_raw   <- num(ab[[V$age]])
offset    <- tapply(age_raw - ab[[dv]] / 365.25, ab[[idv]], stats::median, na.rm = TRUE)
ab$age    <- as.numeric(offset[ab[[idv]]]) + ab[[dv]] / 365.25   # fills missing ages
ab$educ   <- locf(keep_codes(cols(ab, V$education)[[1]], 1:8), ab[[idv]])
ab$income <- locf(keep_codes(cols(ab, V$income)[[1]], 1:6), ab[[idv]])
first_day <- tapply(ab[[dv]], ab[[idv]], min)

sex_new <- keep_codes(cols(ab, V$sex_birth)[[1]], 1:2)
sex_old <- keep_codes(cols(ab, V$sex)[[1]], 1:2)
ab$sex_any <- ifelse(!is.na(sex_new), sex_new, sex_old)
sex_p   <- person_last(ab, "sex_any")

# ---- Chain consecutive employment reports into non-overlapping intervals ----
abv <- ab[!is.na(ab$emp), ]
rows_by_id <- split(seq_len(nrow(abv)), abv[[idv]])
pairs <- lapply(rows_by_id, function(r) {
  days <- abv[[dv]][r]; out <- list(); i <- 1
  while (i < length(r)) {
    j <- which(days > days[i] & days - days[i] >= cfg$gap_min_days)
    j <- j[j > i]
    if (!length(j)) break
    j <- j[1]; out[[length(out) + 1]] <- c(r[i], r[j]); i <- j
  }
  out[[length(out) + 1]] <- c(r[i], NA)   # last report: no follow-up (used for IPW)
  if (length(out)) do.call(rbind, out) else NULL
})
pairs <- do.call(rbind, pairs)
if (is.null(pairs)) stop("No pairs of employment reports found.", call. = FALSE)

iv <- data.frame(
  fox_insight_id = abv[[idv]][pairs[, 1]],
  t = abv[[dv]][pairs[, 1]], t1 = abv[[dv]][pairs[, 2]],
  emp0 = abv$emp[pairs[, 1]], emp1 = abv$emp[pairs[, 2]],
  age = abv$age[pairs[, 1]], educ = abv$educ[pairs[, 1]], income = abv$income[pairs[, 1]],
  stringsAsFactors = FALSE)
names(iv)[1] <- idv
iv$row_id   <- seq_len(nrow(iv))
iv$gap_days <- iv$t1 - iv$t
iv$gap_yrs  <- iv$gap_days / 365.25
iv$yrs_in_study <- (iv$t - as.numeric(first_day[iv[[idv]]])) / 365.25
iv$female   <- as.integer(as.numeric(sex_p[iv[[idv]]]) == 2)

# ---- Cohort (PD / control) and registration variables -----------------------
init_pd <- num(person_first(tabs$users, V$init_pd))
names(init_pd) <- names(person_first(tabs$users, V$init_pd))
curr_pd <- num(person_last(tabs$general, V$curr_pd))
names(curr_pd) <- names(person_last(tabs$general, V$curr_pd))
ip <- init_pd[iv[[idv]]]; cp <- curr_pd[iv[[idv]]]; cp[is.na(cp)] <- ip[is.na(cp)]
iv$cohort <- ifelse(ip %in% 1 & cp %in% 1, "PD", ifelse(ip %in% 0 & cp %in% 0, "Control", NA))
iv$pd <- as.integer(iv$cohort == "PD")

ypd <- keep_codes(person_first(tabs$users, V$years_pd), 0:2)
names(ypd) <- names(person_first(tabs$users, V$years_pd))
iv$years_pd <- factor(ypd[iv[[idv]]], levels = 0:2, labels = c("<3", "3-10", "11+"))

ctry <- person_first(tabs$users, V$country)
us <- grepl("united states|^usa?$|^u\\.s\\.", ctry[iv[[idv]]], ignore.case = TRUE)
iv$country <- factor(ifelse(is.na(ctry[iv[[idv]]]), "Unknown", ifelse(us, "USA", "Other")),
                     levels = c("USA", "Other", "Unknown"))

iv$educ3 <- factor(cut(iv$educ, c(0, 3, 5, 8), labels = c("School/some college",
                   "Associate/bachelor", "Postgraduate")))
iv$income4 <- as.character(cut(iv$income, c(0, 3, 5, 6), labels = c("<50k", "50-99k", "100k+")))
iv$income4[is.na(iv$income4)] <- "Missing"
iv$income4 <- factor(iv$income4, levels = c("<50k", "50-99k", "100k+", "Missing"))

# ---- Outcomes ----------------------------------------------------------------
working0 <- iv$emp0 %in% 1:2
has1     <- !is.na(iv$emp1)
iv$exit         <- ifelse(working0 & has1, as.integer(iv$emp1 %in% 3:4), NA)
iv$exit_retired <- ifelse(working0 & iv$emp1 %in% c(1, 2, 3), as.integer(iv$emp1 == 3), NA)
iv$exit_unemp   <- ifelse(working0 & iv$emp1 %in% c(1, 2, 4), as.integer(iv$emp1 == 4), NA)
# reduced hours: full-time at t; intervals ending in exit are excluded (competing event)
iv$reduce       <- ifelse(iv$emp0 %in% 1 & iv$emp1 %in% 1:2, as.integer(iv$emp1 == 2), NA)
iv$rtw          <- ifelse(iv$emp0 %in% 3:4 & has1, as.integer(iv$emp1 %in% 1:2), NA)

# ---- Questionnaire-derived scores, per response ------------------------------
nm <- tabs$nonmove
for (v in c(V$sleepiness, V$insomnia, V$rls, V$nocturia, V$rbd_nms, V$anxiety,
            V$concentration, V$smell)) nm[[v]] <- keep_codes(cols(nm, v)[[1]], 0:1)

md <- tabs$mood
for (v in cfg$gds_items) {
  x <- keep_codes(cols(md, v)[[1]], 0:1)
  md[[v]] <- if (v %in% cfg$gds_reversed) 1 - x else x      # 1 = depressive answer
}
md$gds14      <- sum_score(md, setdiff(cfg$gds_items, V$energy), 0:1, max_missing = 2)
md$depressed  <- as.integer(md$gds14 >= cfg$gds_cutoff)
md$low_energy <- md[[V$energy]]                               # 1 = not full of energy

mv <- tabs$move;  mv$updrs2 <- sum_score(mv, cfg$updrs2_items, 0:4, max_missing = 2)
dl <- tabs$daily
if (!is.null(dl)) dl$pdaq <- sum_score(dl, cfg$pdaq_items, 0:4, max_missing = 2)

me <- tabs$meds
if (!is.null(me)) {
  av <- intersect(cfg$agonist_vars, names(me))
  me$agonist <- if (length(av)) as.integer(rowSums(sapply(av, function(v)
    num(me[[v]]) %in% 1), na.rm = TRUE) > 0) else NA
}
pp <- tabs$pdprop2
if (!is.null(pp)) {
  anyhit <- function(prefix) as.integer(rowSums(sapply(paste0(prefix, 1:5), function(v)
    num(cols(pp, v)[[1]]) %in% 1)) > 0)
  pp$pdprop_fatigue <- anyhit("pdprop2fatig_")
  pp$pdprop_sleep   <- anyhit("pdprop2slp_")
}
sl <- tabs$sleep
if (!is.null(sl)) sl$rbd1q <- keep_codes(cols(sl, V$rbd1q)[[1]], 0:1)

# ---- Match to interval start (exposures, covariates) -------------------------
a0 <- data.frame(row_id = iv$row_id, iv[idv], lo = iv$t - cfg$win_before,
                 hi = iv$t + cfg$win_after, target = iv$t)
nm_vars <- c(V$sleepiness, V$insomnia, V$rls, V$nocturia, V$rbd_nms, V$anxiety,
             V$concentration, V$smell)
add <- function(x) iv <<- merge(iv, x, by = "row_id", all.x = TRUE, sort = FALSE)
add(match_nearest(a0, nm, nm_vars, "x_"))
add(match_nearest(a0, md, c("gds14", "depressed", "low_energy"), "x_"))
add(match_nearest(a0, mv, "updrs2", "x_"))
if (!is.null(dl)) add(match_nearest(a0, dl, "pdaq", "x_")) else iv$x_pdaq <- NA
if (!is.null(me)) add(match_nearest(a0, me, "agonist", "x_")) else iv$x_agonist <- NA
if (!is.null(pp)) add(match_nearest(a0, pp, "pdprop_sleep", "x_")) else iv$x_pdprop_sleep <- NA
if (!is.null(sl)) add(match_nearest(transform(a0, lo = target - 400), sl, "rbd1q", "x_"))
iv$x_agonist[iv$cohort %in% "Control"] <- 0   # controls do not complete MedsCurrPD

# Chronicity over the preceding year
ac <- data.frame(row_id = iv$row_id, iv[idv], lo = iv$t - cfg$chronic_days, hi = iv$t)
add(window_mean(ac, nm, c(V$sleepiness, V$insomnia), "chr_"))
chron3 <- function(p) factor(ifelse(is.na(p), NA, ifelse(p == 0, "Never",
                       ifelse(p < 0.5, "Sometimes", "Persistent"))),
                       levels = c("Never", "Sometimes", "Persistent"))
iv$chr_sleepiness <- chron3(iv[[paste0("chr_", V$sleepiness)]])
iv$chr_insomnia   <- chron3(iv[[paste0("chr_", V$insomnia)]])

# ---- Mediators: during the interval, before the outcome report ---------------
am <- data.frame(row_id = iv$row_id, iv[idv], lo = iv$t + cfg$med_start,
                 hi = iv$t1 - cfg$med_end_buffer, target = (iv$t + iv$t1) / 2)
add(match_nearest(am, nm, c(V$anxiety, V$concentration), "m_"))
add(match_nearest(am, md, c("depressed", "low_energy"), "m_"))
if (!is.null(pp)) add(match_nearest(am, pp, "pdprop_fatigue", "m_")) else iv$m_pdprop_fatigue <- NA

# ---- Friendly analysis names ---------------------------------------------------
ren <- c(sleepiness = V$sleepiness, insomnia = V$insomnia, rls = V$rls, nocturia = V$nocturia,
         rbd = V$rbd_nms, anxiety = V$anxiety, concentration = V$concentration, smell = V$smell)
for (k in names(ren)) iv[[k]] <- iv[[paste0("x_", ren[[k]])]]
iv$pdprop_sleep <- iv$x_pdprop_sleep
iv$gds14 <- iv$x_gds14; iv$depressed <- iv$x_depressed
iv$updrs2 <- iv$x_updrs2; iv$pdaq <- iv$x_pdaq; iv$agonist <- iv$x_agonist
iv$med_fatigue    <- iv$m_pdprop_fatigue
iv$med_low_energy <- iv$m_low_energy
iv$med_depressed  <- iv$m_depressed
iv$med_anxiety    <- iv[[paste0("m_", V$anxiety)]]
iv$med_concentration <- iv[[paste0("m_", V$concentration)]]
iv <- iv[order(iv$row_id), ]

# =============================================================================
# Plan v0.2 additions (all optional tables)
# =============================================================================
pfirst_num <- function(d, var) { x <- person_first(d, var); setNames(num(x), names(x)) }
person_val <- function(d, var, ok = NULL) {   # person-level value from a one-time table
  x <- pfirst_num(d, var); if (!is.null(ok)) x[!(x %in% ok)] <- NA; as.numeric(x[iv[[idv]]]) }
person_day <- function(d) { x <- tapply(d[[dv]], d[[idv]], min); as.numeric(x[iv[[idv]]]) }
aA <- data.frame(row_id = iv$row_id, iv[idv], lo = iv$t - cfg$annual_lookback,
                 hi = iv$t + cfg$win_after, target = iv$t)

# Job physical demands (PASE)
pa <- tabs$pase
if (!is.null(pa)) {
  pa$workactive <- keep_codes(cols(pa, "workactive")[[1]], 1:4)
  add(match_nearest(aA, pa, "workactive", "x_"))
} else iv$x_workactive <- NA
iv$jobdemand <- factor(ifelse(is.na(iv$x_workactive), "Missing",
                  c("Sitting", "Sitting/standing", "Walking/handling", "Heavy manual")[iv$x_workactive]),
                  levels = c("Sitting", "Sitting/standing", "Walking/handling", "Heavy manual", "Missing"))
iv$job_active <- ifelse(is.na(iv$x_workactive), NA, as.integer(iv$x_workactive >= 2))

# Other long-term conditions (Your Current Health)
cu <- tabs$curr
grp_vars <- names(cfg$ltc_groups)
if (!is.null(cu)) {
  lim <- sapply(cfg$ltc_lim_vars, function(v) num(cols(cu, v)[[1]]) %in% 1)
  cu$ltc_lim_n <- rowSums(matrix(lim, nrow = nrow(cu)))
  for (g in grp_vars) cu[[paste0("grp_", g)]] <- as.integer(rowSums(matrix(sapply(cfg$ltc_groups[[g]],
    function(v) num(cols(cu, v)[[1]]) %in% 1), nrow = nrow(cu))) > 0)
  add(match_nearest(aA, cu, c("ltc_lim_n", paste0("grp_", grp_vars)), "x_"))
} else { iv$x_ltc_lim_n <- NA; for (g in grp_vars) iv[[paste0("x_grp_", g)]] <- NA }
iv$ltc_n3 <- factor(ifelse(is.na(iv$x_ltc_lim_n), NA, pmin(iv$x_ltc_lim_n, 2)), levels = 0:2,
                    labels = c("0", "1", "2+"))
iv$ltc_any <- as.integer(iv$x_ltc_lim_n > 0)
for (g in grp_vars) iv[[paste0("ltc_", g)]] <- iv[[paste0("x_grp_", g)]]

# Problems with usual activities (EQ-5D-5L), at t and around t1 (H7)
eq <- tabs$eq5d
if (!is.null(eq)) {
  eq$active <- keep_codes(cols(eq, "active")[[1]], 0:4)
  add(match_nearest(a0, eq, "active", "x_"))
  a1 <- data.frame(row_id = iv$row_id, iv[idv], lo = iv$t1 - cfg$win_before,
                   hi = iv$t1 + cfg$win_before, target = iv$t1)
  add(match_nearest(a1[!is.na(a1$target), ], eq, "active", "y_"))
} else { iv$x_active <- NA; iv$y_active <- NA }
iv$activity_problem <- ifelse(iv$x_active %in% 0:1 & !is.na(iv$y_active) & iv$emp0 %in% 1:2,
                              as.integer(iv$y_active >= 2), NA)

# Sleep apnoea and insomnia diagnoses (Medical History, pre-Oct 2017; descriptive only)
mh <- tabs$medhis
iv$apnoea   <- if (!is.null(mh)) person_val(mh, "sleepap", 0:1) else NA
iv$insom_dx <- if (!is.null(mh)) person_val(mh, "insom", 0:1) else NA

# Perceived stress (PSS-10), only for intervals starting after completion
mi <- tabs$mind
if (!is.null(mi)) {
  for (v in cfg$pss_items) {
    x <- keep_codes(cols(mi, v)[[1]], 1:5) - 1
    mi[[v]] <- if (v %in% cfg$pss_reversed) 4 - x else x
  }
  mi$pss <- sum_score(mi, cfg$pss_items, 0:4, max_missing = 1)
  pss_p <- person_val(mi, "pss"); pss_day <- person_day(mi)
  iv$pss <- ifelse(!is.na(pss_day) & iv$t >= pss_day, pss_p, NA)
} else iv$pss <- NA

# Pandemic lay-offs (COVID-19 survey): flag the interval in which the lay-off would be reported
cv <- tabs$cov
if (!is.null(cv)) {
  laid <- person_val(cv, "covsocdiswork", 1:9) == 5; cday <- person_day(cv)
  iv$covid_layoff <- as.integer(laid %in% TRUE & iv$t <= cday & !is.na(iv$t1) & iv$t1 >= cday - 365)
} else iv$covid_layoff <- 0

# Reasons for leaving work (Financial and Social Impact 2025; validation)
fi <- tabs$fnclimp
if (!is.null(fi)) {
  iv$fi_resp    <- as.integer(iv[[idv]] %in% fi[[idv]])
  iv$fi_pd_stop <- ifelse(iv$fi_resp == 1, as.integer(person_val(fi, "fnclimpjobimpearlyretrmnt", 0:1) %in% 1), NA)
  iv$fi_pd_hours <- ifelse(iv$fi_resp == 1, as.integer(person_val(fi, "fnclimpjobimpwrkfwrhrs", 0:1) %in% 1), NA)
} else { iv$fi_resp <- 0; iv$fi_pd_stop <- NA; iv$fi_pd_hours <- NA }

# Night or shift work (PD Microbiome survey; subset sensitivity analysis)
mb <- tabs$pdmb
iv$nightwork <- if (!is.null(mb)) person_val(mb, "pdmbworknight", 0:1) else NA

# Approximate calendar time from one-time surveys with known fielding windows
iv$anchored <- FALSE; iv$anchor_hw <- NA; iv$cal_t <- as.Date(NA); iv$cal_t1 <- as.Date(NA)
for (nm_a in names(cfg$anchor_windows)) {
  d_a <- tabs[[nm_a]]; if (is.null(d_a)) next
  w <- as.Date(cfg$anchor_windows[[nm_a]]); hw <- as.numeric(diff(w)) / 2; mid <- w[1] + hw
  aday <- person_day(d_a)
  use <- !is.na(aday) & (!iv$anchored | (iv$anchor_hw > hw))   # keep the narrowest window
  iv$cal_t[use]  <- mid + (iv$t[use] - aday[use])
  iv$cal_t1[use] <- mid + (iv$t1[use] - aday[use])
  iv$anchor_hw[use] <- hw; iv$anchored[use] <- TRUE
}
pw <- as.Date(cfg$pandemic)
iv$pandemic_possible <- ifelse(iv$anchored & !is.na(iv$cal_t1),
  as.integer(iv$cal_t1 + iv$anchor_hw >= pw[1] & iv$cal_t - iv$anchor_hw <= pw[2]), NA)

iv <- iv[order(iv$row_id), ]

# ---- Eligibility flags ---------------------------------------------------------
iv$working0  <- working0
iv$ok_gap    <- !is.na(iv$gap_days) & iv$gap_days >= cfg$gap_min_days & iv$gap_days <= cfg$gap_max_days
iv$ok_age    <- !is.na(iv$age) & iv$age >= cfg$age_min & iv$age < cfg$age_max
iv$ok_cohort <- !is.na(iv$cohort)
iv$eligible  <- working0 & iv$ok_gap & iv$ok_age & iv$ok_cohort

dir.create("data/derived", showWarnings = FALSE, recursive = TRUE)
saveRDS(iv, "data/derived/intervals.rds")

# ---- Flow (aggregate) ---------------------------------------------------------
flow_row <- function(step, keep) data.frame(step = step,
  intervals = sum(keep), participants = length(unique(iv[[idv]][keep])),
  pd_participants = length(unique(iv[[idv]][keep & iv$cohort %in% "PD"])))
k1 <- rep(TRUE, nrow(iv)); k2 <- k1 & working0; k3 <- k2 & iv$ok_age
k4 <- k3 & iv$ok_cohort; k5 <- k4 & iv$ok_gap
k6 <- k5 & !is.na(iv$sleepiness)
flow <- rbind(
  flow_row("All employment reports that could start an interval", k1),
  flow_row("Employed at report", k2),
  flow_row(sprintf("Aged %d to <%d", cfg$age_min, cfg$age_max), k3),
  flow_row("PD or control cohort (stable diagnosis status)", k4),
  flow_row(sprintf("Next employment report %d-%d days later", cfg$gap_min_days, cfg$gap_max_days), k5),
  flow_row("NMS-Quest within exposure window", k6))
flow$exit_events_pd <- c(rep(NA, 5), sum(iv$exit[k6 & iv$cohort %in% "PD"], na.rm = TRUE))
write_out(suppress_counts(flow, c("intervals", "participants", "pd_participants", "exit_events_pd")),
          "02_flow.csv")
