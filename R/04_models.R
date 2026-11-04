# =============================================================================
# 04_models.R – primary, secondary, mediation, effect modification, sensitivity
# All outputs are aggregate model estimates.
# =============================================================================

message("04: models (this can take a while with cfg$n_boot = 1000)")
set.seed(cfg$seed)

terms_m1 <- c("ns(age, df = 3)", "female", "educ3", "income4", "country", "years_pd",
              "yrs_in_study", "gap_yrs")
terms_m2 <- c(terms_m1, "updrs2", "pdaq", "agonist", "jobdemand", "ltc_n3")
terms_m3 <- c(terms_m2, "gds14", "anxiety")

vars_of <- function(term) all.vars(str2lang(term))

# Build a usable formula + complete-case data: drops covariates that are absent,
# entirely missing, or constant, and records what was dropped.
prep <- function(d, outcome, expo_terms, cov_terms, wvar = NULL) {
  usable <- function(t) { v <- vars_of(t)
    all(v %in% names(d)) && all(vapply(v, function(z) any(!is.na(d[[z]])), TRUE)) }
  ok <- vapply(cov_terms, usable, TRUE)
  dropped <- cov_terms[!ok]; cov_terms <- cov_terms[ok]
  f  <- stats::reformulate(c(expo_terms, cov_terms), response = outcome)
  dd <- droplevels(complete_for(d, f, extra = c(cfg$v$id, wvar)))
  const <- vapply(cov_terms, function(t) any(vapply(vars_of(t), function(z)
    length(unique(dd[[z]])) < 2, TRUE)), TRUE)
  dropped <- c(dropped, cov_terms[const]); cov_terms <- cov_terms[!const]
  f <- stats::reformulate(c(expo_terms, cov_terms), response = outcome)
  list(f = f, d = dd, cov = cov_terms, dropped = paste(dropped, collapse = "; "))
}

# One exposure–outcome analysis: cluster-robust OR + standardised risks (+ bootstrap)
run_one <- function(label, d, outcome, expo, cov_terms, boot = TRUE, wvar = NULL,
                    B = cfg$n_boot, extra_expo = character(0)) {
  p <- prep(d, outcome, c(expo, extra_expo), cov_terms, wvar)
  dd <- p$d
  empty <- data.frame(analysis = label, outcome = outcome, exposure = expo,
                      n_intervals = nrow(dd), n_participants = length(unique(dd[[cfg$v$id]])),
                      n_events = sum(dd[[outcome]]), note = "too few events")
  if (nrow(dd) < 50 || sum(dd[[outcome]]) < 10 || length(unique(dd[[expo]])) < 2) return(empty)
  w <- if (is.null(wvar)) NULL else dd[[wvar]]
  fit <- suppressWarnings(fit_logit(p$f, dd, w))
  ct <- coef_table(fit, dd[[cfg$v$id]], expo)
  g  <- gcomp(fit, dd, expo, w)
  out <- data.frame(analysis = label, outcome = outcome, exposure = expo,
                    n_intervals = nrow(dd), n_participants = length(unique(dd[[cfg$v$id]])),
                    n_events = sum(dd[[outcome]]),
                    events_exposed = sum(dd[[outcome]][dd[[expo]] == 1]),
                    or = ct$or, or_lo = ct$or_lo, or_hi = ct$or_hi, p = ct$p,
                    risk_exposed = g[["risk_exposed"]], risk_unexposed = g[["risk_unexposed"]],
                    rd = g[["rd"]], rr = g[["rr"]], dropped_covariates = p$dropped)
  if (boot) {
    est <- function(b) {
      wb <- if (is.null(wvar)) NULL else b[[wvar]]
      f2 <- suppressWarnings(fit_logit(p$f, b, wb)); gcomp(f2, b, expo, wb)[c("rd", "rr")]
    }
    ci <- boot_ci(est, dd, cfg$v$id, B)
    out$rd_lo <- ci[1, 1]; out$rd_hi <- ci[1, 2]; out$rr_lo <- ci[2, 1]; out$rr_hi <- ci[2, 2]
    lim <- if (out$rr >= 1) out$rr_lo else out$rr_hi
    out$evalue_point <- evalue(out$rr)
    out$evalue_ci <- if (is.na(lim) || (out$rr >= 1 && lim <= 1) || (out$rr < 1 && lim >= 1)) 1 else evalue(lim)
  }
  out
}

bind <- function(lst) {
  cols_all <- unique(unlist(lapply(lst, names)))
  do.call(rbind, lapply(lst, function(x) { for (k in setdiff(cols_all, names(x))) x[[k]] <- NA; x[cols_all] }))
}
export <- function(res, file) {
  res$n_events <- as.numeric(res$n_events)
  write_out(suppress_counts(res, intersect(c("n_events", "events_exposed"), names(res))), file)
}

pd  <- el[el$cohort == "PD", ]
pexp <- names(cfg$exposures_primary)          # "sleepiness", "insomnia"

# ---- Decision rule ----------------------------------------------------------
n_exit_pd <- sum(pd$exit[!is.na(pd$sleepiness)], na.rm = TRUE)
message(sprintf("  PD work-exit events with exposure data: %d", n_exit_pd))
if (n_exit_pd < cfg$min_exit_events)
  warning(sprintf("Fewer than %d exit events: see Appendix B (widen interval window to 730 days and report as a deviation).",
                  cfg$min_exit_events), call. = FALSE)

# ---- 1. Primary: H1/H2 -------------------------------------------------------
message("  primary models")
prim <- list()
for (x in pexp) {
  prim[[length(prim) + 1]] <- run_one("Model 1", pd, "exit", x, terms_m1, boot = FALSE)
  prim[[length(prim) + 1]] <- run_one("Model 2 (primary)", pd, "exit", x, terms_m2, boot = TRUE)
  prim[[length(prim) + 1]] <- run_one("Model 3", pd, "exit", x, terms_m3, boot = FALSE)
}
prim <- bind(prim)
is2 <- prim$analysis == "Model 2 (primary)"
prim$p_holm <- NA; prim$p_holm[is2] <- stats::p.adjust(prim$p[is2], "holm")
export(prim, "04a_primary.csv")

# ---- 2. Secondary outcome (H3) and other sleep exposures (exploratory) ---------
message("  secondary models")
sec <- list()
for (x in pexp) sec[[length(sec) + 1]] <- run_one("H3: full-time to part-time", pd, "reduce", x, terms_m2)
for (x in names(cfg$exposures_secondary))
  sec[[length(sec) + 1]] <- run_one("Exploratory exposure", pd, "exit", x, terms_m2)
if ("x_rbd1q" %in% names(pd)) {
  pd$rbd1q <- pd$x_rbd1q
  sec[[length(sec) + 1]] <- run_one("Exploratory exposure", pd, "exit", "rbd1q", terms_m2)
}
for (x in pexp) sec[[length(sec) + 1]] <- run_one("H7: difficulty with usual activities (EQ-5D)", pd,
                                                    "activity_problem", x, terms_m2)
rtw_d <- iv[iv$cohort %in% "PD" & iv$ok_gap & iv$ok_age & iv$emp0 %in% 3:4, ]
for (x in pexp) sec[[length(sec) + 1]] <- run_one("Exploratory: return to work", rtw_d, "rtw", x, terms_m2)
export(bind(sec), "04b_secondary.csv")

# ---- 3. Chronicity (H5) --------------------------------------------------------
message("  chronicity")
chr <- list()
for (x in pexp) {
  cx <- paste0("chr_", x)
  p <- prep(pd, "exit", cx, terms_m2)
  if (nrow(p$d) >= 50 && length(unique(p$d[[cx]])) == 3) {
    fit <- suppressWarnings(fit_logit(p$f, p$d))
    ct <- coef_table(fit, p$d[[cfg$v$id]], paste0(cx, c("Sometimes", "Persistent")))
    risks <- sapply(levels(p$d[[cx]]), function(l) { z <- p$d; z[[cx]] <- factor(l, levels = levels(p$d[[cx]]))
      mean(stats::predict(fit, z, type = "response")) })
    # trend: ordinal score 0/1/2
    p$d$.score <- as.numeric(p$d[[cx]]) - 1
    ft <- suppressWarnings(fit_logit(stats::update(p$f, paste(". ~ . -", cx, "+ .score")), p$d))
    trend <- coef_table(ft, p$d[[cfg$v$id]], ".score")
    chr[[length(chr) + 1]] <- data.frame(exposure = x, level = levels(p$d[[cx]]),
      n_intervals = as.numeric(table(p$d[[cx]])),
      n_events = as.numeric(tapply(p$d$exit, p$d[[cx]], sum)),
      or = c(1, ct$or), or_lo = c(NA, ct$or_lo), or_hi = c(NA, ct$or_hi),
      standardised_risk = as.numeric(risks), p_trend = trend$p)
  }
}
if (length(chr)) { chr <- bind(chr); write_out(suppress_counts(chr, c("n_intervals", "n_events")), "04c_chronicity.csv") }

# ---- 4. Mediation (H4) -------------------------------------------------------------
message("  mediation")
mediators <- c(fatigue = "med_fatigue", low_energy = "med_low_energy",
               depressed = "med_depressed", anxiety = "med_anxiety",
               concentration = "med_concentration")
mediate_one <- function(d, expo, med, cov_terms) {
  p <- prep(d, "exit", c(expo, med, paste0(expo, ":", med)), cov_terms)
  dd <- p$d
  fm <- stats::reformulate(c(expo, p$cov), response = med)
  est <- function(b) {
    mfit <- suppressWarnings(stats::glm(fm, data = b, family = stats::binomial()))
    yfit <- suppressWarnings(stats::glm(p$f, data = b, family = stats::binomial()))
    pr <- function(fit, a, m = NULL) { z <- b; z[[expo]] <- a; if (!is.null(m)) z[[med]] <- m
      stats::predict(fit, z, type = "response") }
    ey <- function(a, astar) { pm <- pr(mfit, astar); mean(pm * pr(yfit, a, 1) + (1 - pm) * pr(yfit, a, 0)) }
    e11 <- ey(1, 1); e10 <- ey(1, 0); e00 <- ey(0, 0)
    c(total = e11 - e00, nde = e10 - e00, nie = e11 - e10, prop_mediated = (e11 - e10) / (e11 - e00))
  }
  if (nrow(dd) < 50 || sum(dd$exit) < 10 || length(unique(dd[[med]])) < 2)
    return(data.frame(exposure = expo, mediator = med, n_intervals = nrow(dd), n_events = sum(dd$exit), note = "too few"))
  pt <- est(dd); ci <- boot_ci(est, dd, cfg$v$id, cfg$n_boot_med)
  data.frame(exposure = expo, mediator = med, n_intervals = nrow(dd), n_events = sum(dd$exit),
             effect = names(pt), estimate = pt, lo = ci[, 1], hi = ci[, 2],
             dropped_covariates = p$dropped, row.names = NULL)
}
medres <- list()
for (x in pexp) for (m in mediators) medres[[length(medres) + 1]] <- mediate_one(pd, x, m, terms_m2)
export(bind(medres), "04d_mediation.csv")

# ---- 5. PD vs controls (H6) ----------------------------------------------------------
message("  PD vs controls")
both <- el[el$cohort %in% c("PD", "Control"), ]
terms_pool <- setdiff(terms_m2, c("years_pd", "agonist"))
cmp <- list()
for (x in pexp) {
  p <- prep(both, "exit", c(x, "pd", paste0(x, ":pd")), terms_pool)
  if (nrow(p$d) < 50 || length(unique(p$d$pd)) < 2) next
  fit <- suppressWarnings(fit_logit(p$f, p$d))
  V <- vcov_cluster(fit, p$d[[cfg$v$id]]); b <- stats::coef(fit)[rownames(V)]
  int <- paste0(x, ":pd")
  lc <- function(w) { est <- sum(w * b[names(w)]); se <- sqrt(as.numeric(t(w) %*% V[names(w), names(w)] %*% w))
    c(or = exp(est), lo = exp(est - 1.96 * se), hi = exp(est + 1.96 * se)) }
  ctl <- lc(setNames(1, x)); pdo <- lc(setNames(c(1, 1), c(x, int)))
  wt <- wald_test(fit, p$d[[cfg$v$id]], paste0("^", x, ":pd$"))
  cmp[[length(cmp) + 1]] <- data.frame(exposure = x, cohort = c("Control", "PD"),
    n_intervals = c(sum(p$d$pd == 0), sum(p$d$pd == 1)),
    n_events = c(sum(p$d$exit[p$d$pd == 0]), sum(p$d$exit[p$d$pd == 1])),
    or = c(ctl[["or"]], pdo[["or"]]), or_lo = c(ctl[["lo"]], pdo[["lo"]]), or_hi = c(ctl[["hi"]], pdo[["hi"]]),
    p_interaction = wt[["p"]], dropped_covariates = p$dropped)
}
if (length(cmp)) export(bind(cmp), "04e_pd_vs_control.csv")

# ---- 6. Effect modification within PD (exploratory) -----------------------------------
message("  effect modification")
pd$age50 <- as.integer(pd$age >= 50)
pd$usa   <- as.integer(pd$country == "USA")
pd$pss_high <- ifelse(is.na(pd$pss), NA, as.integer(pd$pss >= stats::median(pd$pss, na.rm = TRUE)))
mods <- c(sex = "female", age = "age50", country = "usa", agonist = "agonist",
          job_demands = "job_active", other_ltc = "ltc_any", perceived_stress = "pss_high")
emr <- list()
for (x in pexp) for (mname in names(mods)) {
  mv <- mods[[mname]]
  p <- prep(pd, "exit", c(x, paste0(x, ":", mv)), unique(c(terms_m2, mv)))
  if (nrow(p$d) < 50 || length(unique(p$d[[mv]])) < 2) next
  fit <- suppressWarnings(fit_logit(p$f, p$d))
  wt <- wald_test(fit, p$d[[cfg$v$id]], paste0("^", x, ":", mv, "$"))
  strata <- lapply(0:1, function(k) {
    r <- run_one(sprintf("%s = %d", mv, k), pd[pd[[mv]] %in% k, ], "exit", x,
                 setdiff(terms_m2, mv), boot = FALSE)
    r$modifier <- mname; r$p_interaction <- wt[["p"]]; r })
  emr <- c(emr, strata)
}
if (length(emr)) export(bind(emr), "04f_effect_modification.csv")

# ---- 7. Sensitivity analyses ---------------------------------------------------------
message("  sensitivity analyses")
sens <- list()
base_pd <- iv[iv$cohort %in% "PD" & iv$working0 & iv$ok_gap & !is.na(iv$age) & iv$age >= cfg$age_min, ]
first_only <- pd[!duplicated(pd[[cfg$v$id]]), ]
for (x in pexp) {
  other <- setdiff(pexp, x)
  sens <- c(sens, list(
    run_one("S1: age < 62", base_pd[base_pd$age < 62, ], "exit", x, terms_m2, boot = FALSE),
    run_one("S1: age < 67", base_pd[base_pd$age < 67, ], "exit", x, terms_m2, boot = FALSE),
    run_one("S2: retirement only", pd, "exit_retired", x, terms_m2, boot = FALSE),
    run_one("S2: unemployment only", pd, "exit_unemp", x, terms_m2, boot = FALSE),
    run_one("S3: interval 270-450 days", pd[pd$gap_days >= 270 & pd$gap_days <= 450, ], "exit", x, terms_m2, boot = FALSE),
    run_one("S4: first interval per person", first_only, "exit", x, terms_m2, boot = FALSE),
    run_one("S5: no dopamine agonist", pd[pd$agonist %in% 0, ], "exit", x, setdiff(terms_m2, "agonist"), boot = FALSE),
    run_one("S6: mutually adjusted", pd, "exit", x, terms_m2, boot = FALSE, extra_expo = other)))
}

for (x in pexp) {
  sens <- c(sens, list(
    run_one("S9a: excluding pandemic lay-offs", pd[!(pd$covid_layoff %in% 1), ], "exit", x, terms_m2, boot = FALSE),
    run_one("S9b: calendar-anchored participants, all intervals (reference)", pd[pd$anchored, ], "exit", x,
            terms_m2, boot = FALSE),
    run_one("S9b: calendar-anchored, excluding intervals possibly overlapping the pandemic",
            pd[pd$anchored & pd$pandemic_possible %in% 0, ], "exit", x, terms_m2, boot = FALSE),
    run_one("S10: night/shift work subset, unadjusted (reference)", pd[!is.na(pd$nightwork), ], "exit", x,
            terms_m2, boot = FALSE),
    run_one("S10: night/shift work subset, adjusted for night/shift work", pd[!is.na(pd$nightwork), ], "exit", x,
            c(terms_m2, "nightwork"), boot = FALSE)))
}

# S7: inverse probability of follow-up weights
ipw_base <- iv[iv$cohort %in% "PD" & iv$working0 & iv$ok_age, ]
ipw_base$observed <- as.integer(ipw_base$ok_gap)
for (x in pexp) {
  pw <- prep(ipw_base, "observed", x, setdiff(terms_m2, "gap_yrs"))
  if (nrow(pw$d) < 50) next
  sel <- suppressWarnings(stats::glm(pw$f, data = pw$d, family = stats::binomial()))
  pw$d$ps <- stats::predict(sel, pw$d, type = "response")
  pw$d$sw <- mean(pw$d$observed) / pw$d$ps
  q <- stats::quantile(pw$d$sw, c(0.01, 0.99)); pw$d$sw <- pmin(pmax(pw$d$sw, q[1]), q[2])
  dw <- merge(pd, pw$d[, c("row_id", "sw")], by = "row_id")
  sens[[length(sens) + 1]] <- run_one("S7: inverse probability of follow-up weighted", dw, "exit", x,
                                      terms_m2, boot = FALSE, wvar = "sw")
}

# S8: negative-control exposure (loss of smell/taste)
sens[[length(sens) + 1]] <- run_one("S8: negative control (smell/taste)", pd, "exit", "smell", terms_m2, boot = FALSE)
export(bind(sens), "04g_sensitivity.csv")

# ---- 8. Other long-term conditions (comparison cohort, exploratory) -----------------
message("  other long-term conditions")
ctrl <- el[el$cohort %in% "Control", ]
grp_cols <- paste0("ltc_", names(cfg$ltc_groups))
ctrl$ltc_anycond <- as.integer(rowSums(ctrl[, grp_cols, drop = FALSE] == 1, na.rm = TRUE) > 0)
ctrl$ltc_anycond[rowSums(!is.na(ctrl[, grp_cols, drop = FALSE])) == 0] <- NA
terms_ctrl <- setdiff(terms_m2, c("years_pd", "agonist"))
oc <- list()
for (g in c("ltc_anycond", grp_cols)) for (x in pexp) {
  r <- run_one(paste("Comparison cohort with", sub("^ltc_", "", g)), ctrl[ctrl[[g]] %in% 1, ], "exit", x,
               terms_ctrl, boot = FALSE)
  if (!is.na(r$n_events) && r$n_events < 50) r$note <- "fewer than 50 exit events: not interpreted (plan 7.3)"
  oc[[length(oc) + 1]] <- r
}
export(bind(oc), "04h_other_conditions.csv")

# ---- 9. Validation and sources of sleepiness (plan 7.6) ---------------------------------
message("  validation")
cnt <- function(x) length(unique(x))
ex <- pd[pd$exit %in% 1 & pd$fi_resp %in% 1, ]
val <- data.frame(
  measure = c("Participants with a work exit who completed the 2025 Financial and Social Impact survey",
              "  of whom: stopped working / retired early for reasons related to PD",
              "  of whom: worked fewer hours for reasons related to PD",
              "PD participants with Medical History data and daytime sleepiness at a first eligible interval",
              "  of whom: report sleep apnoea", "  of whom: report insomnia"),
  n = NA)
val$n[1] <- cnt(ex[[cfg$v$id]])
val$n[2] <- cnt(ex[[cfg$v$id]][ex$fi_pd_stop %in% 1]); val$n[3] <- cnt(ex[[cfg$v$id]][ex$fi_pd_hours %in% 1])
f1 <- pd[!duplicated(pd[[cfg$v$id]]) & pd$sleepiness %in% 1 & !is.na(pd$apnoea), ]
val$n[4] <- nrow(f1); val$n[5] <- sum(f1$apnoea == 1); val$n[6] <- sum(f1$insom_dx %in% 1)
val$pct <- c(NA, fmt(100 * val$n[2:3] / max(val$n[1], 1), 1), NA, fmt(100 * val$n[5:6] / max(val$n[4], 1), 1))
val$pct[!is.na(val$n) & val$n > 0 & val$n < cfg$min_cell] <- NA
write_out(suppress_counts(val, "n"), "04i_validation.csv")
vfit <- lapply(pexp, function(x) run_one("Validation: Financial and Social Impact respondents only",
                                         pd[pd$fi_resp %in% 1, ], "exit", x, terms_m2, boot = FALSE))

pd$sleepy_src <- factor(ifelse(is.na(pd$sleepiness) | is.na(pd$apnoea), NA,
                   ifelse(pd$sleepiness == 0, "No sleepiness",
                   ifelse(pd$apnoea == 1, "Sleepiness with apnoea diagnosis", "Sleepiness without apnoea diagnosis"))),
                   levels = c("No sleepiness", "Sleepiness without apnoea diagnosis", "Sleepiness with apnoea diagnosis"))
ps <- prep(pd, "exit", "sleepy_src", terms_m2)
if (nrow(ps$d) >= 50 && nlevels(ps$d$sleepy_src) == 3 && sum(ps$d$exit) >= 10) {
  fs <- suppressWarnings(fit_logit(ps$f, ps$d))
  ct <- coef_table(fs, ps$d[[cfg$v$id]], paste0("sleepy_src", levels(ps$d$sleepy_src)[-1]))
  br <- data.frame(group = levels(ps$d$sleepy_src), n_intervals = as.numeric(table(ps$d$sleepy_src)),
                   n_events = as.numeric(tapply(ps$d$exit, ps$d$sleepy_src, sum)),
                   or = c(1, ct$or), or_lo = c(NA, ct$or_lo), or_hi = c(NA, ct$or_hi))
  write_out(suppress_counts(br, c("n_intervals", "n_events")), "04j_sleepiness_by_apnoea.csv")
}
export(bind(vfit), "04k_validation_subset_model.csv")

message("04: done")
