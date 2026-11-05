# =============================================================================
# 04_models.R – Part A pre-registered models (H1-H7), negative control, sensitivity
# =============================================================================
message("04: Part A models (bootstrap B = ", cfg$n_boot, ")")
set.seed(cfg$seed)

C1 <- c("ns(age, df = 3)", "female", "wave_f")
C2 <- c(C1, "qual3", "wealth5", "selfemp", "jobact", "ltc_n4")
C3 <- c(C2, "cesd7", "srh")
lv4 <- levels(iv$insom_freq)
vars_of <- function(t) all.vars(str2lang(t))

# Merge exposure levels with fewer than 10 exits into the adjacent level (registered rule)
merge_sparse <- function(d, fx, outcome = "exit") {
  f <- d[[fx]]; if (!is.factor(f)) return(list(d = d, note = ""))
  ev <- tapply(d[[outcome]], f, sum, na.rm = TRUE); ev[is.na(ev)] <- 0
  if (all(ev >= 10)) return(list(d = d, note = ""))
  lv <- levels(f); note <- character(0)
  for (k in rev(seq_along(lv))) if (ev[[lv[k]]] < 10 && length(levels(f)) > 2) {
    to <- if (k == length(lv)) lv[k - 1] else lv[k + 1]
    levels(f)[levels(f) == lv[k]] <- to; note <- c(note, paste(lv[k], "merged into", to))
  }
  d[[fx]] <- droplevels(f); list(d = d, note = paste(note, collapse = "; "))
}

prep <- function(d, outcome, expo_terms, cov, extra = character(0)) {
  usable <- function(t) { v <- vars_of(t); all(v %in% names(d)) && all(vapply(v, function(z) any(!is.na(d[[z]])), TRUE)) }
  cov <- cov[vapply(cov, usable, TRUE)]
  f <- stats::reformulate(c(expo_terms, cov), response = outcome)
  dd <- droplevels(complete_for(d, f, c(idv, extra)))
  const <- vapply(cov, function(t) any(vapply(vars_of(t), function(z) length(unique(dd[[z]])) < 2, TRUE)), TRUE)
  dropped <- cov[const]; cov <- cov[!const]
  list(f = stats::reformulate(c(expo_terms, cov), response = outcome), d = dd, cov = cov,
       dropped = paste(dropped, collapse = "; "))
}

# Fit with the registered convergence rule (fall back to Model 1 covariates)
fit_rule <- function(d, outcome, expo_terms, cov, w = NULL, extra = character(0)) {
  p <- prep(d, outcome, expo_terms, cov, extra)
  ww <- if (is.null(w)) NULL else p$d[[w]]
  fit <- tryCatch(suppressWarnings(fit_logit(p$f, p$d, ww)), error = function(e) NULL)
  note <- ""
  if (is.null(fit) || !fit$converged || any(is.na(stats::coef(fit)[grep(paste(vars_of(expo_terms[1]), collapse = "|"), names(stats::coef(fit)))]))) {
    p <- prep(d, outcome, expo_terms, C1, extra); ww <- if (is.null(w)) NULL else p$d[[w]]
    fit <- suppressWarnings(fit_logit(p$f, p$d, ww)); note <- "did not converge: Model 1 covariates used"
  }
  list(fit = fit, d = p$d, dropped = p$dropped, note = note, f = p$f)
}

or_rows <- function(m, terms, label, outcome, expo) {
  V <- vcov_cluster(m$fit, m$d[[idv]])
  ct <- coef_table(m$fit, m$d[[idv]], terms, V)
  data.frame(analysis = label, outcome = outcome, exposure = expo, term = terms,
             n_intervals = nrow(m$d), n_people = length(unique(m$d[[idv]])),
             n_events = sum(m$d[[outcome]]), or = ct$or, or_lo = ct$or_lo, or_hi = ct$or_hi, p = ct$p,
             dropped_covariates = m$dropped, note = m$note, row.names = NULL)
}

trend_row <- function(d, outcome, score, cov, label, w = NULL) {
  m <- fit_rule(d, outcome, score, cov, w)
  r <- or_rows(m, score, label, outcome, score); r$term <- paste0(score, " (trend, per level)"); r
}

events_by <- function(d, fx, outcome) {
  data.frame(term = paste0(fx, levels(d[[fx]])), level_n = as.numeric(table(d[[fx]])),
             level_events = as.numeric(tapply(d[[outcome]], d[[fx]], sum)))
}

cat_model <- function(d, fx, outcome, cov, label, score = NULL, w = NULL) {
  ms <- merge_sparse(d, fx, outcome); d <- ms$d
  m <- fit_rule(d, outcome, fx, cov, w)
  lv <- levels(m$d[[fx]])
  r <- or_rows(m, paste0(fx, lv[-1]), label, outcome, fx)
  ref <- r[1, ]; ref$term <- paste0(fx, lv[1]); ref[, c("or", "or_lo", "or_hi", "p")] <- c(1, NA, NA, NA)
  r <- rbind(ref, r)
  r <- merge(r, events_by(m$d, fx, outcome), by = "term", all.x = TRUE, sort = FALSE)
  r$risk <- gcomp_levels(m$fit, m$d, fx, lv, if (is.null(w)) NULL else m$d[[w]])[match(sub(fx, "", r$term), lv)]
  if (!is.null(score)) {
    tr <- trend_row(d, outcome, score, cov, label, w)
    r$p_trend <- tr$p; r$or_trend <- tr$or; r$or_trend_lo <- tr$or_lo; r$or_trend_hi <- tr$or_hi
  }
  r$sparse_note <- ms$note
  r
}

# ---- Primary analyses (Model 2): H1, H2 with bootstrap risks, E-values, Holm --------------
h1 <- fit_rule(iv, "exit", "restless", C2)
h1_tab <- or_rows(h1, "restless", "H1: restless sleep (yes vs no)", "exit", "restless")
h1_tab$risk_no <- gcomp_levels(h1$fit, h1$d, "restless", 0); h1_tab$risk_yes <- gcomp_levels(h1$fit, h1$d, "restless", 1)
h2_tab <- cat_model(iv, "insom_freq", "exit", C2, "H2: night-time insomnia frequency", score = "insom_score")
h2 <- fit_rule(iv, "exit", "insom_freq", C2)

est_primary <- function(b) {
  f1 <- suppressWarnings(fit_logit(h1$f, b)); f2 <- suppressWarnings(fit_logit(h2$f, b))
  r1 <- gcomp_levels(f1, b, "restless", c(0, 1))
  r2 <- gcomp_levels(f2, b, "insom_freq", lv4[c(1, 4)])
  or1 <- exp(stats::coef(f1)[["restless"]]); or4 <- exp(stats::coef(f2)[[paste0("insom_freq", lv4[4])]])
  c(h1_rd = r1[[2]] - r1[[1]], h1_rr = r1[[2]] / r1[[1]], h2_rd = r2[[2]] - r2[[1]], h2_rr = r2[[2]] / r2[[1]],
    ratio_or4_or1 = or4 / or1)
}
bd <- merge(h1$d[, c(idv, "wave")], h2$d, by = c(idv, "wave"))           # common sample for the ratio
pt <- est_primary(bd); ci <- boot_ci(est_primary, bd, idv, cfg$n_boot)
eff <- data.frame(quantity = names(pt), estimate = pt, ci_lo = ci[, 1], ci_hi = ci[, 2], row.names = NULL)
eff$evalue_point <- NA; eff$evalue_point[eff$quantity == "h1_rr"] <- evalue(pt[["h1_rr"]])
eff$evalue_point[eff$quantity == "h2_rr"] <- evalue(pt[["h2_rr"]])
eff$bootstrap_resamples <- cfg$n_boot; eff$n_intervals_common <- nrow(bd)

p_h1 <- h1_tab$p; p_h2 <- h2_tab$p_trend[1]
holm <- stats::p.adjust(c(p_h1, p_h2), "holm")
h1_tab$p_holm <- holm[1]; h2_tab$p_holm_trend <- holm[2]
primary <- bind(list(h1_tab, h2_tab))
write_out(suppress_counts(primary, c("n_intervals", "n_people", "n_events", "level_n", "level_events")), "04a_primary.csv")
write_out(eff, "04a_primary_effects_bootstrap.csv")

# Decision rule (registered inference criteria)
or4 <- h2_tab[h2_tab$term == paste0("insom_freq", lv4[4]), ]
crit_a <- isTRUE(holm[2] < 0.05) && nrow(or4) == 1 && isTRUE(or4$or > 1) && isTRUE(or4$or_lo > 1)
ratio_lo <- eff$ci_lo[eff$quantity == "ratio_or4_or1"]; ratio <- pt[["ratio_or4_or1"]]
crit_b <- isTRUE(ratio >= cfg$ratio_threshold)
no_trend <- isTRUE(h2_tab$or_trend_lo[1] <= 1 & h2_tab$or_trend_hi[1] >= 1) && nrow(or4) == 1 && isTRUE(or4$or_lo <= 1)
verdict <- if (crit_a && crit_b) "Supported" else if (no_trend) "Not supported" else if (crit_a) "Inconclusive (both measures predict exit)" else "Not supported / unclear: see estimates"
write_out(data.frame(criterion = c("(a) H2 graded, Holm trend p < 0.05, OR(3+/week) CI > 1",
                                   "(b) OR(3+/week) / OR(restless) >= threshold", "Verdict"),
                     value = c(crit_a, sprintf("%s (ratio %.2f, 95%% CI %.2f to %.2f; threshold %.1f)", crit_b, ratio,
                                               ratio_lo, eff$ci_hi[eff$quantity == "ratio_or4_or1"], cfg$ratio_threshold), verdict)),
          "04g_decision.csv")

# ---- Registered checks: linear trend (LRT factor vs score) and collinearity (VIF) -----------
h2s <- suppressWarnings(stats::glm(stats::update(h2$f, . ~ . - insom_freq + insom_score), data = h2$d, family = stats::binomial()))
lrt <- stats::anova(h2s, h2$fit, test = "LRT")
Xm <- stats::model.matrix(h2$fit)[, -1, drop = FALSE]
Xm <- Xm[, apply(Xm, 2, stats::sd) > 0, drop = FALSE]
vif <- tryCatch(diag(solve(stats::cor(Xm))), error = function(e) rep(NA, ncol(Xm)))
write_out(data.frame(check = c("LRT: 4-level factor vs linear score (H2), p", paste("VIF", colnames(Xm))),
                     value = c(lrt[2, ncol(lrt)], round(vif, 2))), "04i_model_checks.csv")

# ---- Secondary analyses ----------------------------------------------------------------
sec <- list()
h3 <- fit_rule(iv, "exit", c("insom_freq", "restless"), C2)
sec[[1]] <- or_rows(h3, c(paste0("insom_freq", lv4[-1]), "restless"), "H3: frequency and restless sleep together", "exit", "insom_freq + restless")
sec[[2]] <- cat_model(iv, "tired", "exit", C2, "H4a: waking tired and worn out", score = "tired_n")
sec[[3]] <- cat_model(iv[iv$wave %in% c(6, 8), ], "doze", "exit", C2, "H4b: daytime dozing (waves 6 and 8)", score = "doze_n")
g4 <- fit_rule(iv, "exit", "grp4", C2)
V4 <- vcov_cluster(g4$fit, g4$d[[idv]])
sec[[4]] <- or_rows(g4, paste0("grp4", levels(iv$grp4)[-1]), "H4c: night-time x waking tired groups (vs neither)", "exit", "grp4")
cn <- function(a, b) { x <- lincom(g4$fit, V4, a, b); data.frame(analysis = "H4c contrast", outcome = "exit", exposure = "grp4",
  term = paste(a, "vs", b), or = x[["or"]], or_lo = x[["or_lo"]], or_hi = x[["or_hi"]], p = x[["p"]]) }
if (all(c("grp4Both", "grp4Night-time only") %in% names(stats::coef(g4$fit))))
  sec[[5]] <- cn("grp4Both", "grp4Night-time only")
sec[[6]] <- cat_model(iv, "insom_freq", "newlimit", C2, "H6: newly reporting health limits work", score = "insom_score")
write_out(suppress_counts(bind(sec), c("n_intervals", "n_people", "n_events", "level_n", "level_events")), "04b_secondary.csv")

# ---- H5: multinomial (stayed / health-related exit / other exit) ------------------------
if (requireNamespace("nnet", quietly = TRUE)) {
  h5_one <- function(expo_terms) {
    p <- prep(iv, "exit3", expo_terms, C2)
    p$d <- droplevels(p$d)
    mc <- multinom_cluster(p$f, p$d, p$d[[idv]])
    se <- sqrt(diag(mc$V))
    data.frame(term = names(mc$b), log_or = mc$b, se = se, or = exp(mc$b), or_lo = exp(mc$b - 1.96 * se),
               or_hi = exp(mc$b + 1.96 * se), p = 2 * stats::pnorm(-abs(mc$b / se)), n_intervals = nrow(p$d),
               row.names = NULL)[grepl("insom", names(mc$b)), ]
  }
  h5 <- h5_one("insom_freq")
  p5 <- prep(iv, "exit3", "insom_score", C2); mc <- multinom_cluster(p5$f, droplevels(p5$d), p5$d[[idv]])
  a <- grep("^Health-related exit:insom_score$", names(mc$b)); b <- grep("^Other exit:insom_score$", names(mc$b))
  dz <- (mc$b[a] - mc$b[b]) / sqrt(mc$V[a, a] + mc$V[b, b] - 2 * mc$V[a, b])
  tr <- data.frame(term = names(mc$b)[c(a, b)], log_or = mc$b[c(a, b)], or = exp(mc$b[c(a, b)]),
                   p_difference_health_vs_other = 2 * stats::pnorm(-abs(dz)), n_intervals = nrow(p5$d))
  ev <- table(p5$d$exit3)
  h5_out <- bind(list(cbind(analysis = "H5: by level", h5), cbind(analysis = "H5: trend", tr)))
  h5_out$n_health_exits <- ev[["Health-related exit"]]; h5_out$n_other_exits <- ev[["Other exit"]]
  write_out(suppress_counts(h5_out, c("n_intervals", "n_health_exits", "n_other_exits")), "04c_h5_multinomial.csv")
} else message("  nnet not installed: H5 skipped (install.packages('nnet'))")

# ---- H7: long-term conditions -------------------------------------------------------------
C2_noltc <- setdiff(C2, "ltc_n4")
h7 <- list()
mi7 <- fit_rule(iv, "exit", c("insom_score", "ltc_any", "insom_score:ltc_any"), C2_noltc)
V7 <- vcov_cluster(mi7$fit, mi7$d[[idv]])
h7[[1]] <- data.frame(analysis = "H7: interaction insomnia trend x any LTC", term = "insom_score:ltc_any",
                      or = exp(stats::coef(mi7$fit)[["insom_score:ltc_any"]]),
                      p = wald_test(mi7$fit, V7, "^insom_score:ltc_any$")[["p"]], n_intervals = nrow(mi7$d))
for (k in 0:1) h7[[length(h7) + 1]] <- cbind(trend_row(iv[iv$ltc_any %in% k, ], "exit", "insom_score", C2_noltc,
                                                       sprintf("H7: trend, any long-term condition = %d", k)))
for (g in grep("^grp_", names(iv), value = TRUE)) {
  z <- iv[iv[[g]] %in% 1, ]
  if (sum(z$exit, na.rm = TRUE) >= cfg$min_exits_subgroup)
    h7[[length(h7) + 1]] <- trend_row(z, "exit", "insom_score", C2, paste("H7: trend within", sub("grp_", "", g)))
  else h7[[length(h7) + 1]] <- data.frame(analysis = paste("H7: trend within", sub("grp_", "", g)),
                                          n_intervals = nrow(z), n_events = sum(z$exit, na.rm = TRUE),
                                          note = "fewer than 50 exits: descriptive only")
}
write_out(suppress_counts(bind(h7), c("n_intervals", "n_people", "n_events")), "04d_h7_conditions.csv")

# ---- Negative control ---------------------------------------------------------------------
nc <- fit_rule(iv, "exit", "hearing_poor", C2)
write_out(suppress_counts(or_rows(nc, "hearing_poor", "Negative control: poor hearing", "exit", "hearing_poor"),
                          c("n_intervals", "n_people", "n_events")), "04e_negative_control.csv")

# ---- Sensitivity analyses (H1 and H2) ----------------------------------------------------------
sens <- list()
s_pair <- function(d, label, cov = C2, outcome = "exit", w = NULL) list(
  or_rows(fit_rule(d, outcome, "restless", cov, w), "restless", paste(label, "- H1"), outcome, "restless"),
  cat_model(d, "insom_freq", outcome, cov, paste(label, "- H2"), score = "insom_score", w = w))
sens <- c(sens, s_pair(iv, "S1 Model 1", C1), s_pair(iv, "S1 Model 3", C3))
sens <- c(sens, s_pair(iv[iv$first_int == 1, ], "S2 first interval per person"))
s2i <- fit_rule(iv, "exit", c("insom_score", "first_int", "insom_score:first_int"), C2)
V2i <- vcov_cluster(s2i$fit, s2i$d[[idv]])
sens[[length(sens) + 1]] <- data.frame(analysis = "S2 interaction insomnia trend x first interval",
  term = "insom_score:first_int", or = exp(stats::coef(s2i$fit)[["insom_score:first_int"]]),
  p = wald_test(s2i$fit, V2i, "^insom_score:first_int$")[["p"]])
sens <- c(sens, s_pair(iv[iv$helwk %in% 0, ], "S3 no health limit on work at baseline"))
iv$xwgt_n <- iv$xwgt
sens <- c(sens, s_pair(iv[!is.na(iv$xwgt_n) & iv$xwgt_n > 0, ], "S4 cross-sectional weights", w = "xwgt_n"))
sens <- c(sens, s_pair(iv[!(iv$exit_nonretire %in% 1), ], "S5 retirement exits", outcome = "exit_retire"))
sens <- c(sens, s_pair(iv[!(iv$exit_retire %in% 1), ], "S5 non-retirement exits", outcome = "exit_nonretire"))
sens <- c(sens, s_pair(iv[iv$age < 60, ], "S6 aged 50-59"))
sens <- c(sens, s_pair(iv[iv$selfemp %in% 0, ], "S7 employees only"))
for (v in c("poor_quality", "short_sleep"))
  sens[[length(sens) + 1]] <- or_rows(fit_rule(iv, "exit", v, C2), v, paste("S8", v, "in place of H2"), "exit", v)
sens <- c(sens, s_pair(iv[!is.na(iv$ipw), ], "Loss to follow-up IPW", w = "ipw"))
write_out(suppress_counts(bind(sens), c("n_intervals", "n_people", "n_events", "level_n", "level_events")), "04f_sensitivity.csv")

# ---- Multiple imputation (if > 5% of intervals miss a Model 2 covariate) -----------------------
c2vars <- unique(unlist(lapply(C2, vars_of)))
miss_prop <- mean(!stats::complete.cases(iv[, c2vars]))
message(sprintf("  intervals missing a Model 2 covariate: %.1f%%", 100 * miss_prop))
if (miss_prop > cfg$mi_threshold) {
  if (requireNamespace("mice", quietly = TRUE)) {
    imp_vars <- c(idv, "exit", "restless", "insom_freq", "insom_score", c2vars, "srh", "helwk")
    dm <- iv[!is.na(iv$exit), imp_vars]
    dm[[idv]] <- num(dm[[idv]])
    meth <- mice::make.method(dm); meth[c(idv, "exit", "restless", "insom_freq", "insom_score")] <- ""
    pred <- mice::make.predictorMatrix(dm); pred[, idv] <- 0
    imp <- mice::mice(dm, m = cfg$n_imp, method = meth, predictorMatrix = pred, printFlag = FALSE, seed = cfg$seed)
    pool_one <- function(terms_expo, keep) {
      fits <- lapply(seq_len(cfg$n_imp), function(i) {
        di <- mice::complete(imp, i); f <- stats::reformulate(c(terms_expo, C2), response = "exit")
        fi <- suppressWarnings(stats::glm(f, data = di, family = stats::binomial()))
        V <- vcov_cluster(fi, di[[idv]]); list(b = stats::coef(fi)[keep], v = diag(V)[keep])
      })
      Q <- sapply(fits, `[[`, "b"); U <- sapply(fits, `[[`, "v"); Q <- matrix(Q, nrow = length(keep)); U <- matrix(U, nrow = length(keep))
      qb <- rowMeans(Q); ub <- rowMeans(U); bv <- apply(Q, 1, stats::var); tv <- ub + (1 + 1 / cfg$n_imp) * bv
      data.frame(term = keep, or = exp(qb), or_lo = exp(qb - 1.96 * sqrt(tv)), or_hi = exp(qb + 1.96 * sqrt(tv)))
    }
    mi_out <- bind(list(cbind(analysis = "MI - H1", pool_one("restless", "restless")),
                        cbind(analysis = "MI - H2", pool_one("insom_freq", paste0("insom_freq", lv4[-1])))))
    mi_out$n_imputations <- cfg$n_imp; mi_out$prop_missing_covariate <- round(miss_prop, 3)
    write_out(mi_out, "04h_multiple_imputation.csv")
  } else message("  mice not installed: multiple imputation skipped (install.packages('mice'))")
}
