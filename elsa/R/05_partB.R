# =============================================================================
# 05_partB.R – Part B: wave 10 accelerometer sleep -> work outcomes at wave 11
# One record per person: ordinary robust (HC0) standard errors.
# =============================================================================
message("05: Part B (objective sleep)")
if (is.null(pb) || !nrow(pb)) {
  message("  no Part B data (accelerometry or wave 11 file missing)")
} else {
  covB <- c("ns(age, df = 3)", "female", "qual3", "wealth5", "selfemp", "jobact", "ltc_n4", "season", "int_year_f")
  covB <- covB[vapply(covB, function(t) { v <- all.vars(str2lang(t)); all(v %in% names(pb)) &&
    all(vapply(v, function(z) length(unique(stats::na.omit(pb[[z]]))) > 1, TRUE)) }, TRUE)]
  n_exits <- sum(pb$exit, na.rm = TRUE)
  # descriptive risks by exposure group (always reported)
  pb$waso_tert <- cut(pb$waso_30 * 30, stats::quantile(pb$waso_30 * 30, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                      include.lowest = TRUE, labels = c("Low", "Middle", "High"))
  pb$sol_tert <- cut(pb$sol_15 * 15, stats::quantile(pb$sol_15 * 15, c(0, 1/3, 2/3, 1), na.rm = TRUE),
                     include.lowest = TRUE, labels = c("Low", "Middle", "High"))
  dsc <- do.call(rbind, lapply(c("tst_cat", "waso_tert", "sol_tert"), function(v) {
    z <- pb[!is.na(pb[[v]]) & !is.na(pb$exit), ]
    data.frame(exposure = v, level = levels(z[[v]]), n_people = as.numeric(table(z[[v]])),
               n_exits = as.numeric(tapply(z$exit, z[[v]], sum)),
               pct_exit = round(100 * as.numeric(tapply(z$exit, z[[v]], mean)), 1))
  }))
  dsc$pct_exit[!is.na(dsc$n_exits) & dsc$n_exits > 0 & dsc$n_exits < cfg$min_cell] <- NA
  write_out(suppress_counts(dsc, c("n_people", "n_exits")), "05_partB_descriptive.csv")

  # self-report vs objective
  sr <- do.call(rbind, lapply(c("waso_30", "tst_h", "sol_15"), function(v) {
    z <- pb[!is.na(pb[[v]]) & !is.na(pb$restless), ]
    data.frame(measure = v, n = nrow(z), mean_restless_no = mean(z[[v]][z$restless == 0]),
               mean_restless_yes = mean(z[[v]][z$restless == 1]))
  }))
  write_out(suppress_counts(sr, "n"), "05_partB_selfreport_vs_objective.csv")

  # excluded for < 3 valid days vs included
  if (exists("pb_excl") && nrow(pb_excl)) {
    cmp <- data.frame(group = c("Included (>= 3 valid days)", "Excluded (< 3 valid days)"),
                      n = c(nrow(pb), nrow(pb_excl)),
                      mean_age = round(c(mean(pb$age, na.rm = TRUE), mean(pb_excl$age, na.rm = TRUE)), 1),
                      pct_female = round(100 * c(mean(pb$female, na.rm = TRUE), mean(pb_excl$female, na.rm = TRUE)), 1),
                      pct_any_ltc = round(100 * c(mean(pb$ltc_any, na.rm = TRUE), mean(pb_excl$ltc_any, na.rm = TRUE)), 1))
    write_out(suppress_counts(cmp, "n"), "05_partB_excluded_comparison.csv")
  }

  if (n_exits < cfg$min_exits_subgroup) {
    message(sprintf("  Part B: %d exits (< %d): descriptive only, as registered", n_exits, cfg$min_exits_subgroup))
  } else {
    fitB <- function(expo, outcome = "exit") {
      f <- stats::reformulate(c(expo, covB), response = outcome)
      d <- complete_for(pb, f, idv)
      fit <- suppressWarnings(stats::glm(f, data = d, family = stats::binomial()))
      V <- vcov_cluster(fit, d[[idv]])        # one row per person = HC0 robust
      terms <- grep(paste0("^(", paste(expo, collapse = "|"), ")"), names(stats::coef(fit)), value = TRUE)
      ct <- coef_table(fit, d[[idv]], terms, V)
      data.frame(model = paste(expo, collapse = " + "), outcome = outcome, term = terms, n_people = nrow(d),
                 n_events = sum(d[[outcome]]), or = ct$or, or_lo = ct$or_lo, or_hi = ct$or_hi, p = ct$p)
    }
    outB <- bind(list(fitB("waso_30"), fitB("tst_cat"), fitB("sol_15"), fitB(c("waso_30", "tst_cat", "sol_15")),
                      if (sum(pb$newlimit, na.rm = TRUE) >= cfg$min_cell) fitB("waso_30", "newlimit")))
    write_out(suppress_counts(outB, c("n_people", "n_events")), "05_partB_models.csv")
  }
}
