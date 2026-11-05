# =============================================================================
# 03_descriptives.R – sample description, prevalence, agreement between measures
# =============================================================================
message("03: descriptives")

pct <- function(x) round(100 * mean(x, na.rm = TRUE), 1)
desc_group <- function(d, g) {
  lv <- levels(d[[g]])
  do.call(rbind, lapply(lv, function(l) {
    z <- d[d[[g]] %in% l, ]
    data.frame(group = l, n_intervals = nrow(z), n_people = length(unique(z[[idv]])),
               mean_age = round(mean(z$age, na.rm = TRUE), 1), pct_female = pct(z$female),
               pct_selfemp = pct(z$selfemp), pct_sedentary = pct(z$job_sedentary),
               pct_any_ltc = pct(z$ltc_any), pct_restless = pct(z$restless),
               mean_cesd7 = round(mean(z$cesd7, na.rm = TRUE), 2), n_exits = sum(z$exit, na.rm = TRUE),
               pct_exit = pct(z$exit))
  }))
}
iv$all <- factor("All")
t1 <- rbind(cbind(by = "All", desc_group(iv, "all")), cbind(by = "Insomnia frequency", desc_group(iv, "insom_freq")))
write_out(suppress_counts(t1, c("n_intervals", "n_people", "n_exits")), "03_table1.csv")

# Prevalence of each sleep item, overall and by long-term condition group (first interval per person)
first <- iv[iv$first_int == 1, ]
prev_items <- c("restless", "night_freq", "tired_freq", "short_sleep", "poor_quality")
prev <- data.frame(subgroup = "All", n = nrow(first), t(sapply(prev_items, function(v) pct(first[[v]]))))
for (g in grep("^grp_", names(first), value = TRUE)) {
  z <- first[first[[g]] %in% 1, ]
  prev <- rbind(prev, data.frame(subgroup = sub("^grp_", "", g), n = nrow(z),
                                 t(sapply(prev_items, function(v) pct(z[[v]])))))
}
prev[prev$n < cfg$min_cell, prev_items] <- NA
write_out(suppress_counts(prev, "n"), "03_prevalence_by_condition.csv")

# Agreement: yes/no restless sleep vs insomnia frequency (all intervals)
ag <- as.data.frame.matrix(table(insomnia_frequency = iv$insom_freq, restless_sleep = factor(iv$restless, 0:1, c("No", "Yes"))))
ag$insomnia_frequency <- rownames(ag)
ag$pct_restless_yes <- round(100 * ag$Yes / (ag$Yes + ag$No), 1)
ag <- ag[, c("insomnia_frequency", "No", "Yes", "pct_restless_yes")]
write_out(suppress_counts(ag, c("No", "Yes")), "03_agreement_restless_vs_frequency.csv")

# Reasons for leaving, by insomnia frequency (exits only)
ex <- iv[iv$exit %in% 1, ]
rs <- as.data.frame.matrix(table(ex$insom_freq, ex$exit3)[, c("Health-related exit", "Other exit"), drop = FALSE])
rs$insomnia_frequency <- rownames(rs)
write_out(suppress_counts(rs, c("Health-related exit", "Other exit")), "03_exit_type_by_frequency.csv")

# Neurological conditions including Parkinson's (descriptive only)
nr <- iv[iv$grp_neurological %in% 1, ]
nd <- data.frame(group = "Neurological (incl. Parkinson's)", n_intervals = nrow(nr),
                 pct_night_freq = pct(nr$night_freq), pct_restless = pct(nr$restless),
                 n_exits = sum(nr$exit, na.rm = TRUE), pct_exit = pct(nr$exit))
if (nrow(nr) < cfg$min_cell) nd[, c("pct_night_freq", "pct_restless", "pct_exit")] <- NA
write_out(suppress_counts(nd, c("n_intervals", "n_exits")), "03_neurological_descriptive.csv")

# Loss to follow-up by sleep exposure
lf <- iv_ipw
lf_tab <- data.frame(insomnia_frequency = levels(lf$insom_freq),
                     n = as.numeric(table(lf$insom_freq)),
                     pct_lost = as.numeric(tapply(!lf$interviewed1, lf$insom_freq, function(z) round(100 * mean(z), 1))))
write_out(suppress_counts(lf_tab, "n"), "03_loss_to_followup.csv")
