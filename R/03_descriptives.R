# =============================================================================
# 03_descriptives.R – Table 1, transitions, exposure prevalence (aggregate only)
# =============================================================================

message("03: descriptives")
el <- iv[iv$eligible, ]
first <- el[!duplicated(el[[idv]]), ]   # first eligible interval per participant (iv is time-ordered within id)
first <- first[order(first$row_id), ]

describe <- function(d, label) {
  cont <- function(x) sprintf("%s (%s)", fmt(mean(x, na.rm = TRUE), 1), fmt(stats::sd(x, na.rm = TRUE), 1))
  np   <- function(n, N) {
    if (n > 0 && n < cfg$min_cell) paste0("<", cfg$min_cell)
    else sprintf("%d (%s%%)", as.integer(n), fmt(100 * n / max(N, 1), 1))
  }
  pct  <- function(x) { x <- x[!is.na(x)]; np(sum(x == 1), length(x)) }
  lev  <- function(f) sapply(levels(f), function(l) np(sum(f == l, na.rm = TRUE), sum(!is.na(f))))
  rows <- c(
    "Participants, n" = as.character(nrow(d)),
    "Age, mean (SD)" = cont(d$age),
    "Female, n (%)" = pct(d$female),
    "Employed full-time, n (%)" = pct(as.integer(d$emp0 == 1)),
    setNames(lev(d$educ3), paste("Education:", levels(d$educ3))),
    setNames(lev(d$income4), paste("Income:", levels(d$income4))),
    setNames(lev(d$country), paste("Country:", levels(d$country))),
    "Daytime sleepiness, n (%)" = pct(d$sleepiness),
    "Insomnia symptoms, n (%)" = pct(d$insomnia),
    "Restless legs symptoms, n (%)" = pct(d$rls),
    "Nocturia, n (%)" = pct(d$nocturia),
    "Dream enactment, n (%)" = pct(d$rbd),
    "Anxiety, n (%)" = pct(d$anxiety),
    "Depressive symptoms (GDS-14 >= 5), n (%)" = pct(d$depressed),
    "MDS-UPDRS II, mean (SD)" = cont(d$updrs2),
    "PDAQ-15, mean (SD)" = cont(d$pdaq),
    "Dopamine agonist, n (%)" = pct(d$agonist),
    setNames(lev(d$jobdemand), paste("Job physical demands:", levels(d$jobdemand))),
    setNames(lev(d$ltc_n3), paste("Physical long-term conditions limiting activities:", levels(d$ltc_n3))),
    "Problems with usual activities (EQ-5D moderate or worse), n (%)" = pct(as.integer(d$x_active >= 2)),
    "Perceived stress (PSS-10), mean (SD)" = cont(d$pss))
  if (any(d$cohort == "PD")) rows <- c(rows, setNames(lev(d$years_pd), paste("Years with PD at enrolment:", levels(d$years_pd))))
  out <- data.frame(characteristic = names(rows), value = unname(rows))
  names(out)[2] <- label
  out
}
t1 <- describe(first[first$cohort == "PD", ], "PD")
t_c <- describe(first[first$cohort == "Control", ], "Control")
t1$Control <- t_c$Control[match(t1$characteristic, t_c$characteristic)]
write_out(t1, "03_table1_first_interval.csv")

# Employment transitions (all intervals within gap/age/cohort rules)
tr <- iv[iv$ok_gap & iv$ok_age & iv$ok_cohort, ]
lab <- c("1" = "Full-time", "2" = "Part-time", "3" = "Retired", "4" = "Unemployed")
tm <- as.data.frame(table(cohort = tr$cohort, from = lab[as.character(tr$emp0)],
                          to = lab[as.character(tr$emp1)]))
write_out(suppress_counts(tm, "Freq"), "03_transitions.csv")

# Exit rates by exposure (crude), PD and controls
crude <- do.call(rbind, lapply(c("sleepiness", "insomnia", "rls", "nocturia", "rbd", "smell"), function(x) {
  do.call(rbind, lapply(c("PD", "Control"), function(co) {
    d <- el[el$cohort == co & !is.na(el[[x]]) & !is.na(el$exit), ]
    if (!nrow(d)) return(NULL)
    data.frame(exposure = x, cohort = co, level = c(0, 1),
               intervals = c(sum(d[[x]] == 0), sum(d[[x]] == 1)),
               exits = c(sum(d$exit[d[[x]] == 0]), sum(d$exit[d[[x]] == 1])),
               exit_pct = c(fmt(100 * mean(d$exit[d[[x]] == 0]), 1), fmt(100 * mean(d$exit[d[[x]] == 1]), 1)))
  }))
}))
crude$exit_pct[crude$exits > 0 & crude$exits < cfg$min_cell] <- NA   # avoid back-calculation
write_out(suppress_counts(crude, c("intervals", "exits")), "03_crude_exit_by_exposure.csv")
