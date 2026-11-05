# =============================================================================
# 02_build.R – person-intervals (starting wave -> next wave), derived variables
# Follows the registered "Indices" and "Transformations" sections.
# =============================================================================
message("02: building person-intervals")
idv <- cfg$id

freq4 <- function(x) keep_codes(x, 1:4)
ltc_wave <- function(d, w) {
  out <- data.frame(row.names = seq_len(nrow(d)))
  for (cn in names(cfg$ltc)) {
    v <- cfg$ltc[[cn]]
    out[[cn]] <- as.integer(num(getv(d, v[["new"]])) %in% 1 | num(getv(d, v[["conf"]])) %in% 1)
  }
  out
}
ltc_w10 <- function(d) {
  out <- data.frame(row.names = seq_len(nrow(d)))
  for (cn in names(cfg$ltc_w10)) out[[cn]] <- as.integer(num(getv(d, cfg$ltc_w10[[cn]])) %in% 1)
  out
}
groups <- unique(vapply(cfg$ltc, function(v) v[["group"]], ""))

derive_start <- function(d, w, ltc) {
  x <- data.frame(id = d[[idv]], wave = w, stringsAsFactors = FALSE)
  names(x)[1] <- idv
  x$age <- neg_na(getv(d, "indager"))
  x$female <- as.integer(num(getv(d, "indsex")) == 2)
  x$core <- num(getv(d, "samptyp")) %in% 1
  x$proxy <- num(getv(d, "askpx")) %in% 1
  wa <- keep_codes(getv(d, "wpactw"), 1:4); wd <- neg_na(getv(d, "wpdes"))
  x$working <- ifelse(!is.na(wa), wa %in% 1:2, ifelse(!is.na(wd), wd %in% 2:3, NA))
  x$selfemp <- ifelse(wd %in% 2:3, as.integer(wd == 3), NA)
  x$jobact <- factor(keep_codes(getv(d, "wpjact"), 1:4), levels = 1:4,
                     labels = c("Sedentary", "Standing", "Physical", "Heavy manual"))
  x$job_sedentary <- ifelse(is.na(x$jobact), NA, as.integer(x$jobact == "Sedentary"))
  q <- keep_codes(getv(d, "qual3"), 0:2)
  x$qual3 <- factor(q, levels = c(2, 1, 0), labels = c("Above A-level", "O-level or equiv", "Below O-level"))
  wq <- keep_codes(getv(d, "totwq5_bu_s"), 1:5)
  x$wealth5 <- factor(wq, levels = 5:1, labels = paste0("Q", 5:1))
  x$ltc_n <- rowSums(ltc)
  x$ltc_n4 <- factor(pmin(x$ltc_n, 3), levels = 0:3, labels = c("0", "1", "2", "3+"))
  x$ltc_any <- as.integer(x$ltc_n >= 1)
  for (g in groups) {
    mem <- names(cfg$ltc)[vapply(cfg$ltc, function(v) v[["group"]] == g, TRUE)]
    x[[paste0("grp_", g)]] <- as.integer(rowSums(ltc[, intersect(mem, names(ltc)), drop = FALSE]) > 0)
  }
  # sleep
  a <- freq4(getv(d, "heslpa")); b <- freq4(getv(d, "heslpb"))
  x$insom_freq_n <- ifelse(is.na(a) & is.na(b), NA, pmax(a, b, na.rm = TRUE))
  x$insom_freq <- factor(x$insom_freq_n, levels = 1:4,
                         labels = c("Not in past month", "<1/week", "1-2/week", "3+/week"))
  x$restless <- ifelse(num(getv(d, "pscedc")) %in% 1, 1L, ifelse(num(getv(d, "pscedc")) %in% 2, 0L, NA))
  x$tired_n <- freq4(getv(d, "heslpd"))
  x$tired <- factor(x$tired_n, levels = 1:4, labels = levels(x$insom_freq))
  x$doze_n <- freq4(getv(d, "heslpx"))
  x$doze <- factor(x$doze_n, levels = 1:4, labels = levels(x$insom_freq))
  x$night_freq <- ifelse(is.na(x$insom_freq_n), NA, as.integer(x$insom_freq_n == 4))
  x$tired_freq <- ifelse(is.na(x$tired_n), NA, as.integer(x$tired_n == 4))
  x$grp4 <- factor(ifelse(is.na(x$night_freq) | is.na(x$tired_freq), NA,
                   ifelse(x$night_freq == 0 & x$tired_freq == 0, "Neither",
                   ifelse(x$night_freq == 1 & x$tired_freq == 0, "Night-time only",
                   ifelse(x$night_freq == 0, "Waking tired only", "Both")))),
                   levels = c("Neither", "Night-time only", "Waking tired only", "Both"))
  hrs <- neg_na(getv(d, "heslpe")); hrs[!is.na(hrs) & (hrs < 2 | hrs > 16)] <- NA
  x$short_sleep <- ifelse(is.na(hrs), NA, as.integer(hrs < 6))
  qual <- keep_codes(getv(d, "heslpf"), 1:4)
  x$poor_quality <- ifelse(is.na(qual), NA, as.integer(qual >= 3))
  # depressive symptoms without the sleep item
  neg <- sapply(c("psceda", "pscedb", "pscede", "pscedg", "pscedh"), function(v) {
    z <- num(getv(d, v)); ifelse(z %in% 1, 1, ifelse(z %in% 2, 0, NA)) })
  pos <- sapply(c("pscedd", "pscedf"), function(v) {
    z <- num(getv(d, v)); ifelse(z %in% 2, 1, ifelse(z %in% 1, 0, NA)) })
  m <- cbind(neg, pos); n_ok <- rowSums(!is.na(m))
  x$cesd7 <- ifelse(n_ok >= 6, rowSums(m, na.rm = TRUE) * 7 / pmax(n_ok, 1), NA)
  hf <- keep_codes(getv(d, "hehelf"), 1:5)
  x$srh <- factor(ifelse(is.na(hf), NA, ifelse(hf <= 2, "Excellent/very good", ifelse(hf == 3, "Good",
                  ifelse(hf == 4, "Fair", "Poor")))), levels = c("Excellent/very good", "Good", "Fair", "Poor"))
  hh <- keep_codes(getv(d, "hehear"), 1:5)
  x$hearing_poor <- ifelse(is.na(hh), NA, as.integer(hh >= 4))
  x$helwk <- ifelse(num(getv(d, "helwk")) %in% 1, 1L, ifelse(num(getv(d, "helwk")) %in% 2, 0L, NA))
  x$wplimwk0 <- ifelse(num(getv(d, "wplimwk")) %in% 1, 1L, ifelse(num(getv(d, "wplimwk")) %in% 2, 0L, NA))
  x$xwgt <- neg_na(getv(d, paste0("w", w, "xwgt")))
  x$int_year <- neg_na(getv(d, "iintdaty")); x$int_month <- keep_codes(getv(d, "iintdatm"), 1:12)
  x
}

derive_next <- function(d) {
  y <- data.frame(id = d[[idv]], stringsAsFactors = FALSE); names(y)[1] <- idv
  wa <- keep_codes(getv(d, "wpactw"), 1:4); wd <- neg_na(getv(d, "wpdes"))
  y$working1 <- ifelse(!is.na(wa), wa %in% 1:2, ifelse(!is.na(wd), wd %in% 2:3, NA))
  y$retired1 <- as.integer(wd %in% 1)
  y$health_reason <- as.integer(num(getv(d, "wpystmoh")) %in% 1 | num(getv(d, "wpystmti")) %in% 1 | wd %in% 5)
  y$wplimwk1 <- ifelse(num(getv(d, "wplimwk")) %in% 1, 1L, ifelse(num(getv(d, "wplimwk")) %in% 2, 0L, NA))
  y$interviewed1 <- TRUE
  y
}

make_intervals <- function(s, nxt, base_extra = NULL) {
  d0 <- W[[as.character(s)]]; d1 <- W[[as.character(nxt)]]
  if (is.null(d0) || is.null(d1)) return(NULL)
  ltc <- if (s == cfg$partb_wave) ltc_w10(d0) else ltc_wave(d0, s)
  x <- derive_start(d0, s, ltc)
  if (!is.null(base_extra)) x <- merge(x, base_extra, by = idv, all.x = TRUE)
  x <- merge(x, derive_next(d1), by = idv, all.x = TRUE)
  x$interviewed1[is.na(x$interviewed1)] <- FALSE
  x$died <- !x$interviewed1 & x[[idv]] %in% DEAD
  x$exit <- ifelse(x$interviewed1 & !is.na(x$working1), as.integer(!x$working1), NA)
  x$exit3 <- factor(ifelse(is.na(x$exit), NA, ifelse(x$exit == 0, "Stayed",
                    ifelse(x$health_reason %in% 1, "Health-related exit", "Other exit"))),
                    levels = c("Stayed", "Health-related exit", "Other exit"))
  x$exit_retire <- ifelse(is.na(x$exit), NA, as.integer(x$exit == 1 & x$retired1 %in% 1))
  x$exit_nonretire <- ifelse(is.na(x$exit), NA, as.integer(x$exit == 1 & !(x$retired1 %in% 1)))
  x$newlimit <- ifelse(x$exit %in% 0 & x$wplimwk0 %in% 0, x$wplimwk1, NA)
  x
}

# ---- Part A ---------------------------------------------------------------------------
iv_all <- bind(lapply(cfg$start_waves, function(s) make_intervals(s, s + 1)))
flow <- list()
fl <- function(step, d) flow[[length(flow) + 1]] <<- data.frame(step = step, intervals = nrow(d),
                                                                  people = length(unique(d[[idv]])))
fl("Interviewed at a starting wave (4, 6, 8)", iv_all)
s1 <- iv_all[iv_all$core, ];                                   fl("Core sample member", s1)
s2 <- s1[!s1$proxy, ];                                         fl("Not a proxy interview", s2)
s3 <- s2[!is.na(s2$age) & s2$age >= cfg$age_min & s2$age < cfg$age_max, ]; fl("Aged 50 to under 65", s3)
s4 <- s3[s3$working %in% TRUE, ];                              fl("In paid work", s4)
s5 <- s4[!is.na(s4$insom_freq_n) | !is.na(s4$restless), ];     fl("Answered sleep items", s5)
s6 <- s5[!s5$died, ];                                          fl("Alive at next wave", s6)
iv_ipw <- s6                                                   # includes those lost to follow-up
iv <- s6[s6$interviewed1, ];                                   fl("Interviewed at next wave (analysis set)", iv)
iv <- iv[order(iv[[idv]], iv$wave), ]
iv$first_int <- as.integer(!duplicated(iv[[idv]]))
iv$wave_f <- factor(iv$wave)
iv$insom_score <- iv$insom_freq_n
fl_out <- bind(flow); fl_out$exits <- c(rep(NA, nrow(fl_out) - 1), sum(iv$exit, na.rm = TRUE))
write_out(suppress_counts(fl_out, c("intervals", "people", "exits")), "02_flow_partA.csv")

# Loss-to-follow-up weights (stabilised, truncated 1st/99th percentiles)
iv_ipw$wave_f <- factor(iv_ipw$wave)
fw <- stats::reformulate(c("ns(age, df = 3)", "female", "wave_f", "qual3", "wealth5", "selfemp",
                           "jobact", "ltc_n4", "insom_freq", "restless"), response = "interviewed1")
dw <- complete_for(iv_ipw, fw, idv)
if (nrow(dw) > 50 && length(unique(dw$interviewed1)) == 2) {
  pf <- stats::glm(fw, data = dw, family = stats::binomial())
  ps <- stats::predict(pf, dw, type = "response")
  wt <- mean(dw$interviewed1) / ps
  q <- stats::quantile(wt, c(.01, .99)); wt <- pmin(pmax(wt, q[1]), q[2])
  key <- paste(dw[[idv]], dw$wave)
  iv$ipw <- wt[match(paste(iv[[idv]], iv$wave), key)]
} else iv$ipw <- NA

# ---- Part B ---------------------------------------------------------------------------
pb <- NULL
if (!is.null(ACC) && !is.null(W[[as.character(cfg$partb_wave)]])) {
  acc <- ACC
  acc$tst_h <- neg_na(acc$overall_mean_tst_min) / 60
  acc$tst_h[!is.na(acc$tst_h) & (acc$tst_h < 2 | acc$tst_h > 14)] <- NA
  acc$waso_30 <- neg_na(acc$overall_mean_waso_min) / 30
  acc$sol_15 <- neg_na(acc$overall_mean_sol_min) / 15
  acc$valid_days <- neg_na(acc$num_valid_days)
  acc$tst_cat <- factor(ifelse(is.na(acc$tst_h), NA, ifelse(acc$tst_h < 6, "<6 h", ifelse(acc$tst_h < 8, "6-<8 h", "8+ h"))),
                        levels = c("6-<8 h", "<6 h", "8+ h"))
  acc$has_accel <- TRUE
  # month of wear from the start of the monitoring period (first yyyy-mm or dd/mm/yyyy found)
  st <- as.character(getv(acc, "filestarttime"))
  ok <- !is.na(st) & grepl("[0-9]{4}-[0-9]{2}", st)
  acc$wear_month <- NA_integer_
  acc$wear_month[ok] <- as.integer(substr(regmatches(st[ok], regexpr("[0-9]{4}-[0-9]{2}", st[ok])), 6, 7))
  ok2 <- !is.na(st) & !ok & grepl("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", st)
  acc$wear_month[ok2] <- as.integer(sub("^[0-9]{1,2}/([0-9]{1,2})/.*$", "\\1",
                                        regmatches(st[ok2], regexpr("[0-9]{1,2}/[0-9]{1,2}/[0-9]{4}", st[ok2]))))
  acc$wear_month[!(acc$wear_month %in% 1:12)] <- NA
  pb_all <- make_intervals(cfg$partb_wave, cfg$partb_wave + 1,
                           base_extra = acc[, c(idv, "tst_h", "tst_cat", "waso_30", "sol_15", "valid_days", "has_accel", "wear_month")])
  pb_all$wear_month[is.na(pb_all$wear_month)] <- pb_all$int_month[is.na(pb_all$wear_month)]   # fallback: interview month
  pb_all$season <- factor(c("Winter", "Winter", "Spring", "Spring", "Spring", "Summer", "Summer", "Summer",
                            "Autumn", "Autumn", "Autumn", "Winter")[pb_all$wear_month],
                          levels = c("Spring", "Summer", "Autumn", "Winter"))
  pb_all$int_year_f <- factor(pb_all$int_year)
  flb <- list(); flb2 <- function(step, d) flb[[length(flb) + 1]] <<- data.frame(step = step, people = nrow(d))
  flb2("Interviewed at wave 10", pb_all)
  b1 <- pb_all[pb_all$core & !pb_all$proxy & !is.na(pb_all$age) & pb_all$age >= cfg$age_min & pb_all$age < cfg$age_max &
               pb_all$working %in% TRUE, ];                    flb2("Core, not proxy, aged 50-64, in paid work", b1)
  b2 <- b1[b1$has_accel %in% TRUE, ];                          flb2("Accelerometer sleep summary", b2)
  b3 <- b2[!is.na(b2$valid_days) & b2$valid_days >= cfg$min_valid_days, ]; flb2("At least 3 valid days", b3)
  b4 <- b3[!b3$died & b3$interviewed1, ];                       flb2("Alive and interviewed at wave 11 (analysis set)", b4)
  pb <- b4
  pb_excl <- b2[!(b2[[idv]] %in% b3[[idv]]), ]                  # fewer than 3 valid days
  flb_out <- bind(flb); flb_out$exits <- c(rep(NA, nrow(flb_out) - 1), sum(pb$exit, na.rm = TRUE))
  write_out(suppress_counts(flb_out, c("people", "exits")), "02_flow_partB.csv")
}
message(sprintf("  Part A: %d intervals, %d people, %d exits", nrow(iv), length(unique(iv[[idv]])), sum(iv$exit, na.rm = TRUE)))
