# =============================================================================
# simulate_elsa.R – fake data in ELSA SN 5050 format, for testing the code only.
# Variable names and codes follow the ELSA data dictionaries (waves 4-11).
# Built-in effect: frequent insomnia raises the chance of leaving work; the yes/no
# restless-sleep item is only loosely related to it. Numbers mean nothing.
# Writes tab-delimited files to sim/data/.
# =============================================================================

set.seed(1)
N <- 8000
ids <- 100000 + seq_len(N)
age4 <- round(runif(N, 38, 74))     # wide range so that some are 50-64 and working at wave 10 (Part B)
sex  <- sample(1:2, N, TRUE)
sleep_trait <- rnorm(N)                        # stable tendency to poor sleep
ltc_trait   <- rnorm(N)
qual3 <- sample(0:2, N, TRUE, prob = c(.3, .4, .3))
wealth <- sample(1:5, N, TRUE)
jact <- sample(1:4, N, TRUE, prob = c(.45, .3, .2, .05))
core <- ifelse(runif(N) < .9, 1, 2)
wave_years <- c(`4` = 0, `5` = 2, `6` = 4, `7` = 6, `8` = 8, `9` = 10, `10` = 13, `11` = 16)

alive <- rep(TRUE, N); working <- runif(N) < plogis(3 - 0.25 * (age4 - 50))
in_study <- rep(TRUE, N); died_wave <- rep(NA, N)
lag_freq <- rep(1, N)
cond_codes <- c(chronic = "lu as ar os ca pd ps ad de ms", cvd = "bp an mi hf ar di st")
chron <- strsplit(cond_codes[["chronic"]], " ")[[1]]; cvd <- strsplit(cond_codes[["cvd"]], " ")[[1]]
had <- matrix(FALSE, N, length(chron) + length(cvd),
              dimnames = list(NULL, c(paste0("c_", chron), paste0("v_", cvd))))
rate <- c(.03, .04, .12, .03, .03, .003, .05, .002, .003, .003, .15, .03, .02, .01, .03, .06, .02)

dir.create("sim/data", showWarnings = FALSE, recursive = TRUE)
w_tab <- function(x, f) utils::write.table(x, file.path("sim/data", f), sep = "\t", row.names = FALSE,
                                           quote = FALSE, na = "-1")
code_na <- function(x, p = .02) { x[runif(length(x)) < p] <- -9; x }

for (w in 4:11) {
  if (w > 4) {
    # deaths and attrition since the last wave
    die <- alive & in_study & runif(N) < 0.012 * exp(0.06 * (age4 + wave_years[[as.character(w)]] - 60))
    died_wave[die] <- w; alive[die] <- FALSE
    drop <- alive & in_study & runif(N) < 0.08
    in_study[drop] <- FALSE
  }
  r <- which(alive & in_study)
  n <- length(r)
  age <- age4[r] + wave_years[[as.character(w)]]
  # long-term conditions accumulate
  newc <- matrix(runif(n * ncol(had)), n) < outer(plogis(ltc_trait[r] - 1), rate) * 1.5
  prev <- had[r, , drop = FALSE]
  had[r, ] <- prev | newc
  ltc_n <- rowSums(had[r, , drop = FALSE])
  # sleep
  sl <- sleep_trait[r] + 0.3 * ltc_n / 3 + rnorm(n, 0, .8)
  freq <- as.integer(cut(sl, c(-Inf, -0.3, 0.4, 1.1, Inf)))
  heslpa <- pmin(4, pmax(1, freq + sample(-1:0, n, TRUE)))
  heslpb <- pmin(4, pmax(1, freq + sample(-1:0, n, TRUE, prob = c(.4, .6))))
  # work transitions: exit risk rises with age, frequent insomnia, conditions
  wk_prev <- working[r]
  ins_prev <- lag_freq[r]
  p_exit <- plogis(-2.8 + 0.12 * (age - 58) + 0.35 * (ins_prev == 4) + 0.15 * (ins_prev == 3) + 0.15 * ltc_n)
  ex <- wk_prev & runif(n) < p_exit
  working[r] <- wk_prev & !ex
  wpactw <- ifelse(working[r], sample(1:2, n, TRUE, prob = c(.95, .05)), 4)
  wpdes <- ifelse(working[r], sample(2:3, n, TRUE, prob = c(.85, .15)),
                  ifelse(age >= 60, 1, sample(c(1, 4, 5, 6), n, TRUE, prob = c(.4, .2, .25, .15))))
  health_ex <- ex & runif(n) < plogis(-1.5 + 0.5 * (ins_prev == 4) + 0.2 * ltc_n)
  d <- data.frame(idauniq = ids[r], indager = ifelse(age >= 90, -7, age), indsex = sex[r],
                  samptyp = core[r], askpx = ifelse(runif(n) < .02, 1, 0),
                  wpactw = code_na(wpactw, .01), wpdes = wpdes,
                  wpjact = ifelse(working[r], jact[r], -1),
                  wplimwk = ifelse(working[r], ifelse(runif(n) < plogis(-2.5 + 0.4 * (freq == 4) + 0.2 * ltc_n), 1, 2), -1),
                  helwk = ifelse(runif(n) < plogis(-2 + 0.3 * ltc_n), 1, 2),
                  wpystmoh = ifelse(ex, as.integer(health_ex), -1),
                  wpystmti = ifelse(ex, as.integer(runif(n) < .15), -1),
                  hehelf = pmin(5, pmax(1, round(2.6 + 0.3 * ltc_n + 0.2 * sl + rnorm(n, 0, .8)))),
                  hehear = pmin(5, pmax(1, round(2.3 + 0.03 * (age - 60) + rnorm(n, 0, .9)))),
                  pscedc = ifelse(runif(n) < plogis(-1 + 0.6 * sl), 1, 2),
                  iintdaty = 2008 + wave_years[[as.character(w)]] + sample(0:1, n, TRUE),
                  iintdatm = sample(1:12, n, TRUE))
  d[[paste0("w", w, "xwgt")]] <- round(runif(n, .5, 2), 3)
  for (it in c("psceda", "pscedb", "pscede", "pscedg", "pscedh"))
    d[[it]] <- ifelse(runif(n) < plogis(-1.8 + 0.3 * sl), 1, 2)
  for (it in c("pscedd", "pscedf")) d[[it]] <- ifelse(runif(n) < plogis(1.8 - 0.3 * sl), 1, 2)
  if (w %in% c(4, 6, 8)) {
    d$heslpa <- code_na(heslpa); d$heslpb <- code_na(heslpb)
    d$heslpd <- code_na(pmin(4, pmax(1, freq + sample(-1:1, n, TRUE))))
    d$heslpe <- round(pmax(3, pmin(11, 7.2 - 0.4 * sl + rnorm(n, 0, 1))), 1)
    d$heslpf <- pmin(4, pmax(1, round(2 + 0.5 * sl + rnorm(n, 0, .6))))
    if (w >= 6) { d$heslpx <- code_na(sample(1:4, n, TRUE, prob = c(.5, .25, .15, .1))); d$heslpc <- rpois(n, 3) }
  }
  if (w <= 9) {
    for (j in seq_along(chron)) {
      cc <- chron[j]; if (cc == "ms" && w < 6) next
      d[[paste0("hedib", cc)]] <- as.integer(newc[, j] & !prev[, j])
      d[[paste0("hedbd", cc)]] <- ifelse(prev[, j], ifelse(runif(n) < .97, 1, 2), -1)
    }
    for (j in seq_along(cvd)) {
      cc <- cvd[j]; k <- length(chron) + j
      d[[paste0("hedia", cc)]] <- as.integer(newc[, k] & !prev[, k])
      d[[paste0("hedac", cc)]] <- ifelse(prev[, k], ifelse(runif(n) < .97, 1, 2), -1)
    }
  } else if (w == 10) {
    nm10 <- c("heevercl", "heeveras", "heeverar", "heeveros", "heeverca", "heeverpd", "heeverps",
              "heeverad", "heeverdm", "heeverms", "heeverbp", "heeveran", "heevermi", "heeverhf",
              "heeverah", "heacd", "heeverst")
    for (j in seq_along(nm10)) d[[nm10[j]]] <- ifelse(had[r, j], 1, 2)
  }
  w_tab(d, sprintf("wave_%d_elsa_data_eul.tab", w))
  w_tab(data.frame(idauniq = ids[r], qual3 = qual3[r]), sprintf("wave_%d_ifs_derived_variables.tab", w))
  w_tab(data.frame(idauniq = ids[r], totwq5_bu_s = wealth[r]), sprintf("wave_%d_financial_derived_variables.tab", w))
  if (w == 10) {
    wear <- r[runif(n) < .5]
    sl10 <- sleep_trait[wear] + rnorm(length(wear), 0, .7)
    w_tab(data.frame(idauniq = ids[wear], num_valid_days = sample(1:8, length(wear), TRUE, prob = c(1, 1, 2, 3, 4, 6, 8, 10)),
                     overall_mean_tst_min = round(pmax(200, 420 - 25 * sl10 + rnorm(length(wear), 0, 40)))),
          "wave_10_accelrmtry_summary_asleep_mainvars.tab")
    w_tab(data.frame(idauniq = ids[wear], overall_mean_sol_min = round(pmax(2, 18 + 6 * sl10 + rnorm(length(wear), 0, 6))),
                     overall_mean_waso_min = round(pmax(5, 60 + 15 * sl10 + rnorm(length(wear), 0, 15)))),
          "wave_10_accelrmtry_summary_asleep_addvars.tab")
    w_tab(data.frame(idauniq = ids[wear], filestarttime = sprintf("2022-%02d-%02d 10:00:00",
                     sample(1:12, length(wear), TRUE), sample(1:28, length(wear), TRUE))),
          "wave_10_accelrmtry_summary_bbaa_mainvars.tab")
  }
  lag_freq[r] <- if (w %in% c(4, 6, 8)) freq else lag_freq[r]
}
# end-of-life interviews for people who died (one file, wave of report)
dd <- which(!is.na(died_wave))
w_tab(data.frame(idauniq = ids[dd], eolwave = died_wave[dd]), "elsa_eol_sim_eul.tab")
message("Simulated ELSA-format data written to sim/data/")
