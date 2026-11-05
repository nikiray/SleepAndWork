# =============================================================================
# config.R – ELSA companion study (pre-registered on OSF)
# "Sleep and early work exit in long-term conditions: a companion analysis in
#  the English Longitudinal Study of Ageing"
# All settings in one place. Run from the elsa/ folder.
# =============================================================================

cfg <- list()
cfg$data_dir   <- "data/raw"     # ELSA SN 5050 tab-delimited files go here (never commit)
cfg$output_dir <- "outputs"      # aggregate outputs only

cfg$id <- "idauniq"

# ---- Files (regular expressions, matched case-insensitively; .tab or .csv) ----------
# If a pattern matches the wrong file, or none, put the exact file name here.
cfg$main_pattern <- function(w) sprintf("^wave_%d_elsa_data(_eul)?(_v[0-9]+)?\\.(tab|csv)$", w)
cfg$ifs_pattern  <- function(w) sprintf("^wave_%d_ifs_derived_variables\\.(tab|csv)$", w)
cfg$fin_pattern  <- function(w) sprintf("^wave_%d_financial_derived_variables\\.(tab|csv)$", w)
cfg$accel_main   <- "^wave_10_accelrmtry_summary_asleep_mainvars.*\\.(tab|csv)$"
cfg$accel_add    <- "^wave_10_accelrmtry_summary_asleep_addvars.*\\.(tab|csv)$"
cfg$accel_bbaa   <- "^wave_10_accelrmtry_summary_bbaa_mainvars.*\\.(tab|csv)$"   # filestarttime (month of wear)
cfg$eol_pattern  <- "(^|_)(eol|endoflife)(_|\\.).*(tab|csv)$"   # end-of-life (deaths); harmonised h_ files excluded

cfg$waves_main   <- 4:11
cfg$start_waves  <- c(4, 6, 8)      # waves with the sleep module (Part A)
cfg$partb_wave   <- 10

# ---- Eligibility ---------------------------------------------------------------------
cfg$age_min <- 50
cfg$age_max <- 65     # under 65
cfg$min_valid_days <- 3

# ---- Long-term conditions (ELSA health-conditions lookup table, waves 4-9) -------------
# condition present at a wave if newly reported (prefix_new == 1) or previously reported
# and confirmed (prefix_conf == 1); disputed (prefix_conf == 2) = absent
cfg$ltc <- list(
  lung      = c(new = "hediblu", conf = "hedbdlu", group = "respiratory"),
  asthma    = c(new = "hedibas", conf = "hedbdas", group = "respiratory"),
  arthritis = c(new = "hedibar", conf = "hedbdar", group = "musculoskeletal"),
  osteopor  = c(new = "hedibos", conf = "hedbdos", group = "musculoskeletal"),
  cancer    = c(new = "hedibca", conf = "hedbdca", group = "cancer"),
  parkinson = c(new = "hedibpd", conf = "hedbdpd", group = "neurological"),
  psych     = c(new = "hedibps", conf = "hedbdps", group = "psychiatric"),
  alzheimer = c(new = "hedibad", conf = "hedbdad", group = "neurological"),
  dementia  = c(new = "hedibde", conf = "hedbdde", group = "neurological"),
  ms_mnd    = c(new = "hedibms", conf = "hedbdms", group = "neurological"),   # from wave 6
  hbp       = c(new = "hediabp", conf = "hedacbp", group = "cardiometabolic"),
  angina    = c(new = "hediaan", conf = "hedacan", group = "cardiometabolic"),
  mi        = c(new = "hediami", conf = "hedacmi", group = "cardiometabolic"),
  hf        = c(new = "hediahf", conf = "hedachf", group = "cardiometabolic"),
  rhythm    = c(new = "hediaar", conf = "hedacar", group = "cardiometabolic"),
  diabetes  = c(new = "hediadi", conf = "hedacdi", group = "cardiometabolic"),
  stroke    = c(new = "hediast", conf = "hedacst", group = "cardiometabolic")
)
# Wave 10 (Part B): redesigned health module, "ever diagnosed" items (1 = yes)
cfg$ltc_w10 <- c(lung = "heevercl", asthma = "heeveras", arthritis = "heeverar", osteopor = "heeveros",
                 cancer = "heeverca", parkinson = "heeverpd", psych = "heeverps", alzheimer = "heeverad",
                 dementia = "heeverdm", ms_mnd = "heeverms", hbp = "heeverbp", angina = "heeveran",
                 mi = "heevermi", hf = "heeverhf", rhythm = "heeverah", diabetes = "heacd",
                 stroke = "heeverst")

# ---- Analysis ------------------------------------------------------------------------
cfg$n_boot   <- 1000
cfg$seed     <- 20261105
cfg$min_cell <- 10      # suppress counts below this in outputs
cfg$min_exits_subgroup <- 50
cfg$ratio_threshold <- 1.2   # "materially larger": OR(H2 level 4) / OR(H1) >= 1.2
cfg$mi_threshold <- 0.05     # multiple imputation if > 5% intervals miss a Model 2 covariate
cfg$n_imp <- 20
