# =============================================================================
# config.R – settings for the Fox Insight sleep and work-exit analysis
# Edit this file only; the scripts in R/ read everything from `cfg`.
# =============================================================================

cfg <- list()

# ---- Folders -----------------------------------------------------------------
cfg$data_dir   <- "data/raw"   # folder holding the Fox DEN CSVs (one per survey)
cfg$output_dir <- "outputs"    # aggregate results only (safe to share)

# ---- Which file is which -----------------------------------------------------
# Case-insensitive regular expressions matched against file names in data_dir.
# If a pattern matches more than one file, replace it with the exact file name.
cfg$files <- list(
  about   = "^about",                 # About You (employment, age, sex, ...)
  users   = "^users|^registration",   # Registration (InitPDDiag, YearsWithPD, LocCountry)
  general = "^general",               # CurrPDDiag
  nonmove = "^nonmove",               # NMS-Quest
  mood    = "^mood([^a-z]|$)",        # GDS-15 (not the one-time mood-disorder survey)
  move    = "^move",                  # MDS-UPDRS Part II
  daily   = "^daily",                 # PDAQ-15
  meds    = "^medscurrpd",            # PD medications
  pdprop2 = "^pdprop2",               # PD-PROP 2.0
  sleep   = "^sleep",                 # RBD1Q
  # added in plan v0.2 (all optional; analyses using them are skipped if absent)
  curr    = "^curr",                  # Your Current Health (comorbidities)
  pase    = "^pase",                  # Your Physical Activities (job demands)
  eq5d    = "^physical|eq-?5d",       # Your Physical Experiences (EQ-5D-5L)
  medhis  = "^medhis|^medical",       # Your Medical History (sleep apnoea, insomnia)
  mind    = "^mind",                  # Role of Stress in PD (PSS-10), one-time
  cov     = "^cov([^0-9a-z]|$)",      # COVID-19 Experience (first survey, not COV2)
  fatigue = "^fatigue",               # Understanding Fatigue (used for calendar anchoring)
  fnclimp = "^fnclimp",               # Financial and Social Impact 2025 (reasons for exit)
  pdmb    = "^pdmb"                   # PD Microbiome survey (night/shift work)
)
cfg$required_tables <- c("about", "users", "general", "nonmove", "mood", "move")

# ---- Sample definition -------------------------------------------------------
cfg$age_min        <- 18
cfg$age_max        <- 65     # working age: age at t < age_max
cfg$gap_min_days   <- 180    # interval between employment reports
cfg$gap_max_days   <- 548
cfg$min_exit_events <- 100   # decision rule (Appendix B of the plan)

# ---- Time windows (days, relative to t = start of interval) ------------------
cfg$win_before     <- 120    # covariates/exposures: t - 120 ...
cfg$win_after      <- 30     # ... to t + 30
cfg$chronic_days   <- 365    # chronicity window: t - 365 ... t
cfg$med_start      <- 30     # mediators: t + 30 ...
cfg$med_end_buffer <- 60     # ... to t1 - 60

# ---- Variable names (lower case; column names are lower-cased on loading) ----
cfg$v <- list(
  id = "fox_insight_id", day = "days_elapsed",
  employment = "employment", age = "age",
  sex = "sex", sex_birth = "sexatbirth", education = "education", income = "income",
  init_pd = "initpddiag", curr_pd = "currpddiag", years_pd = "yearswithpd",
  country = "loccountry",
  sleepiness = "nonmoveawake", insomnia = "nonmovesleep",
  rls = "nonmovesens", nocturia = "nonmoveurinepm", rbd_nms = "nonmovetalk",
  anxiety = "nonmoveanxious", concentration = "nonmoveconcent", smell = "nonmovesmell",
  rbd1q = "sleep", energy = "moodenergy"
)
cfg$exposures_primary   <- c(sleepiness = "nonmoveawake", insomnia = "nonmovesleep")
cfg$exposures_secondary <- c(rls = "nonmovesens", nocturia = "nonmoveurinepm",
                             rbd = "nonmovetalk", pdprop_sleep = "pdprop_sleep")

# GDS-15 items; depressive answer is 1 ("Yes") except these, where it is 0 ("No")
cfg$gds_items    <- c("moodafraid", "moodalive", "moodbetter", "moodbored", "moodempty",
                      "moodenergy", "moodhappy", "moodhelp", "moodhome", "moodhopeless",
                      "moodinterest", "moodmemory", "moodsatis", "moodspirits", "moodworth")
cfg$gds_reversed <- c("moodalive", "moodenergy", "moodhappy", "moodsatis", "moodspirits")
cfg$gds_cutoff   <- 5        # GDS-14 (energy removed) >= 5 = depressive symptoms

cfg$updrs2_items <- c("movespeech", "movesaliva", "movechew", "moveeat", "movedress",
                      "movehygiene", "movewrite", "movehobby", "movesleep", "movetremor",
                      "moveup", "movewalk", "movefreeze")
cfg$pdaq_items   <- c("dailyclock", "dailydiscuss", "dailyerrand", "dailyexplain",
                      "dailyfinance", "dailyinstruct", "dailylearn", "dailymany",
                      "dailymap", "dailymoney", "dailynumber", "dailyproblem",
                      "dailyread", "dailyrememb", "dailythought")

# Dopamine agonists in MedsCurrPD (pramipexole, ropinirole, rotigotine,
# bromocriptine, apomorphine)
cfg$agonist_vars <- c("medscurrpdsifro", "medscurrpdmiraer", "medscurrpdrequi",
                      "medscurrpdrequixl", "medscurrpdread", "medscurrpdharu",
                      "medscurrpdneupr", "medscurrpdparlo", "medscurrpdapok",
                      "medscurrpdkynm")

# ---- Plan v0.2 additions -------------------------------------------------------
cfg$annual_lookback <- 400   # job demands / comorbidities: t - 400 ... t + 30
# physical conditions counted in the multimorbidity covariate (depression/anxiety excluded)
cfg$ltc_lim_vars <- c("currarthlim", "currbacklim", "currbloodlim", "currcancerlim",
                      "currdiabeteslim", "currgastriclim", "currhbplim", "currheartlim",
                      "currkidneylim", "currliverlim", "currlunglim")
# condition groups for the cross-condition analysis (comparison cohort)
cfg$ltc_groups <- list(diabetes = "currdiabetes", heart = c("currheart", "currhbp"),
                       lung = "currlung", msk = c("currarth", "currback"),
                       mental_health = c("currdepress", "curranxiety"))
cfg$pss_items <- c("mindstressfeelunexpect", "mindstressfeelcontrol", "mindstressfeelnervous",
                   "mindstressfeelconfident", "mindstressfeelyourway", "mindstressfeelcope",
                   "mindstressfeelirritate", "mindstressfeelontop", "mindstressfeelangry",
                   "mindstressfeelpiled")
cfg$pss_reversed <- c("mindstressfeelconfident", "mindstressfeelyourway",
                      "mindstressfeelirritate", "mindstressfeelontop")
# one-time surveys with known fielding windows, used to anchor calendar time
cfg$anchor_windows <- list(mind    = c("2019-08-01", "2019-09-30"),
                           fatigue = c("2019-03-01", "2019-06-30"))
cfg$pandemic <- c("2020-03-01", "2021-12-31")

# ---- Inference ---------------------------------------------------------------
cfg$n_boot   <- 1000   # cluster bootstrap resamples (set to 50 for a quick test)
cfg$n_boot_med <- 500  # bootstrap resamples per mediation analysis (10 analyses)
cfg$seed     <- 20261103
cfg$min_cell <- 10     # suppress any count below this in exported tables
