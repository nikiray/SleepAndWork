# =============================================================================
# 01_load.R – find and read the Fox DEN tables; report what was found
# Output: outputs/01_file_check.csv (no participant-level data)
# =============================================================================

message("01: loading tables from ", cfg$data_dir)
tabs <- list()
check <- data.frame()

expected_vars <- list(
  about   = c("employment", "age", "education", "income", "sex", "sexatbirth"),
  users   = c("initpddiag", "yearswithpd", "loccountry"),
  general = c("currpddiag"),
  nonmove = c("nonmoveawake", "nonmovesleep", "nonmovesens", "nonmoveurinepm",
              "nonmovetalk", "nonmoveanxious", "nonmoveconcent", "nonmovesmell"),
  mood    = cfg$gds_items,
  move    = cfg$updrs2_items,
  daily   = cfg$pdaq_items,
  meds    = cfg$agonist_vars,
  pdprop2 = c(paste0("pdprop2fatig_", 1:5), paste0("pdprop2slp_", 1:5)),
  sleep   = c("sleep"),
  curr    = c(cfg$ltc_lim_vars, unlist(cfg$ltc_groups)),
  pase    = c("workactive", "work"),
  eq5d    = c("active"),
  medhis  = c("sleepap", "insom"),
  mind    = cfg$pss_items,
  cov     = c("covsocdiswork"),
  fatigue = character(0),
  fnclimp = c("fnclimpjobimpearlyretrmnt", "fnclimpjobimpwrkfwrhrs"),
  pdmb    = c("pdmbworknight")
)

for (nm in names(cfg$files)) {
  path <- find_file(cfg$files[[nm]], cfg$data_dir)
  if (is.na(path)) {
    if (nm %in% cfg$required_tables)
      stop(sprintf("Required table '%s' not found (pattern '%s') in %s",
                   nm, cfg$files[[nm]], cfg$data_dir), call. = FALSE)
    message(sprintf("  %-8s not found (optional) – related variables will be missing", nm))
    check <- rbind(check, data.frame(table = nm, file = NA, n_rows = 0, n_participants = 0,
                                     missing_expected_vars = "table not found"))
    next
  }
  d <- read_fox(path)
  tabs[[nm]] <- d
  miss <- setdiff(expected_vars[[nm]], names(d))
  # sex/sexatbirth: only one of the two is needed
  if (nm == "about" && any(c("sex", "sexatbirth") %in% names(d))) miss <- setdiff(miss, c("sex", "sexatbirth"))
  check <- rbind(check, data.frame(table = nm, file = basename(path), n_rows = nrow(d),
                                   n_participants = length(unique(d[[cfg$v$id]])),
                                   missing_expected_vars = paste(miss, collapse = "; ")))
  message(sprintf("  %-8s %-40s %8d rows %7d participants %s", nm, basename(path), nrow(d),
                  length(unique(d[[cfg$v$id]])),
                  if (length(miss)) paste("| MISSING:", paste(miss, collapse = ", ")) else ""))
}

if (!"employment" %in% names(tabs$about)) stop("About table has no 'Employment' column.", call. = FALSE)
if (!"age" %in% names(tabs$about))
  stop("About table has no 'Age' column. Check the export and set cfg$v$age.", call. = FALSE)

write_out(check, "01_file_check.csv")
