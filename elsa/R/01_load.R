# =============================================================================
# 01_load.R – read ELSA files and check expected variables
# =============================================================================
message("01: loading ELSA files from ", cfg$data_dir)

needed_main <- c(cfg$id, "indager", "indsex", "samptyp", "askpx", "wpactw", "wpdes", "wpjact",
                 "wplimwk", "helwk", "wpystmoh", "wpystmti", "hehelf", "hehear",
                 paste0("psced", letters[1:8]))
needed_sleep <- c("heslpa", "heslpb", "heslpd", "heslpe", "heslpf")

W <- list(); check <- list()
for (w in cfg$waves_main) {
  p <- find_file(cfg$main_pattern(w), required = w %in% c(cfg$start_waves, cfg$start_waves + 1))
  if (is.na(p)) next
  d <- read_elsa(p)
  ifs <- find_file(cfg$ifs_pattern(w)); fin <- find_file(cfg$fin_pattern(w))
  if (!is.na(ifs)) d <- merge(d, cols(read_elsa(ifs), c(cfg$id, "qual3")), by = cfg$id, all.x = TRUE)
  if (!is.na(fin)) d <- merge(d, cols(read_elsa(fin), c(cfg$id, "totwq5_bu_s")), by = cfg$id, all.x = TRUE)
  W[[as.character(w)]] <- d
  exp_v <- c(needed_main, if (w %in% cfg$start_waves) needed_sleep, if (w %in% c(6, 8)) "heslpx",
             if (w %in% cfg$start_waves) c("qual3", "totwq5_bu_s"))
  miss <- setdiff(exp_v, names(d))
  check[[length(check) + 1]] <- data.frame(wave = w, file = basename(p), n_rows = nrow(d),
                                          missing_expected_vars = paste(miss, collapse = " "))
  message(sprintf("  wave %-2d %-45s %6d rows%s", w, basename(p), nrow(d),
                  if (length(miss)) paste0("  MISSING: ", paste(miss, collapse = " ")) else ""))
}

# Accelerometry (Part B)
am <- find_file(cfg$accel_main); aa <- find_file(cfg$accel_add)
ACC <- NULL
if (!is.na(am)) {
  ACC <- cols(read_elsa(am), c(cfg$id, "num_valid_days", "overall_mean_tst_min"))
  if (!is.na(aa)) ACC <- merge(ACC, cols(read_elsa(aa), c(cfg$id, "overall_mean_sol_min", "overall_mean_waso_min")),
                               by = cfg$id, all.x = TRUE)
  ab <- find_file(cfg$accel_bbaa)
  if (!is.na(ab)) ACC <- merge(ACC, cols(read_elsa(ab), c(cfg$id, "filestarttime")), by = cfg$id, all.x = TRUE)
  check[[length(check) + 1]] <- data.frame(wave = 10, file = basename(am), n_rows = nrow(ACC),
                                          missing_expected_vars = paste(setdiff(c("num_valid_days", "overall_mean_tst_min",
                                            "overall_mean_sol_min", "overall_mean_waso_min", "filestarttime"),
                                            names(ACC)[colSums(!is.na(ACC)) > 0]), collapse = " "))
  message(sprintf("  accelerometry: %d participants", nrow(ACC)))
}

# End-of-life files (deaths)
eol_files <- find_file(cfg$eol_pattern)
DEAD <- character(0)
if (!all(is.na(eol_files))) {
  for (f in eol_files) DEAD <- union(DEAD, read_elsa(f)[[cfg$id]])
  message(sprintf("  end-of-life files: %d, people: %d", length(eol_files), length(DEAD)))
}
write_out(bind(check), "01_file_check.csv")
