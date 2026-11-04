# =============================================================================
# run_all.R – run the whole analysis.  From R, with the working directory set to
# this project folder:
#
#   TEST <- TRUE;  source("run_all.R")   # 1) check everything runs on fake data
#   TEST <- FALSE; source("run_all.R")   # 2) run on the real Fox Insight data
#
# Only base R is needed (stats, splines). data.table is used if installed.
# =============================================================================

library(splines)
source("config.R")

if (exists("TEST") && isTRUE(TEST)) {
  source("sim/simulate_fox.R")   # regenerates the fake data each time
  cfg$data_dir <- "sim/data"; cfg$output_dir <- "outputs_TEST"
  cfg$n_boot <- 20; cfg$n_boot_med <- 20
  message("*** TEST MODE: simulated data, 20 bootstrap resamples ***")
}

t0 <- Sys.time()
source("R/00_functions.R")
source("R/01_load.R")
source("R/02_build.R")
source("R/03_descriptives.R")
source("R/04_models.R")
source("R/05_figure.R")
writeLines(c(capture.output(sessionInfo()), "", paste("Run time:", format(Sys.time() - t0))),
           file.path(cfg$output_dir, "session_info.txt"))
message("All done. Aggregate results are in ", cfg$output_dir)
