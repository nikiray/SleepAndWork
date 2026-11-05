# =============================================================================
# run_all.R – ELSA companion study (pre-registered; see OSF)
#   TEST <- TRUE;  source("run_all.R")   # step 1: simulated data -> outputs_TEST/
#   TEST <- FALSE; source("run_all.R")   # step 2: real ELSA data  -> outputs/
# Run from the elsa/ folder (setwd to it first).
# =============================================================================

library(splines)
source("config.R")
if (exists("TEST") && isTRUE(TEST)) {
  source("sim/simulate_elsa.R")
  cfg$data_dir <- "sim/data"; cfg$output_dir <- "outputs_TEST"; cfg$n_boot <- 20; cfg$n_imp <- 3
}
source("R/00_functions.R")
source("R/01_load.R")
source("R/02_build.R")
source("R/03_descriptives.R")
source("R/04_models.R")
source("R/05_partB.R")
message("All done. Outputs are in ", cfg$output_dir)
