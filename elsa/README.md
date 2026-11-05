# ELSA companion study: sleep and early work exit

Code for the pre-registered OSF plan "Sleep and early work exit in long-term conditions: a companion
analysis in the English Longitudinal Study of Ageing" (same OSF project as the Fox Insight study).

## Data governance – read first

- ELSA data (UK Data Service SN 5050, End User Licence) stay on Manchester Met secure storage.
  Never commit `data/` or any `.tab` file. `.gitignore` blocks them.
- Everything in `outputs/` is aggregate. Counts below 10 are suppressed.
- Do not download ELSA data until (1) the OSF registration is approved and (2) this code has run on
  simulated data and been committed (the registered "starting rule").

## Setup

1. Download SN 5050 in **tab-delimited** format. Put these files in `data/raw/`:
   - `wave_N_elsa_data_eul*.tab` for waves 4 to 11
   - `wave_N_ifs_derived_variables.tab` and `wave_N_financial_derived_variables.tab` for waves 4, 6, 8 and 10
   - `wave_10_accelrmtry_summary_asleep_mainvars.tab`, `..._asleep_addvars.tab`, `..._bbaa_mainvars.tab`
   - all end-of-life files (`*eol*` / `*endoflife*`; not the harmonised `h_elsa_eol` file)
2. Optional packages: `data.table` (faster loading), `nnet` (H5 multinomial; ships with R),
   `mice` (multiple imputation, used only if > 5% of intervals miss a Model 2 covariate).

## Run (from this folder)

```r
setwd("elsa")
TEST <- TRUE;  source("run_all.R")   # simulated data -> outputs_TEST/ (20 bootstrap resamples)
TEST <- FALSE; source("run_all.R")   # real data      -> outputs/ (1,000 resamples)
```

`01_file_check.csv` lists each file matched and any expected variables not found. If a pattern
matches the wrong file, put the exact file name in `config.R`.

## Outputs

| File | Contents |
|---|---|
| 01_file_check.csv | Files matched, rows, missing expected variables |
| 02_flow_partA.csv / 02_flow_partB.csv | Flow of intervals and people (STROBE) |
| 03_table1.csv | Sample characteristics, overall and by insomnia frequency |
| 03_prevalence_by_condition.csv | Prevalence of sleep items by long-term condition group |
| 03_agreement_restless_vs_frequency.csv | Yes/no restless sleep vs insomnia frequency |
| 03_exit_type_by_frequency.csv | Health-related vs other exits by insomnia frequency |
| 03_neurological_descriptive.csv | Neurological conditions including Parkinson's (descriptive) |
| 03_loss_to_followup.csv | Loss to follow-up by insomnia frequency |
| 04a_primary.csv | H1, H2: odds ratios, risks, trend test, Holm p |
| 04a_primary_effects_bootstrap.csv | Risk differences, risk ratios, OR ratio (H2/H1) with bootstrap CIs; E-values |
| 04b_secondary.csv | H3, H4a-c, H6 |
| 04c_h5_multinomial.csv | H5: health-related vs other exits |
| 04d_h7_conditions.csv | H7: long-term condition interaction and groups |
| 04e_negative_control.csv | Poor hearing (negative control) |
| 04f_sensitivity.csv | S1-S8 and loss-to-follow-up weights |
| 04g_decision.csv | Registered decision rule: supported / not supported / inconclusive |
| 04h_multiple_imputation.csv | Only if > 5% missing covariates |
| 04i_model_checks.csv | Linear-trend LRT and variance inflation factors |
| 05_partB_*.csv | Part B: descriptive risks, models (if >= 50 exits), self-report vs objective |

## Implementation notes to check against the real files

- `heacd` is used for "ever diagnosed diabetes" at wave 10 (from the health-conditions lookup table);
  check its coding (1 = yes assumed).
- Month of accelerometer wear is taken from `filestarttime` (bbaa file); if it cannot be parsed, the
  wave 10 interview month is used. Months are grouped into seasons.
- Deaths between waves: anyone in an end-of-life file who was not interviewed at the next wave.
