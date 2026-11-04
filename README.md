# Sleep problems and early exit from work in Parkinson's disease (Fox Insight)

Analysis code for the pre-registered plan in `analysis_plan.md`.
https://osf.io/g8t7y

## Data governance – read first

- Participant-level Fox Insight data must stay on a Manchester Met machine, in line
  with the Fox Insight Data Use Agreement. Never commit `data/` to GitHub or OSF.
- Everything written to `outputs/` is aggregate: counts, model estimates and figures.
  Counts below 10 are suppressed as `<10`. These outputs are safe to share with
  co-authors.
- Check the Fox Insight Publication Policy for the required acknowledgement wording, and
  for whether MJFF must review the manuscript before it is posted as a preprint.

## Setup

1. Download from Fox DEN one CSV per survey and put them in `data/raw/`:
   About You, Registration (Users), General, Non-Movement Experiences, Mood,
   Movement Experiences, Cognition and Daily Activities, Medications (PD),
   What's Bothering You v2.0 (PD-PROP 2.0) and Sleep Habits.
   Added in plan v0.2: Current Health, Physical Activities (PASE), Physical Experiences
   (EQ-5D-5L), Medical History, and the one-time surveys The Role of Stress in PD,
   COVID-19 Experience in the PD Community (the first survey, not Part 2),
   Understanding Fatigue, Financial and Social Impact of PD and PRD (2025) and
   PD Microbiome. These are optional: if one is missing, the analyses that need it are
   skipped and `01_file_check.csv` says so.
2. Open R in this folder. Only base R is needed. The `data.table` package speeds up loading
   if it is installed.

## Run

```r
TEST <- TRUE;  source("run_all.R")   # step 1: runs on simulated data -> outputs_TEST/
TEST <- FALSE; source("run_all.R")   # step 2: runs on the real data -> outputs/
```

Step 1 checks that the code runs on your machine before you touch the real data.

Step 2 starts by writing `outputs/01_file_check.csv`. This lists each file it matched and any
expected variables it could not find. If a file pattern matches the wrong file, or none,
set the exact file name in `config.R` (`cfg$files`).

A full run with 1,000 bootstrap resamples may take an hour or more. For a quick first look,
set `cfg$n_boot <- 50` and `cfg$n_boot_med <- 50` in `config.R`, then return them to their
pre-registered values for the final run.

## Outputs

| File | Contents |
|---|---|
| 01_file_check.csv | Files matched, rows, participants, missing variables |
| 02_flow.csv | Participant and interval flow (for the STROBE diagram) |
| 03_table1_first_interval.csv | Characteristics at first eligible interval, PD and controls |
| 03_transitions.csv | Employment transitions between consecutive reports |
| 03_crude_exit_by_exposure.csv | Crude exit rates by each sleep item |
| 04a_primary.csv | H1/H2: Models 1–3; odds ratios, standardised risks, RD/RR (bootstrap), E-values, Holm p |
| 04b_secondary.csv | H3 (reduced hours), H7 (usual activities), other sleep exposures, return to work |
| 04c_chronicity.csv | H5: never / sometimes / persistent symptoms, trend test |
| 04d_mediation.csv | H4: total, direct and indirect effects for each mediator |
| 04e_pd_vs_control.csv | H6: odds ratios by cohort and interaction test |
| 04f_effect_modification.csv | Sex, age, country, agonist, job demands, other conditions and perceived stress subgroups |
| 04g_sensitivity.csv | Sensitivity analyses S1–S10 (S9 pandemic, S10 shift work) |
| 04h_other_conditions.csv | Comparison cohort with other long-term conditions |
| 04i_validation.csv | PD-related reasons for exit; sleep apnoea / insomnia among people with daytime sleepiness |
| 04j_sleepiness_by_apnoea.csv | Sleepiness with vs without an apnoea diagnosis |
| 04k_validation_subset_model.csv | Primary model in Financial and Social Impact respondents |
| 05_forest_model2.png | Forest plot of the adjusted odds ratios |

## Structure

```
config.R            all settings (paths, variable names, windows, bootstrap)
run_all.R           runs everything
R/00_functions.R    helpers: file matching, time matching, scores, cluster-robust SEs, g-computation
R/01_load.R         reads and checks tables
R/02_build.R        builds the person-interval dataset (stays local: data/derived/)
R/03_descriptives.R
R/04_models.R
R/05_figure.R
sim/simulate_fox.R  fake data in the same format, for testing
```
