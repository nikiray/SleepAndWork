---
title: "Sleep problems and early exit from work in Parkinson's disease: pre-registered analysis plan"
subtitle: "Secondary analysis of the Fox Insight longitudinal cohort"
author: "Nicola Ray (Manchester Metropolitan University) [add co-authors]"
date: "DRAFT v0.2 – 4 November 2026"
---

# 1. Status of the data at registration

This is a pre-registration of a secondary analysis of existing data. Access to Fox Insight was granted through Fox DEN on [date]. At the time of registration, the authors have read the data dictionary (version 2.1, July 2025) and viewed the overall frequency of each employment category displayed in Fox DEN. We have **not** downloaded the data, linked any exposure to any outcome, or run any of the analyses described here. [Edit if this changes before posting.]

The plan will be posted on the Open Science Framework (OSF) before the outcome data are analysed. The analysis code is version-controlled and will be released alongside the paper. Any deviation from this plan will be reported and justified in the paper.

# 2. Background and rationale

Parkinson's disease (PD) often begins during working life, and people with PD leave paid work years earlier than people without it. People who have left work frequently name fatigue and poor sleep among their reasons. However, most studies are cross-sectional and rely on people recalling why they left. Prospective studies suggest that depression, anxiety and psychiatric distress predict leaving work, while motor severity does not.

Sleep problems are common in PD from the earliest stages. They are also potentially modifiable through treatment, medication review and workplace adjustments. In early PD, poor sleep quality predicts later increases in fatigue and depression. Nobody has yet tested, in a large longitudinal cohort, whether sleep problems come *before* leaving work in PD, or whether fatigue, mood and concentration carry any such effect.

# 3. Research questions and hypotheses

**Primary research questions (two co-primary exposures)**

- **H1.** Among working-age, employed people with PD, reporting difficulty staying awake during daytime activities (excessive daytime sleepiness) is associated with a higher risk of leaving paid work over the following year.
- **H2.** Among working-age, employed people with PD, reporting difficulty getting to sleep or staying asleep (insomnia symptoms) is associated with a higher risk of leaving paid work over the following year.

**Secondary questions**

- **H3.** Each sleep problem is associated with a higher risk of moving from full-time to part-time work.
- **H4.** Part of the association between each sleep problem and leaving work is explained by (mediated through) fatigue, depressive symptoms, anxiety and difficulty concentrating.
- **H5.** Sleep problems that persist over the preceding year are more strongly associated with leaving work than single reports (a dose-response relationship).
- **H6.** The associations differ between people with PD and people without PD in the same cohort. We make no directional prediction.
- **H7.** Each sleep problem is associated with a higher risk of developing moderate or worse problems with usual activities, including work (EQ-5D-5L). This captures difficulty at work before someone actually leaves.

**Exploratory questions**

- Associations of other sleep features with leaving work: restless legs symptoms, getting up at night to pass urine, REM sleep behaviour disorder symptoms, and sleep problems named among a person's most bothersome PD problems.
- Whether sleep problems predict returning to work among people of working age who are not working.
- Whether associations differ by sex, age group (under 50 vs 50–64), country (USA vs elsewhere), dopamine agonist use, how physically demanding the job is, number of other long-term conditions, and perceived stress.
- **Other long-term conditions.** Whether sleep problems predict leaving work among participants *without* PD who report another long-term condition (diabetes, heart disease, lung disease, arthritis or back problems, depression or anxiety). This tests whether the pathway is shared across conditions.
- **Sources of sleepiness.** What proportion of people reporting daytime sleepiness also report a diagnosis of sleep apnoea or insomnia, and whether the association with leaving work is similar with and without such a diagnosis.

# 4. Data source

Fox Insight is an online longitudinal study of people with and without PD, run by the Michael J. Fox Foundation. Participants complete core questionnaires repeatedly on a fixed schedule. The data are released as one table per questionnaire in long format, identified by `fox_insight_id`. Time is given as `days_elapsed` since a randomised participant-specific reference date. The measures used here, with their schedules, are:

| Questionnaire (table) | Content used | Schedule |
|---|---|---|
| About You (`About`) | Employment status, age, sex, education, income | Registration, then confirmed or updated annually ("summary mode") |
| Registration (`Users`) | Baseline PD diagnosis, years with PD at enrolment, country | Once |
| General (`General`) | Most recent PD diagnosis status | Derived |
| Non-Movement Experiences, NMS-Quest (`NonMove`) | Sleep, sleepiness and other non-motor symptoms, past month (yes/no) | Every 3 months |
| Your Mood, GDS-15 (`Mood`) | Depressive symptoms; energy item | Repeated |
| Movement Experiences, MDS-UPDRS Part II (`Move`) | Motor experiences of daily living | Every 6 months |
| Cognition and Daily Activities, PDAQ-15 (`Daily`) | Cognitive function | Every 6 months |
| Medications (PD) (`MedsCurrPD`) | Dopamine agonist use | Every 3 months (PD only) |
| What's Bothering You?, PD-PROP 2.0 (`Pdprop2`) | Fatigue and sleep among the five most bothersome problems, with severity | Every 3 months (PD only) |
| Your Sleep Habits, RBD1Q (`Sleep`) | REM sleep behaviour disorder screen | Annual |
| Your Current Health (`Curr`) | Current long-term conditions and whether each limits activities | Baseline, 3 months, then annual |
| Your Physical Activities, PASE (`PASE`) | Physical demands of the job (`WorkActive`); paid or volunteer work in the past 7 days (`Work`) | 1 and 3 months, then annual |
| Your Physical Experiences, EQ-5D-5L | Problems with usual activities, including work (`Active`) | Every 6 months |
| Your Medical History (`MedHis`) | Diagnosed sleep apnoea and insomnia | Once; only for participants enrolled before October 2017 |

We also use these one-time surveys. Each participant completed them once, during known calendar windows:

| One-time survey | Content used | Fielded | Respondents |
|---|---|---|---|
| The Role of Stress in PD (`Mind`) | Perceived Stress Scale (PSS-10) | Aug–Sep 2019 | 9,387 (PD) |
| COVID-19 Experience in the PD Community (`COV`) | Effect of the pandemic on work, including being laid off (`COVSocDisWork`) | Apr 2020–Mar 2021 | 9,146 |
| Financial and Social Impact of PD and Related Disorders (`FnclImp`) | Whether the person ever stopped working, retired early or cut their hours *for reasons related to PD* | From Apr 2025 | about 1,540 |
| PD Microbiome Survey (`PDMB`) | Night or shift work | Feb–Dec 2021 | Subset |
| Understanding Fatigue in PD; Impact of Thinking, Mood and Motor Symptoms | Physical vs mental fatigue | 2019; 2023 | 1,647; 3,502 |
| Environmental Exposure: Occupation | Job types and industries | Oct 2017–Mar 2019 | 3,746 |

# 5. Study design and sample

## 5.1 Unit of analysis: person-intervals

Each participant's About You responses are ordered by `days_elapsed`. Each pair of consecutive responses forms an **interval** from time *t* (start) to time *t1* (end). An interval is eligible if, at *t*, the participant:

1. reports being employed full-time or part-time;
2. is aged 18–64 (working age); and
3. has a valid employment response at *t1*, between 180 and 548 days later (about 6–18 months).

"Prefer not to answer" is treated as missing. A person can contribute several intervals. Once someone has left work, they re-enter the risk set only if they later report working again.

## 5.2 Cohorts

- **PD cohort (primary):** reported a PD diagnosis at registration (`InitPDDiag` = 1) and still reports one at their most recent visit (`CurrPDDiag` = 1).
- **Comparison cohort (H6):** reported no PD diagnosis at registration and at their most recent visit.
- People whose diagnosis status changed during the study are excluded from both cohorts.

# 6. Variables

## 6.1 Outcomes (measured at *t1*)

- **Primary – leaving work:** employed at *t* and retired or unemployed at *t1*.
- **Secondary – reduced hours:** full-time at *t* and part-time at *t1*. This is analysed only in intervals that start full-time. Intervals that end in leaving work are excluded from this analysis, because leaving work is a competing event.
- **Secondary – difficulty with usual activities (H7):** EQ-5D-5L `Active` of 2 or more (moderate problems, severe problems or unable) in the response closest to *t1* (within 120 days). This is analysed only in intervals where the response at *t* was 0 or 1 (no or slight problems). It is measured every 6 months, so it is less affected by the annual reporting of employment.
- **Exploratory – return to work:** retired or unemployed at *t* (aged under 65) and employed at *t1*.

**Pandemic-related job loss.** Exits from work among participants who reported in the COVID-19 survey that they were laid off during the pandemic are flagged. They are excluded in a sensitivity analysis (Section 7.5).

## 6.2 Exposures (measured at the start of the interval)

All exposures are taken from the NMS-Quest response closest to *t*, within 120 days before to 30 days after *t*. When two responses are equally close, the earlier one is used.

- **Co-primary 1 – daytime sleepiness:** `NonMoveAwake`, "finding it difficult to stay awake during activities such as working, driving or eating" in the last month.
- **Co-primary 2 – insomnia symptoms:** `NonMoveSleep`, "difficulty getting to sleep at night or staying asleep at night" in the last month.
- **Chronicity (H5):** the proportion of NMS-Quest responses in the 365 days up to *t* that report the symptom. At least two responses are required. Responses are grouped as 0%, more than 0% but under 50%, and 50% or more.
- **Exploratory:**
  - restless legs symptoms (`NonMoveSens`)
  - getting up at night to pass urine (`NonMoveUrinePM`)
  - acting out dreams (`NonMoveTalk`, plus the RBD1Q `Sleep` item)
  - any sleep problem among the five most bothersome PD-PROP 2.0 problems (`Pdprop2Slp_1`–`_5`)

Our question is whether sleep problems predict leaving work, **whatever their cause**. Diagnosed sleep apnoea and insomnia are possible sources of the exposure, not confounders, so we do **not** exclude or adjust for them. We report them descriptively and as a secondary breakdown (Section 7.6).

## 6.3 Mediators (H4)

Mediators are measured during the interval. We use the response closest to the interval midpoint, taken between 30 days after *t* and 60 days before *t1*, so that the mediator is measured after the exposure and before the outcome is reported.

- **Fatigue (PD-PROP 2.0):** fatigue among the five most bothersome problems (any of `Pdprop2Fatig_1`–`_5`). PD cohort only.
- **Low energy:** a "No" answer to the GDS item `MoodEnergy`.
- **Depressive symptoms:** a score of 5 or more on the GDS with the energy item removed (14 items), so that it does not overlap with the fatigue measures.
- **Anxiety:** `NonMoveAnxious`.
- **Difficulty concentrating:** `NonMoveConcent`.

## 6.4 Covariates (measured at *t*)

These were chosen using a causal diagram (Appendix A) as likely common causes of sleep problems and leaving work. Measures other than About You are matched within 120 days before to 30 days after *t*.

- **Model 1 (minimal):**
  - age (natural spline, 3 degrees of freedom)
  - sex (sex at birth, or the earlier sex item)
  - education (secondary school or some college / associate or bachelor's degree / postgraduate)
  - household income (under $50k / $50–99k / $100k or more / missing as a separate category)
  - country (USA vs elsewhere)
  - years with PD at enrolment (under 3 / 3–10 / 11 or more)
  - years since enrolment
  - interval length
- **Model 2 (primary) – Model 1 plus:**
  - MDS-UPDRS Part II total
  - PDAQ-15 total
  - current dopamine agonist use (pramipexole, ropinirole, rotigotine, bromocriptine or apomorphine)
  - physical demands of the job (PASE `WorkActive`: mainly sitting / sitting or standing with some walking / walking with some handling / heavy manual work, with a missing category), from the response closest to *t* within the previous 400 days
  - number of other current physical long-term conditions that limit activities (0 / 1 / 2 or more, from `Curr*Lim`), from the response closest to *t* within the previous 400 days. The conditions are arthritis, back problems, blood disorders, cancer, diabetes, gastric conditions, high blood pressure, heart disease, kidney disease, liver disease and lung disease. Depression and anxiety are left out of this count because mood is handled in Model 3.
- **Model 3 – Model 2 plus:**
  - depressive symptoms (GDS-14) at *t*
  - anxiety (`NonMoveAnxious`) at *t*

  This tests whether the associations are independent of mood at the time of exposure.

Model 2 is the primary model. Model 3 risks adjusting away part of the effect, because mood can lie on the pathway from sleep to work exit as well as cause sleep problems. We therefore report it alongside Model 2 rather than as the main estimate.

# 7. Statistical analysis

## 7.1 Main models

- **Discrete-time survival models.** We use logistic regression of the outcome on the person-interval data. This estimates the odds of leaving work over an interval, given that the person was working at its start.
- **Clustering.** Standard errors are cluster-robust by participant.
- **Absolute and relative risks.** Using marginal standardisation (g-computation), we report predicted probabilities of leaving work with and without the exposure, the risk difference and the risk ratio.
- **Confidence intervals and multiple testing.** Confidence intervals for g-computation estimates come from 1,000 cluster bootstrap resamples. The two co-primary hypotheses use Holm-corrected p-values at α = 0.05. All other analyses are interpreted through effect sizes and 95% confidence intervals, without formal correction.

## 7.2 Mediation (H4)

- **Method.** We use counterfactual mediation with a separate model for each mediator. A logistic mediator model includes the exposure and Model 2 covariates. A logistic outcome model includes the exposure, mediator, their interaction and Model 2 covariates.
- **Effects estimated.** Natural direct and indirect effects on the risk-difference scale, and the proportion mediated, are estimated by g-computation with cluster bootstrap confidence intervals.
- **Interpretation.** Mood and fatigue may themselves be affected by the exposure and then influence other mediators. We therefore interpret these estimates cautiously, as interventional effects.

## 7.3 Effect modification and cohort comparison (H6, exploratory)

- **Cohort comparison.** Pooled models include an exposure-by-cohort interaction. Covariates are Model 2 without dopamine agonist use and years with PD, because these apply only to the PD cohort.
- **Effect modification.** Exposure-by-modifier interactions are tested within the PD cohort, using cluster-robust Wald tests. The modifiers are:
  - sex
  - age group
  - country
  - dopamine agonist use
  - job physical demands (mainly sitting vs more active)
  - other long-term conditions that limit activities (none vs any)
  - perceived stress (PSS-10 at or above vs below the sample median). This uses only intervals starting after the participant completed the 2019 stress survey. Items are scored 0–4, and the four positively worded items (confident, things going your way, controlling irritations, on top of things) are reversed, giving a total of 0–40.
- **Other long-term conditions (exploratory).** Among comparison-cohort participants who report at least one current long-term condition, we fit Model 2 without the PD-specific covariates. We report results for each condition group where there are at least 50 exit events.

## 7.4 Missing data

- **Primary analysis.** Complete cases (intervals with the exposure, the outcome and all Model 2 covariates).
- **Sensitivity for loss to follow-up.** We weight by the inverse probability of having an eligible next employment response, estimated from Model 2 covariates and the exposure. Weights are stabilised and truncated at the 1st and 99th percentiles.

## 7.5 Sensitivity analyses (pre-specified)

1. Upper age limit of 62 and of 67 instead of 65.
2. Leaving work defined as retirement only, and as unemployment only.
3. A tighter interval window of 270–450 days.
4. The first eligible interval per person only.
5. Excluding people taking dopamine agonists.
6. Mutual adjustment of the two co-primary exposures.
7. **Negative-control exposure:** loss or change of smell or taste (`NonMoveSmell`). This is a marker of PD that should have no direct effect on work. A similar association would suggest residual confounding by disease severity.
8. **Unmeasured confounding:** E-values for the primary risk ratios.
9. **COVID-19 pandemic:**
   - (a) excluding exits flagged as pandemic lay-offs;
   - (b) excluding intervals that overlap March 2020 to December 2021, among participants whose timeline can be anchored to calendar time (see 7.7).
10. **Shift work:** adjusting for night or shift work, in the subset who completed the PD Microbiome Survey.

## 7.6 Validation and descriptive analyses

- **Were exits related to PD?** Among participants who completed the 2025 Financial and Social Impact survey, we report the proportion of work exits in our data that the participant attributes to PD (stopping work, early retirement or fewer hours "for reasons related to your PD"). We also report whether the association between sleep and leaving work is similar in this subset. This checks how far working-age "retirement" in Fox Insight reflects PD-related early exit.
- **Sources of daytime sleepiness.** Among participants with Medical History data (enrolled before October 2017), we report the proportion of those with daytime sleepiness who also report diagnosed sleep apnoea or insomnia. Where numbers allow, we report the association with leaving work separately for people with and without these diagnoses. This describes where sleep problems come from; it is not a correction for them.

## 7.7 Approximate calendar time

Fox Insight does not release calendar dates. However, one-time surveys were fielded during known windows. For participants who completed a survey with a narrow window, the `days_elapsed` value at completion is mapped to the midpoint of that window. The narrowest window is the Role of Stress survey (August–September 2019). For others, we use the narrowest window available. This gives an approximate calendar date for each of that participant's other responses, with uncertainty of about half the window width. We use it only to flag intervals that certainly or possibly overlap the pandemic (sensitivity analysis 9b).

## 7.8 Reporting

We will report a participant flow diagram and follow STROBE guidelines. All released outputs are aggregated. Cells with fewer than 10 participants are suppressed, in line with the Fox Insight data use agreement and publication policy.

# 8. Known limitations (stated in advance)

- **Employment timing.** Employment is collected annually in "summary mode", so changes may be reported late or missed. The exact date of leaving work, and the reason for it, are not recorded. Restricting to working age is how we separate early exit from planned retirement.
- **Measures.** Sleep exposures are single yes/no items covering the past month, and fatigue is not measured directly in the core questionnaires. Job physical demands and comorbidities are self-reported and measured roughly annually. Sleep diagnoses, perceived stress, shift work and reasons for leaving work are available only for participants who completed specific questionnaires.
- **Calendar time.** Calendar dates are not released. We approximate them only for participants who completed a one-time survey with a known window, and we use this only to handle the pandemic period.
- **Sample.** Fox Insight participants are volunteers who take part online. They are mostly from the USA, highly educated and relatively early in their disease, which limits how far the results generalise.
- **Causation.** This is an observational study. The design relies on timing, adjustment, negative controls and sensitivity analyses rather than establishing causation.

# Appendix A. Causal diagram (summary)

The assumed causal structure is:

- **Common causes of sleep and work exit:** PD severity and duration (motor and cognitive), other long-term conditions, age, sex, socioeconomic position, country, the physical demands of the job and dopamine agonist treatment.
- **Sources of sleep problems (not adjusted for):** sleep apnoea, insomnia and other causes lead to the exposure. Our question concerns sleep problems whatever their cause.
- **Exposure:** sleep problems at *t*.
- **Pathways from sleep to leaving work, during the interval:** fatigue, low energy, depressive symptoms, anxiety and difficulty concentrating.
- **Outcome:** leaving work by *t1*.
- **Mood at *t*:** treated as a possible common cause, which is why it is in Model 3.

![Causal diagram. Grey: confounders measured at the start of the interval. Dashed: mood at the start of the interval, adjusted for in Model 3 only. Blue: exposure and its direct path to the outcome. Gold: mediators measured during the interval, and the indirect path.](causal_diagram.png){width=100%}

# Appendix B. Decision rules

- The primary analysis proceeds if there are at least 100 work-exit events among eligible PD intervals.
- If there are fewer, intervals of 180–730 days will be used. This change will be reported as a deviation from the plan.
