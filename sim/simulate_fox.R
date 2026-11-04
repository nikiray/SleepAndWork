# =============================================================================
# sim/simulate_fox.R – write FAKE data in the Fox DEN format to sim/data/
# Used only to check that the pipeline runs end to end. Not real participants.
# =============================================================================

set.seed(1)
N <- 3000
ids  <- sprintf("FOX_%06d", seq_len(N))
pd   <- rbinom(N, 1, 0.65)
age0 <- runif(N, 40, 72)
day0 <- round(age0 * 365.25)
sleepy_p <- plogis(rnorm(N, -1 + 0.6 * pd, 1))
insom_p  <- plogis(rnorm(N, -0.5 + 0.4 * pd, 1))
agon     <- rbinom(N, 1, 0.35) * pd

about <- nms <- mood <- move <- daily <- meds <- pp <- sl <- pase <- curr <- eq <- vector("list", N)
conds <- c("Arth", "Back", "Blood", "Cancer", "Diabetes", "Gastric", "HBP", "Heart", "Kidney", "Liver",
           "Lung", "Depress", "Anxiety")
has_cond <- matrix(rbinom(N * length(conds), 1, 0.12), nrow = N, dimnames = list(NULL, conds))
jobdem <- sample(1:4, N, TRUE, prob = c(.45, .3, .17, .08))
gds_items <- c("MoodAfraid", "MoodAlive", "MoodBetter", "MoodBored", "MoodEmpty", "MoodEnergy",
               "MoodHappy", "MoodHelp", "MoodHome", "MoodHopeless", "MoodInterest", "MoodMemory",
               "MoodSatis", "MoodSpirits", "MoodWorth")
positive <- c("MoodAlive", "MoodEnergy", "MoodHappy", "MoodSatis", "MoodSpirits")
updrs <- c("MoveSpeech", "MoveSaliva", "MoveChew", "MoveEat", "MoveDress", "MoveHygiene",
           "MoveWrite", "MoveHobby", "MoveSleep", "MoveTremor", "MoveUp", "MoveWalk", "MoveFreeze")
pdaq <- c("DailyClock", "DailyDiscuss", "DailyErrand", "DailyExplain", "DailyFinance",
          "DailyInstruct", "DailyLearn", "DailyMany", "DailyMap", "DailyMoney", "DailyNumber",
          "DailyProblem", "DailyRead", "DailyRememb", "DailyThought")
agvars <- c("MedsCurrPDSifro", "MedsCurrPDMiraER", "MedsCurrPDRequi", "MedsCurrPDRequiXL",
            "MedsCurrPDReAd", "MedsCurrPDHaru", "MedsCurrPDNeupr", "MedsCurrPDParlo",
            "MedsCurrPDApok", "MedsCurrPDKynm")

for (i in seq_len(N)) {
  nyr  <- sample(2:6, 1)
  days <- day0[i] + c(0, cumsum(round(rnorm(nyr - 1, 365, 50))))
  ages <- age0[i] + (days - day0[i]) / 365.25
  emp  <- integer(nyr)
  emp[1] <- if (ages[1] >= 65) sample(1:3, 1, prob = c(.15, .15, .7)) else sample(1:4, 1, prob = c(.55, .15, .2, .1))
  for (k in seq_len(nyr)[-1]) {
    if (emp[k - 1] %in% 1:2) {
      h <- plogis(-3 + 1.5 * sleepy_p[i] + 0.6 * insom_p[i] + 0.06 * (ages[k] - 55) + 0.3 * pd[i])
      emp[k] <- if (runif(1) < h) sample(3:4, 1, prob = c(.75, .25)) else
                if (emp[k - 1] == 1 && runif(1) < 0.08) 2 else emp[k - 1]
    } else emp[k] <- if (runif(1) < 0.05) 2 else emp[k - 1]
  }
  if (runif(1) < 0.03) emp[sample(nyr, 1)] <- 5
  about[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = days, Employment = emp,
                           Age = round(ages, 1), Education = sample(1:9, 1, prob = c(1, 8, 10, 6, 25, 25, 8, 10, 1)),
                           Income = sample(1:7, 1), SexAtBirth = sample(1:2, 1))
  q  <- seq(days[1] + sample(-10:10, 1), max(days) + 90, by = 91); nq <- length(q)
  nms[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = q,
    NonMoveAwake = rbinom(nq, 1, sleepy_p[i]), NonMoveSleep = rbinom(nq, 1, insom_p[i]),
    NonMoveSens = rbinom(nq, 1, .2), NonMoveUrinePM = rbinom(nq, 1, .4),
    NonMoveTalk = rbinom(nq, 1, .15 + .2 * pd[i]), NonMoveAnxious = rbinom(nq, 1, .2 + .3 * insom_p[i]),
    NonMoveConcent = rbinom(nq, 1, .2 + .3 * sleepy_p[i]), NonMoveSmell = rbinom(nq, 1, .2 + .4 * pd[i]))
  q2 <- q[seq(1, nq, by = 2)]; n2 <- length(q2)
  m <- sapply(gds_items, function(v) rbinom(n2, 1, if (v %in% positive) .8 - .3 * sleepy_p[i] else .15 + .2 * insom_p[i]))
  m <- matrix(m, nrow = n2, dimnames = list(NULL, gds_items)); m[sample(length(m), 1)] <- 3
  mood[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = q2, m)
  mv <- matrix(sample(0:4, n2 * 13, TRUE, prob = if (pd[i]) c(.4, .3, .2, .07, .03) else c(.85, .1, .03, .01, .01)),
               nrow = n2, dimnames = list(NULL, updrs))
  move[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = q2, mv)
  dl <- matrix(sample(0:5, n2 * 15, TRUE, prob = c(.01, .03, .06, .2, .68, .02)), nrow = n2, dimnames = list(NULL, pdaq))
  daily[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = q2 + 30, dl)
  yr <- seq(days[1] + 30, max(days), by = 365)
  sl[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = yr, Sleep = rbinom(length(yr), 1, .15 + .25 * pd[i]))
  pase[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = yr + 10, Work = rbinom(length(yr), 1, .6),
                          WorkActive = jobdem[i])
  cm <- matrix(rep(has_cond[i, ], each = length(yr)), nrow = length(yr), dimnames = list(NULL, paste0("Curr", conds)))
  lm <- cm * matrix(rbinom(length(cm), 1, .5), nrow = length(yr)); colnames(lm) <- paste0("Curr", conds, "Lim")
  lm[cm == 0] <- NA
  curr[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = yr + 20, cm, lm)
  eq[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = q2 + 5,
                        Active = sample(0:4, n2, TRUE, prob = c(.5 - .3 * sleepy_p[i], .3, .1 + .2 * sleepy_p[i], .07, .03)))
  if (pd[i] == 1) {
    a <- matrix(0, nrow = nq, ncol = length(agvars), dimnames = list(NULL, agvars))
    if (agon[i]) a[, sample(agvars[1:7], 1)] <- 1
    meds[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = q + 30, a, MedsCurrPDNone = 0)
    fat <- ifelse(runif(nq) < .2 + .4 * sleepy_p[i], 1, NA); slp <- ifelse(runif(nq) < .3 * insom_p[i], 1, NA)
    pp[[i]] <- data.frame(fox_insight_id = ids[i], days_elapsed = q + 30,
                          Pdprop2Fatig_1 = fat, Pdprop2Fatig_2 = NA, Pdprop2Fatig_3 = NA, Pdprop2Fatig_4 = NA, Pdprop2Fatig_5 = NA,
                          Pdprop2Slp_1 = NA, Pdprop2Slp_2 = slp, Pdprop2Slp_3 = NA, Pdprop2Slp_4 = NA, Pdprop2Slp_5 = NA,
                          PdpropSev_1 = sample(0:3, nq, TRUE), PdpropSev_2 = sample(0:3, nq, TRUE))
  }
}

users <- data.frame(fox_insight_id = ids, days_elapsed = day0, InitPDDiag = pd,
                    YearsWithPD = ifelse(pd == 1, sample(0:2, N, TRUE), NA),
                    LocCountry = sample(c("United States", "United Kingdom", "Canada"), N, TRUE, prob = c(.8, .1, .1)))
general <- data.frame(fox_insight_id = ids, days_elapsed = day0 + 2000,
                      CurrPDDiag = ifelse(runif(N) < .01, 1 - pd, pd))

# one-time / once-only tables
pick <- function(p) which(runif(N) < p)
mid_day <- function(k) day0[k] + sample(200:1500, length(k), TRUE)
k <- pick(.4); medhis <- data.frame(fox_insight_id = ids[k], days_elapsed = day0[k] + 30,
  SleepAp = rbinom(length(k), 1, .15), Insom = rbinom(length(k), 1, .2))
k <- which(pd == 1 & runif(N) < .5)
mind <- data.frame(fox_insight_id = ids[k], days_elapsed = mid_day(k),
  matrix(sample(1:5, length(k) * 10, TRUE), ncol = 10, dimnames = list(NULL,
    c("MindStressFeelUnexpect", "MindStressFeelControl", "MindStressFeelNervous", "MindStressFeelConfident",
      "MindStressFeelYourWay", "MindStressFeelCope", "MindStressFeelIrritate", "MindStressFeelOnTop",
      "MindStressFeelAngry", "MindStressFeelPiled"))))
k <- pick(.5); cov <- data.frame(fox_insight_id = ids[k], days_elapsed = mid_day(k),
  COVSocDisWork = sample(1:9, length(k), TRUE, prob = c(.3, .02, .02, .1, .06, .05, .25, .15, .05)))
k <- which(pd == 1 & runif(N) < .3); fatigue <- data.frame(fox_insight_id = ids[k], days_elapsed = mid_day(k),
  FatigueFrequency = sample(1:7, length(k), TRUE))
k <- pick(.2); fncl <- data.frame(fox_insight_id = ids[k], days_elapsed = mid_day(k) + 1000,
  FnclImpJobImpEarlyRetrmnt = rbinom(length(k), 1, .3), FnclImpJobImpWrkFwrHrs = rbinom(length(k), 1, .3))
k <- pick(.2); pdmb <- data.frame(fox_insight_id = ids[k], days_elapsed = mid_day(k),
  PDMBWorkNight = sample(c(0, 1, 999), length(k), TRUE, prob = c(.8, .1, .1)))

dir.create("sim/data", showWarnings = FALSE, recursive = TRUE)
w <- function(x, f) utils::write.csv(x, file.path("sim/data", f), row.names = FALSE, na = "")
w(do.call(rbind, about), "About.csv"); w(users, "Users.csv"); w(general, "General.csv")
w(do.call(rbind, nms), "NonMove.csv"); w(do.call(rbind, mood), "Mood.csv")
w(do.call(rbind, move), "Move.csv"); w(do.call(rbind, daily), "Daily.csv")
w(do.call(rbind, meds), "MedsCurrPD.csv"); w(do.call(rbind, pp), "Pdprop2.csv")
w(do.call(rbind, sl), "Sleep.csv")
w(do.call(rbind, pase), "PASE.csv"); w(do.call(rbind, curr), "Curr.csv")
w(do.call(rbind, eq), "PhysicalExperiences.csv"); w(medhis, "MedHis.csv"); w(mind, "Mind.csv")
w(cov, "COV.csv"); w(fatigue, "Fatigue.csv"); w(fncl, "FnclImp.csv"); w(pdmb, "PDMB.csv")
message("Simulated Fox-format data written to sim/data/")
