# ---- Package/Library Install/Load ----
install.packages("pacman")
library("pacman")
pacman::p_load(jmv, tidyverse, psych, readxl, writexl,
               skimr, readxl, dplyr, psych, expss, 
               ggpubr, magrittr, gamlj)


# ---- Load Final Data ----
library(readxl)
Final_Data <- read_excel("Desktop/PSY 5050 ADV QUANT/Copy of Final_Data.xlsx")
View(Final_Data)

# --------- Q1: Dimension Reduction ----------
## Descriptives -----
jmv::descriptives(
  data = Final_Data,
  vars = vars(GRIT_1, 
              GRIT_2, 
              GRIT_3, 
              GRIT_4, 
              GRIT_5, 
              GRIT_6, 
              GRIT_7, 
              GRIT_8, 
              GRIT_9, 
              GRIT_10),
  hist = TRUE,
  n = FALSE,
  missing = FALSE)

## ----- Correlations -----
jmv::corrMatrix(
  data = Final_Data,
  vars = vars(GRIT_1, 
              GRIT_2, 
              GRIT_3, 
              GRIT_4, 
              GRIT_5, 
              GRIT_6, 
              GRIT_7, 
              GRIT_8, 
              GRIT_9, 
              GRIT_10),
  flag = TRUE)
### Meet criteria (.3 < r < .8): All

## ---- EFA for Grit Measure -------
jmv::efa(
  data = Final_Data,
  vars = vars(GRIT_1, 
              GRIT_2, 
              GRIT_3, 
              GRIT_4, 
              GRIT_5, 
              GRIT_6, 
              GRIT_7, 
              GRIT_8, 
              GRIT_9, 
              GRIT_10),
  extraction = "pa",
  hideLoadings = 0.4,
  sortLoadings = TRUE,
  screePlot = TRUE,
  factorCor = TRUE,
  factorSummary = TRUE,
  kmo = TRUE,
  bartlett = TRUE)
## 1- items 2,4,6,8,10
## 2 - items 3,7,9
## 3 - items 1,5

## ----- Create Aggregate Columns for Factors -------
### Factor 1 ---
"Grit_Factor1" <- Final_Data$GRIT_2 + 
  Final_Data$GRIT_4 +
  Final_Data$GRIT_6 +
  Final_Data$GRIT_8 +
  Final_Data$GRIT_10
Final_Data['Grit_Factor1'] <- 
  c(Grit_Factor1)
head(Final_Data)
### Check Column Names
colnames(Final_Data)
### Recheck Descriptives and Visuals
psych::describe(Final_Data$Grit_Factor1)

### Factor 2 ---
"Grit_Factor2" <- Final_Data$GRIT_3 + 
  Final_Data$GRIT_7 +
  Final_Data$GRIT_9
Final_Data['Grit_Factor2'] <- 
  c(Grit_Factor2)
head(Final_Data)
### Check Column Names
colnames(Final_Data)
### Recheck Descriptives and Visuals
psych::describe(Final_Data$Grit_Factor2)

### Factor 3 ---
"Grit_Factor3" <- Final_Data$GRIT_1 + 
  Final_Data$GRIT_5
Final_Data['Grit_Factor3'] <- 
  c(Grit_Factor3)
head(Final_Data)
### Check Column Names
colnames(Final_Data)
### Recheck Descriptives and Visuals
psych::describe(Final_Data$Grit_Factor3)

# ----------- Regression ----------
## Recodes ------------

#### Gender ---
table(Final_Data$Gender, useNA = 'always')
Final_Data_r <- Final_Data %>%
  dplyr::mutate(Gender_r =
                  ifelse(Gender == "Male", 0,
                         ifelse(Gender == "Female", 1,
                                Gender)))
### Check Recodes
table(Final_Data_r$Gender_r, useNA = 'always')
colnames(Final_Data_r)
skimr:: skim(Final_Data_r$Gender_r)
View(Final_Data_r)

#### First_Gen ---
table(Final_Data_r$First_Gen, useNA = 'always')
Final_Data_r_r<- Final_Data_r %>%
  dplyr::mutate(First_Gen_r =
                  ifelse(First_Gen == "No", 0,
                         ifelse(First_Gen == "Yes", 1,
                                First_Gen)))
### Check Recodes
table(Final_Data_r_r$First_Gen_r, useNA = 'always')
colnames(Final_Data_r_r)
skimr:: skim(Final_Data_r_r$First_Gen_r)
View(Final_Data_r_r)

#### Intent for College Enrollment---
table(Final_Data_r_r$College, useNA = 'always')
Final_Data_R <- Final_Data_r_r %>%
  dplyr::mutate(College_r =
                  ifelse(College == "I dream of going to college, but it's not going to happen for me", 0,
                         ifelse(College == "I know I am going to attend", 1,
                                ifelse(College == "I think it is an option, but I am not sure", NA,
                                       College))))
### Check Recodes
table(Final_Data_R$College_r, useNA = 'always')
colnames(Final_Data_R)
skimr:: skim(Final_Data_R$College_r)
View(Final_Data_R)
### Don't think I'll need this variable, but have the recode if needed

#### Math Level ---
table(Final_Data_R$Math_Level, useNA = 'always')
Final_Data_R_R <- Final_Data_R %>%
  dplyr::mutate(Math_Level_r =
                  ifelse(Math_Level == 1, 0,
                         ifelse(Math_Level == 2, 0,
                                ifelse(Math_Level == 3, 0,
                                       ifelse(Math_Level == 4, 1,
                                              ifelse(Math_Level == 5, 1,
                                                     Math_Level)))))) ## 1 is pass, 0 is fail
### Check Recodes
table(Final_Data_R_R$Math_Level_r, useNA = 'always')
colnames(Final_Data_R_R)
skimr:: skim(Final_Data_R_R$Math_Level_r)
View(Final_Data_R_R)

### English Level ---
table(Final_Data_R$English_Level, useNA = 'always')
Final_Data_q <- Final_Data_R_R %>%
  dplyr::mutate(English_Level_r =
                  ifelse(English_Level == 1, 0,
                         ifelse(English_Level == 2, 0,
                                ifelse(English_Level == 3, 0,
                                       ifelse(English_Level == 4, 1,
                                              ifelse(English_Level == 5, 1,
                                                     English_Level)))))) ## 1 is pass, 0 is fail
### Check Recodes
table(Final_Data_q$English_Level_r, useNA = 'always')
colnames(Final_Data_q)
skimr:: skim(Final_Data_q$English_Level_r)
View(Final_Data_q)

### Add Aggegrate Variable Columns ----

#### GMS --- 
"GMS_Mean" <- Final_Data_q$GMS_1 + 
  Final_Data_q$GMS_2 +
  Final_Data_q$GMS_3 +
  Final_Data_q$GMS_4
Final_Data_q['GMS_Mean'] <- 
  c(GMS_Mean)
head(Final_Data_q)
## Check Column Names
colnames(Final_Data_q)
## Recheck Descriptives and Visuals
psych::describe(Final_Data_q$GMS_Mean)

#### FMS ---
"FMS_Mean" <- Final_Data_q$FMS_1 + 
  Final_Data_q$FMS_2 +
  Final_Data_q$FMS_3
Final_Data_q['FMS_Mean'] <- 
  c(FMS_Mean)
head(Final_Data_q)
## Check Column Names
colnames(Final_Data_q)
## Recheck Descriptives and Visuals
psych::describe(Final_Data_q$FMS_Mean)


## Descriptives ---------

### For Predictors ---
jmv::descriptives(
  data = Final_Data_q,
  vars = vars(GPA, 
              Gender_r, 
              GMS_Mean, 
              FMS_Mean, 
              Grit_Factor1, 
              Grit_Factor2, 
              Grit_Factor3, 
              First_Gen_r),
  hist = TRUE,
  boxLabelOutliers = FALSE,
  range = TRUE)

### For Outcomes ---
jmv::descriptives(
  data = Final_Data_q,
  vars = vars(Math_Score, 
              Math_Level_r, 
              English_Score, 
              English_Level_r),
  hist = TRUE,
  range = TRUE)

## Correlations -----------------
factor(Final_Data_q$Gender_r)
suppressWarnings(Final_Data_q$Gender_r <- 
                   as.numeric(Final_Data_q$Gender_r))

factor(Final_Data_q$First_Gen_r)
suppressWarnings(Final_Data_q$First_Gen_r <- 
                   as.numeric(Final_Data_q$First_Gen_r))
### Math Performance as Scores ---
jmv::corrMatrix(
  data = Final_Data_q,
  vars = vars(Gender_r, 
              First_Gen_r, 
              GMS_Mean, 
              FMS_Mean, 
              Grit_Factor1, 
              Grit_Factor2, 
              Grit_Factor3, 
              GPA, 
              Math_Score),
  flag = TRUE)

### English Performance as Passing ---
jmv::corrMatrix(
  data = Final_Data_q,
  vars = vars(Gender_r, 
              First_Gen_r, 
              GMS_Mean, 
              FMS_Mean, 
              Grit_Factor1, 
              Grit_Factor2, 
              Grit_Factor3, 
              GPA, 
              English_Level_r),
  flag = TRUE)


## Q2: Linear Regression Math Score ---------

### Model 1 --
jmv::linReg(
  data = Final_Data_q,
  dep = Math_Score,
  covs = vars(First_Gen_r, 
              GMS_Mean, 
              FMS_Mean, 
              Grit_Factor1, 
              Grit_Factor2, 
              Grit_Factor3, 
              GPA),
  blocks = list(
    list(
      "First_Gen_r",
      "GMS_Mean",
      "FMS_Mean",
      "Grit_Factor1",
      "Grit_Factor2",
      "Grit_Factor3",
      "GPA")),
  refLevels = list(),
  modelTest = TRUE,
  stdEst = TRUE,
  collin = TRUE)


### Model 2 -- w/o Grittiness --
jmv::linReg(
  data = Final_Data_q,
  dep = Math_Score,
  covs = vars(First_Gen_r, 
              GMS_Mean, 
              FMS_Mean, 
              Grit_Factor2, 
              Grit_Factor3, 
              GPA),
  blocks = list(
    list(
      "First_Gen_r",
      "GMS_Mean",
      "FMS_Mean",
      "Grit_Factor2",
      "Grit_Factor3",
      "GPA")),
  refLevels = list(),
  modelTest = TRUE,
  stdEst = TRUE,
  collin = TRUE)

### Model 3 -- w/o Grittiness or Fluctuating Interests --
jmv::linReg(
  data = Final_Data_q,
  dep = Math_Score,
  covs = vars(First_Gen_r, 
              GMS_Mean, 
              FMS_Mean, 
              Grit_Factor3, 
              GPA),
  blocks = list(
    list(
      "First_Gen_r",
      "GMS_Mean",
      "FMS_Mean",
      "Grit_Factor3",
      "GPA")),
  refLevels = list(),
  modelTest = TRUE,
  stdEst = TRUE,
  collin = TRUE)

### Final Model: Model 4 -- w/o Gritness, Fluct Int, or Self-Control --
jmv::linReg(
  data = Final_Data_q,
  dep = Math_Score,
  covs = vars(First_Gen_r, 
              GMS_Mean, 
              FMS_Mean, 
              GPA),
  blocks = list(
    list(
      "First_Gen_r",
      "GMS_Mean",
      "FMS_Mean",
      "GPA")),
  refLevels = list(),
  modelTest = TRUE,
  stdEst = TRUE,
  collin = TRUE)

### Moderator: Gender ------

### First Generation Status --
gamlj::gamljGzlm(
  formula = Math_Score ~ First_Gen_r + 
    GMS_Mean + 
    FMS_Mean + 
    GPA + 
    Gender_r:First_Gen_r,
  data = Final_Data_q)
### No interaction

### GMS Mean --
gamlj::gamljGzlm(
  formula = Math_Score ~ First_Gen_r + 
    GMS_Mean + 
    FMS_Mean + 
    GPA + 
    Gender_r:GMS_Mean,
  data = Final_Data_q)
### No Interaction

### FMS Mean --
gamlj::gamljGzlm(
  formula = Math_Score ~ First_Gen_r + 
    GMS_Mean + 
    FMS_Mean + 
    GPA + 
    Gender_r:FMS_Mean,
  data = Final_Data_q)
### No Interaction

### Moderator: First Gen College -----

### GMS Mean --
gamlj::gamljGzlm(
  formula = Math_Score ~ GMS_Mean + 
    FMS_Mean + 
    GPA + 
    First_Gen_r:GMS_Mean,
  data = Final_Data_q)
### No Interaction

### FMS Mean --
gamlj::gamljGzlm(
  formula = Math_Score ~ GMS_Mean + 
    FMS_Mean + 
    GPA + 
    First_Gen_r:FMS_Mean,
  data = Final_Data_q)
### No Interaction

### GPA --
gamlj::gamljGzlm(
  formula = Math_Score ~ GMS_Mean + 
    FMS_Mean + 
    GPA + 
    GPA:First_Gen_r,
  data = Final_Data_q)
### No Interactions

## Q3: Logistic Regression English Passing  ------

## Model 1
jmv::logRegBin(
  data = Final_Data_q,
  dep = English_Level_r,
  covs = vars(Gender_r, First_Gen_r, GMS_Mean, FMS_Mean, Grit_Factor1, Grit_Factor3, GPA),
  blocks = list(
    list(
      "Gender_r",
      "First_Gen_r",
      "GMS_Mean",
      "FMS_Mean",
      "Grit_Factor1",
      "Grit_Factor3",
      "GPA")),
  refLevels = list(
    list(
      var="English_Level_r",
      ref="0")),
  modelTest = TRUE,
  omni = TRUE,
  OR = TRUE,
  collin = TRUE)

## Model 2 -- w/o First Generation Status
jmv::logRegBin(
  data = Final_Data_q,
  dep = English_Level_r,
  covs = vars(Gender_r, GMS_Mean, FMS_Mean, Grit_Factor1, Grit_Factor3, GPA),
  blocks = list(
    list(
      "Gender_r",
      "GMS_Mean",
      "FMS_Mean",
      "Grit_Factor1",
      "Grit_Factor3",
      "GPA")),
  refLevels = list(
    list(
      var="English_Level_r",
      ref="0")),
  modelTest = TRUE,
  omni = TRUE,
  OR = TRUE,
  collin = TRUE)

## Model 3 -- w/o Grittiness or First Gen
jmv::logRegBin(
  data = Final_Data_q,
  dep = English_Level_r,
  covs = vars(Gender_r, GMS_Mean, FMS_Mean, Grit_Factor3, GPA),
  blocks = list(
    list(
      "Gender_r",
      "GMS_Mean",
      "FMS_Mean",
      "Grit_Factor3",
      "GPA")),
  refLevels = list(
    list(
      var="English_Level_r",
      ref="0")),
  modelTest = TRUE,
  omni = TRUE,
  OR = TRUE,
  collin = TRUE)

## Final Model: Model 4 -- w/o Grittiness, First Gen, Gender
jmv::logRegBin(
  data = Final_Data_q,
  dep = English_Level_r,
  covs = vars(GMS_Mean, FMS_Mean, Grit_Factor3, GPA),
  blocks = list(
    list(
      "GMS_Mean",
      "FMS_Mean",
      "Grit_Factor3",
      "GPA")),
  refLevels = list(
    list(
      var="English_Level_r",
      ref="0")),
  modelTest = TRUE,
  omni = TRUE,
  OR = TRUE,
  collin = TRUE)

### Moderator: Gender ------

### GMS Mean
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + Gender_r:GMS_Mean,
  data = Final_Data_q,
  modelSelection = "logistic")
### No Interaction

### FMS Mean
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + Gender_r:FMS_Mean,
  data = Final_Data_q,
  modelSelection = "logistic")
### No interaction

### Self-Control
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + Gender_r:Grit_Factor3,
  data = Final_Data_q,
  modelSelection = "logistic")

### GPA
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + GPA:Gender_r,
  data = Final_Data_q,
  modelSelection = "logistic")
### No Interaction

### Moderator: First Gen College  -----

### GMS Mean
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + First_Gen_r:GMS_Mean,
  data = Final_Data_q,
  modelSelection = "logistic")
### No Interaction

### FMS Mean
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + First_Gen_r:FMS_Mean,
  data = Final_Data_q,
  modelSelection = "logistic")
### No interaction

### Self-Control
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + First_Gen_r:Grit_Factor3,
  data = Final_Data_q,
  modelSelection = "logistic")

### GPA
gamlj::gamljGzlm(
  formula = English_Level_r ~ GMS_Mean + FMS_Mean + Grit_Factor3 + GPA + GPA:First_Gen_r,
  data = Final_Data_q,
  modelSelection = "logistic")
### No Interaction