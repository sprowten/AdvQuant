# ---- Package/Library Install/Load ----
install.packages("pacman")
library("pacman")
pacman::p_load(jmv, tidyverse, psych, readxl, 
               skimr, readxl, dplyr, psych, expss, 
               ggpubr, magrittr, gamlj)


# ---- Load College Access Services Data ----
library(readxl)
Regression_Assignment_Data <- 
  read_excel("Desktop/PSY 5050 ADV QUANT/Copy of Regression Assignment Data.xlsx")
View(Regression_Assignment_Data)


# ---- Descriptives ----
skimr::skim(Regression_Assignment_Data)
#138 missing in gender only, all variables skewed right
## ---- Recode Gender ----
table(Regression_Assignment_Data$Gender, useNA = 'always')
Regression_Assignment_Data_r <- Regression_Assignment_Data %>%
  dplyr::mutate(Gender_r =
                   ifelse(Gender == "M", 0,
                          ifelse(Gender == "F", 1,
                                 ifelse(Gender == "NULL", NA,
                                        Gender))))
##Check Recodes
table(Regression_Assignment_Data_r$Gender_r, useNA = 'always')
colnames(Regression_Assignment_Data_r)
skimr:: skim(Regression_Assignment_Data_r$Gender_r)
View(Regression_Assignment_Data_r)


## ----- Correlations--------
### ---- Hours ----
jmv::corrMatrix(
  data = Regression_Assignment_Data_r,
  vars = vars(Advising_Hrs, 
              Career_Hrs, 
              College_Hrs, 
              Mentor_Hrs, 
              Counsel_Hrs, 
              Field_Hrs, 
              Financial_Hrs, 
              Job_Hrs, 
              Summer_Hrs, 
              Tutor_Hrs, 
              Workshop_Hrs),
  flag = TRUE)
### Meet criteria (.3 < r < .8):Academic Advising, Career Counseling, College Visit, Mentoring, Counseling & Advising, Financial Aid, Job Visit/Shadowing, Tutoring/Homework, and Workshop
### Don't meet criteria:Field Trip and Summer Programs so not in EFA

### ---- Count ----
### 1) Counseling and advising removing for crossing loading
jmv::corrMatrix(
  data = Regression_Assignment_Data_r,
  vars = vars(Advising_Ct, 
              Career_Ct, 
              College_Ct, 
              Mentor_Ct, 
              Counsel_Ct, 
              Field_Ct, 
              Financial_Ct, 
              Job_Ct, 
              Summer_Ct, 
              Tutor_Ct, 
              Workshop_Ct),
  flag = TRUE)
### Meet criteria (.3 < r < .8): Academic Advising, Career Counseling, College Visit, Mentoring, Counseling & Advising, Financial Aid, Tutoring/Homework, and Workshop.
### Don't meet criteria: Field Trip, Job Visit/Shadowing, Summer Programs so not in EFA


## -------- EFA ---------
### ---- Hours ------
jmv::efa(
  data = Regression_Assignment_Data_r,
  vars = vars(Advising_Hrs, 
              Career_Hrs, 
              College_Hrs,
              Mentor_Hrs, 
              Counsel_Hrs, 
              Financial_Hrs, 
              Job_Hrs, 
              Tutor_Hrs, 
              Workshop_Hrs),
  extraction = "pa", #picked principal analysis since all histograms skewed
  sortLoadings = TRUE,
  screePlot = TRUE, #Elbow at 3
  factorCor = TRUE,
  factorSummary = TRUE,
  kmo = TRUE, #Passed
  bartlett = TRUE) #Passed
### 1- Career, Finacial, Counsel, Tutor
### 2 - Workshop, Mentor, Job
### 3  College, Advising

### ---- Create Aggregate Columns for Hours Factors ------
### Experiential Learning HRS
"Experiential_Learning_Hrs" <- Regression_Assignment_Data_r$Mentor_Hrs + 
  Regression_Assignment_Data_r$Workshop_Hrs
Regression_Assignment_Data_r['Experiential_Learning_Hrs'] <- 
  c(Experiential_Learning_Hrs)
head(Regression_Assignment_Data_r)
## Check Column Names
colnames(Regression_Assignment_Data_r)
## Recheck Descriptives and Visuals
psych::describe(Regression_Assignment_Data_r$Experiential_Learning_Hrs)

#### Interest in College HRS
  "College_Interest" <- Regression_Assignment_Data_r$College_Hrs + 
    Regression_Assignment_Data_r$Advising_Hrs 
  Regression_Assignment_Data_r['College_Interest'] <- 
  c(College_Interest)
head(Regression_Assignment_Data_r)
##Check Column Names
colnames(Regression_Assignment_Data_r)
## Recheck Descriptives and Visuals
psych::describe(Regression_Assignment_Data_r$College_Interest)

#### Guidance Counseling HRS
"Guidance_Counseling" <- Regression_Assignment_Data_r$Career_Hrs + 
  Regression_Assignment_Data_r$Financial_Hrs + 
  Regression_Assignment_Data_r$Counsel_Hrs +
  Regression_Assignment_Data_r$Tutor_Hrs # Create Variable 
Regression_Assignment_Data_r['Guidance_Counseling'] <- 
  c(Guidance_Counseling)
head(Regression_Assignment_Data_r)
##Check Column Names
colnames(Regression_Assignment_Data_r)
## Recheck Descriptives and Visuals
psych::describe(Regression_Assignment_Data_r$Guidance_Counseling)

### ---- Count ------ 
jmv::efa(
  data = Regression_Assignment_Data_r,
  vars = vars(Advising_Ct, 
              Career_Ct, 
              College_Ct, 
              Mentor_Ct, 
              Counsel_Ct, 
              Financial_Ct, 
              Tutor_Ct, 
              Workshop_Ct),
  extraction = "pa", #picked principal analysis since all histograms skewed
  sortLoadings = TRUE,
  screePlot = TRUE, #Elbow at 2
  factorCor = TRUE,
  factorSummary = TRUE,
  kmo = TRUE,
  bartlett = TRUE)
###Use oblique rotation (obilimin), with parallel
###Workshop is cross-loaded on 1, 2 with uniqueness of .515
###Career is cross-loaded on 1,4 with uniqueness of .480
###Counsel is cross-loaded on 1, 2 with uniqueness of .540
###Removed Counseling, reran model, then removed Career to get final model below
jmv::efa(
  data = Regression_Assignment_Data_r,
  vars = vars(College_Ct, 
              Mentor_Ct, 
              Financial_Ct, 
              Workshop_Ct, 
              Tutor_Ct),
  extraction = "pa",
  hideLoadings = 0.4,
  sortLoadings = TRUE,
  screePlot = TRUE,
  factorCor = TRUE,
  factorSummary = TRUE,
  bartlett = TRUE)

### --- Create Aggregate Columns for Count Factors ----
### College Preparedness Count
"College_Preparedness" <- Regression_Assignment_Data_r$College_Ct + 
  Regression_Assignment_Data_r$Financial_Ct + 
  Regression_Assignment_Data_r$Tutor_Ct 
Regression_Assignment_Data_r['College_Preparedness'] <- 
  c(College_Preparedness)
head(Regression_Assignment_Data_r)
## Check Column Names
colnames(Regression_Assignment_Data_r)
## Recheck Descriptives and Visuals
psych::describe(Regression_Assignment_Data_r$College_Preparedness)

#### Experiential Learning CT
"Experiential_Learning_Ct" <- Regression_Assignment_Data_r$Workshop_Ct + 
  Regression_Assignment_Data_r$Mentor_Ct 
Regression_Assignment_Data_r['Experiential_Learning_Ct'] <- 
  c(Experiential_Learning_Ct)
head(Regression_Assignment_Data_r)
##Check Column Names
colnames(Regression_Assignment_Data_r)
## Recheck Descriptives and Visuals
psych::describe(Regression_Assignment_Data_r$Experiential_Learning_Ct)


# ------ Regressions ---------
#Outcome Variable Descriptives
factor(Regression_Assignment_Data_r$Gender_r)
suppressWarnings(Regression_Assignment_Data_r$Gender_r <- 
                   as.numeric(Regression_Assignment_Data_r$Gender_r))


factor(Regression_Assignment_Data_r$Grad_GPA)
suppressWarnings(Regression_Assignment_Data_r$Grad_GPA <- 
                   as.numeric(Regression_Assignment_Data_r$Grad_GPA))


jmv::descriptives(
  data = Regression_Assignment_Data_r,
  vars = vars(Gender_r, 
              Grad_GPA,
              Fall_Enrolled),
  desc = "rows",
  n = FALSE,
  missing = FALSE)

## ----- Variable Correlations -----
### Descriptives and Correlation Matrix for Factors in Terms of Hours ----
jmv::descriptives(
  data = Regression_Assignment_Data_r,
  vars = vars(Guidance_Counseling, 
              Experiential_Learning_Hrs, 
              College_Interest),
  desc = "rows",
  n = FALSE,
  missing = FALSE,
  min = FALSE)
jmv::corrMatrix(
  data = Regression_Assignment_Data_r,
  vars = vars(Guidance_Counseling, 
              Experiential_Learning_Hrs, 
              College_Interest,
              Grad_GPA,
              Fall_Enrolled,
              Gender_r),
  flag = TRUE)

### Descriptives and Correlation Matrix for Factors in Terms of Count ----
jmv::descriptives(
  data = Regression_Assignment_Data_r,
  vars = vars(Experiential_Learning_Ct, 
              College_Preparedness),
  desc = "rows",
  n = FALSE,
  missing = FALSE,
  min = FALSE)
jmv::corrMatrix(
  data = Regression_Assignment_Data_r,
  vars = vars(Experiential_Learning_Ct, 
              College_Preparedness,
              Grad_GPA,
              Fall_Enrolled,
              Gender_r),
  flag = TRUE)


## ---- Linear Regressions -----
### ---- Hours ----
jmv::linReg(
  data = Regression_Assignment_Data_r,
  dep = Grad_GPA,
  covs = vars(Guidance_Counseling, 
              Experiential_Learning_Hrs, 
              College_Interest),
  factors = Gender_r,
  blocks = list(
    list(
      "College_Interest",
      "Gender_r",
      "Experiential_Learning_Hrs")),
  refLevels = list(
    list(
      var="Gender_r",
      ref="0")),
  rmse = TRUE,
  modelTest = TRUE,
  collin = TRUE) 
### Model 2
jmv::linReg(
  data = Regression_Assignment_Data_r,
  dep = Grad_GPA,
  covs = vars(Experiential_Learning_Hrs, 
              College_Interest),
  factors = Gender_r,
  blocks = list(
    list(
      "Experiential_Learning_Hrs",
      "College_Interest",
      "Gender_r")),
  refLevels = list(
    list(
      var="Gender_r",
      ref="0")),
  rmse = TRUE)

### Model 3 -Final linear Hours
jmv::linReg(
  data = Regression_Assignment_Data_r,
  dep = Grad_GPA,
  covs = College_Interest,
  factors = Gender_r,
  blocks = list(
    list(
      "Gender_r",
      "College_Interest")),
  refLevels = list(
    list(
      var="Gender_r",
      ref="0")),
  rmse = TRUE)
#### ---- Moderation w/ Gender ----
gamlj::gamljGzlm(
  formula = Grad_GPA ~ Gender_r + College_Interest + Gender_r:College_Interest,
  data = Regression_Assignment_Data_r)
#### gender goes does moderate college interest services effect on GPA

### ---- Count -----
jmv::linReg(
  data = Regression_Assignment_Data_r,
  dep = Grad_GPA,
  covs = vars(College_Preparedness, 
              Experiential_Learning_Ct),
  factors = Gender_r,
  blocks = list(
    list(
      "Experiential_Learning_Ct",
      "College_Preparedness",
      "Gender_r")),
  refLevels = list(
    list(
      var="Gender_r",
      ref="0")),
  rmse = TRUE,
  modelTest = TRUE,
  collin = TRUE)
#### ----Moderation w/ Gender ----
#### Experiential Learning
gamlj::gamljGzlm(
  formula = Grad_GPA ~ Gender_r + Experiential_Learning_Ct + College_Preparedness + Gender_r:Experiential_Learning_Ct,
  data = Regression_Assignment_Data_r)
#### Gender does not interact with Experiential Learning

#### College Preparedness
gamlj::gamljGzlm(
  formula = Grad_GPA ~ Gender_r + Experiential_Learning_Ct + College_Preparedness + Gender_r:College_Preparedness,
  data = Regression_Assignment_Data_r)
#### Gender does interact with College Preparedness

## ---- Binominal Logistic Regression -----
### ---- Hours ----
jmv::logRegBin(
  data = Regression_Assignment_Data_r,
  dep = Fall_Enrolled,
  covs = vars(Grad_GPA, 
              Guidance_Counseling, 
              College_Interest, 
              Experiential_Learning_Hrs),
  factors = Gender_r,
  blocks = list(
    list(
      "Grad_GPA",
      "Guidance_Counseling",
      "College_Interest",
      "Experiential_Learning_Hrs",
      "Gender_r"),
    list(
      c(
        "Experiential_Learning_Hrs",
        "Gender_r"))),
  refLevels = list(
    list(
      var="Fall_Enrolled",
      ref="0"),
    list(
      var="Gender_r",
      ref="0")),
  modelTest = TRUE,
  collin = TRUE)

### Model 2 - Final Model Logistic Hours
jmv::logRegBin(
  data = Regression_Assignment_Data_r,
  dep = Fall_Enrolled,
  covs = vars(Grad_GPA, 
              Guidance_Counseling, 
              College_Interest, 
              Experiential_Learning_Hrs),
  blocks = list(
    list(
      "Grad_GPA",
      "Guidance_Counseling",
      "College_Interest",
      "Experiential_Learning_Hrs")),
  refLevels = list(
    list(
      var="Fall_Enrolled",
      ref="0")),
  modelTest = TRUE,
  collin = TRUE)

#### ----Moderation w/ Gender ----
#### Guidance and Counseling
gamlj::gamljGzlm(
  formula = Fall_Enrolled ~ Gender_r + Grad_GPA + `Guidance_Counseling` + College_Interest + Gender_r:`Guidance_Counseling`,
  data = Regression_Assignment_Data_r,
  modelSelection = "logistic")
#### No moderation with Gender

#### College Interest
gamlj::gamljGzlm(
  formula = Fall_Enrolled ~ Gender_r + Grad_GPA + `Guidance_Counseling` + College_Interest + Gender_r:College_Interest,
  data = Regression_Assignment_Data_r,
  modelSelection = "logistic")
#### No moderation with Gender

#### High-school GPA
gamlj::gamljGzlm(
  formula = Fall_Enrolled ~ Gender_r + Grad_GPA + `Guidance_Counseling` + College_Interest + Gender_r:Grad_GPA,
  data = Regression_Assignment_Data_r,
  modelSelection = "logistic")
#### No moderation with Gender

#### Experiential Learning
gamlj::gamljGzlm(
  formula = Fall_Enrolled ~ `Guidance_Counseling` + 
    Experiential_Learning_Hrs + College_Interest + Grad_GPA + 
    Experiential_Learning_Hrs:Gender_r,
  data = Regression_Assignment_Data_r,
  modelSelection = "logistic")
#### No moderation with Gender

### ---- Count -----
jmv::logRegBin(
  data = Regression_Assignment_Data_r,
  dep = Fall_Enrolled,
  covs = vars(Grad_GPA, 
              College_Preparedness, 
              Experiential_Learning_Ct),
  factors = Gender_r,
  blocks = list(
    list(
      "Grad_GPA",
      "Experiential_Learning_Ct",
      "Gender_r"),
    list(
      c(
        "Gender_r",
        "Experiential_Learning_Ct"))),
  refLevels = list(
    list(
      var="Fall_Enrolled",
      ref="0"),
    list(
      var="Gender_r",
      ref="0")),
  modelTest = TRUE,
  collin = TRUE)

### Model 2 
jmv::logRegBin(
  data = Regression_Assignment_Data_r,
  dep = Fall_Enrolled,
  covs = vars(Experiential_Learning_Ct, 
              Grad_GPA),
  factors = Gender_r,
  blocks = list(
    list(
      "Gender_r",
      "Experiential_Learning_Ct",
      "Grad_GPA")),
  refLevels = list(
    list(
      var="Fall_Enrolled",
      ref="0"),
    list(
      var="Gender_r",
      ref="0")),
  modelTest = TRUE)

### Model 3 - Final Logistic Count
jmv::logRegBin(
  data = Regression_Assignment_Data_r,
  dep = Fall_Enrolled,
  covs = vars(Grad_GPA, 
              Experiential_Learning_Ct),
  blocks = list(
    list(
      "Grad_GPA",
      "Experiential_Learning_Ct")),
  refLevels = list(
    list(
      var="Fall_Enrolled",
      ref="0")),
  modelTest = TRUE,
  collin = TRUE)

#### ---- Moderation w/ Gender -----
#### Experiential Learning
gamlj::gamljGzlm(
  formula = Fall_Enrolled ~ Grad_GPA + Experiential_Learning_Ct + 
    Gender_r:Experiential_Learning_Ct,
  data = Regression_Assignment_Data_r,
  modelSelection = "logistic")
#### no moderation with gender

#### High school GPA
gamlj::gamljGzlm(
  formula = Fall_Enrolled ~ Grad_GPA + 
    Experiential_Learning_Ct + Grad_GPA:Gender_r,
  data = Regression_Assignment_Data_r,
  modelSelection = "logistic")
####No moderation with gender
