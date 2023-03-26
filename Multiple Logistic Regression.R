#Package/Library Install/Load ----
pacman::p_load(jmv, tidyverse, psych, readxl, skimr, haven)
#Load College Access Dataset as SPSS File----
library(haven)
College_Access <- read_sav("Desktop/PSY 5050 ADV QUANT/College Access.sav")
#Analyze Data ----
##Run Descriptives ----
skimr::skim(College_Access)
##No missing values in Enrollment (outcome), skewed left, outlier
##No missing values in CU_GPA/ACT_COMP/SS_Total/PS_Total (predictors)
jmv::descriptives(
  data = College_Access,
  vars = vars(Enrollment, 
              CU_GPA, 
              ACT_COMP, 
              SS_Total, 
              PS_Total),
  hist = TRUE,
  skew = TRUE)
##Ran descriptives on the specific variables to go into my model to get a closer look
##still not red flags, moving forward
##Correlations----
jmv::corrMatrix(
  data = College_Access,
  vars = vars(Enrollment, 
              CU_GPA, 
              ACT_COMP, 
              SS_Total, 
              PS_Total),
  sig = TRUE,
  flag = TRUE,
  ci = TRUE)
##Binominal Logistic Regression----
jmv::logRegBin(
  data = College_Access,
  dep = Enrollment,
  covs = vars(CU_GPA, 
              SS_Total, 
              PS_Total, 
              ACT_COMP),
  blocks = list(
    list(
      "CU_GPA",
      "SS_Total",
      "PS_Total",
      "ACT_COMP")),
  refLevels = list(
    list(
      var="Enrollment",
      ref="0")),
  omni = TRUE,
  ci = TRUE,
  OR = TRUE,
  ciOR = TRUE,
  auc = TRUE, #0.675
  rocPlot = TRUE,
  collin = TRUE) #passed
##CU_GPA (.682) is a significant predictor ( p < .001) of enrollment. OR = 1.98 
##CI [1.66, 2.36]
##Intercept (-1.46, CI[.113,.473]), OR = .231
##SS_Total (.00124) is significant (p = .003), OR = 1.001, CI [1.00, 1.002]
##PS_Total and ACT_COMP not significant
