#Package/Library Install/Load ----
pacman::p_load(jmv, tidyverse, psych, readxl, skimr, haven)
#Load College Access Dataset as SPSS File----
library(haven)
College_Access <- read_sav("Desktop/PSY 5050 ADV QUANT/College Access.sav")
#Analyze Data ----
##Run Descriptives ----
skimr::skim(College_Access)
##No missing values in Enrollment (outcome), skewed left, outlier
##No missing values in CU_GPA (predictor), normally distrubted
jmv::descriptives(
  data = College_Access,
  vars = vars(Enrollment, CU_GPA),
  hist = TRUE,
  skew = TRUE)
##Ran descriptives on the specific variables to go into my model to get a closer look
##still not red flags, moving forward
##Correlations----
jmv::corrMatrix(
  data = College_Access,
  vars = vars(Enrollment, CU_GPA),
  flag = TRUE,
  ci = TRUE)
##positivity correlated, .262, p < .001
##Binominal Logistic Regression----
jmv::logRegBin(
  data = College_Access,
  dep = Enrollment,
  covs = CU_GPA,
  blocks = list(
    list(
      "CU_GPA")),
  refLevels = list(
    list(
      var="Enrollment",
      ref="0")),
  omni = TRUE, #X^2 = 62.82, p < .001
  ci = TRUE,
  OR = TRUE,
  ciOR = TRUE,
  auc = TRUE, #0.659
  rocPlot = TRUE,
  collin = TRUE) #passed
##CU_GPA (.667) is a signficant predictor ( p < .0001) of enrollment. OR = 1.95 
##CI [1.64, 2.31]
##Intercept (-1.22, CI[.177,.493]), OR = .295