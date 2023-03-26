#Package/Library Install/Load ----
pacman::p_load(jmv, tidyverse, psych, readxl, skimr, haven)
#Load College Access Dataset as SPSS File----
library(haven)
Excerise_Data <- read_sav("Desktop/PSY 5050 ADV QUANT/Exercise file.sav")
#Analyze Data ----
##Run Descriptives ----
jmv::descriptives(
  data = Exercise_Data,
  vars = vars(loss, 
              hours, 
              effort),
  hist = TRUE)
##Ran descriptives on the specific variables to go into my model to get a closer look
##Loss slightly skewed right, other variables have normal distributions
##still not red flags, moving forward
##Correlations----
jmv::corrMatrix(
  data = Exercise_Data,
  vars = vars(hours, 
              effort, 
              loss),
  flag = TRUE)
##Loos signficantly correlated to hours and effort
##Moderated Multiple Regression----
jmv::linReg(
  data = Exercise_Data,
  dep = loss,
  covs = vars(effort, hours),
  blocks = list(
    list(
      "effort",
      "hours",
      c("effort", 
        "hours"))),
  refLevels = list(),
  ci = TRUE,
  stdEst = TRUE,
  norm = TRUE, #passed
  qqPlot = TRUE,
  collin = TRUE) #Violated, go back and center predictors
##Interaction term is significant, main effects are not




