#Package/Library Install/Load ----
pacman::p_load(jmv, tidyverse, psych, readxl, skimr, haven)
#Load Cars Dataset as SPSS File----
library(haven)
Cars_file <- read_sav("Desktop/PSY 5050 ADV QUANT/Cars SPSS file.sav")
#Analyze Data ----
##Run Descriptives ----
skimr::skim(Cars_file)
##2 Missing values in cylinders
table(Cars_file$Cylinders, useNA = 'always') #Will Show NA
#   3    4    5    6    8   10   12 <NA> 
#1  136    7  190   87    2    3    2 
##Variables of interest MPG_City and MSRP do not need re-coded
##Since cylinders is not a variable of interest, will not be recoding this at this time
jmv::descriptives(
  data = Cars_file,
  vars = vars(MSRP, MPG_City),
  hist = TRUE) #Both histograms have positive skew
##Correlations ----
jmv::corrMatrix(
  data = Cars_file,
  vars = vars(MPG_City, MSRP),
  ci = TRUE)
#r = -.475, 95% CI [-.398,-.545], p < .001
#Correlation is significant, so this would qualify to would forward to run simple regression
##Simple Regression ----
jmv::linReg(
  data = Cars_file,
  dep = MSRP,
  covs = MPG_City,
  blocks = list(
    list(
      "MPG_City")),
  refLevels = list(),
  norm = TRUE, #passed assumption check
  durbin = TRUE, #passed assumption check
  collin = TRUE) #passed assumption check 
#R = .475, R^2 = .2256
#Model explains 22.56% of the variance in MSRP

