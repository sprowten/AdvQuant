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
##Variables of interest MPG_City, MPG_Highway and MSRP do not need re-coded
##Since cylinders is not a variable of interest, will not be re-coding this at this time
##Correlations ----
jmv::corrMatrix(
  data = Cars_file,
  vars = vars(MPG_City, MPG_Highway, MSRP),
  flag = TRUE)
##HWY/CITY -- r = .94, p < .001
##MSRP/CITY -- r = -.48, p < .001
##MSRP/HWY -- r = -.44, p < .001
##All significant, all predictors should be included in regression
##Multiple Regression ----
jmv::linReg(
  data = Cars_file,
  dep = MSRP,
  covs = vars(MPG_City, MPG_Highway),
  blocks = list(
    list(
      "MPG_City",
      "MPG_Highway")),
  refLevels = list())
##R = .48, r^2 = .2261, explains 22.62% variance in MSRP



