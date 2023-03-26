#Package/Library Install/Load ----
pacman::p_load(jmv, tidyverse, psych, readxl, skimr, haven)
#Load AQ SPSS Dataset ----
library(haven)
R_Studio_AQ <- read_sav("Desktop/PSY 5050 ADV QUANT/R_Studio_AQ.sav")
View(R_Studio_AQ) #pulls up data table
#Analyze Data ----
##Run Descriptives ----
skimr::skim(R_Studio_AQ)
##No Missing Values, All questions used 5 point Likert scale
#should not need to recode based on skimr output, 
#but need to rename variables to make it easier to read
##Rename Variables using dplyr----
R_Studio_AQ_r <- R_Studio_AQ %>%
  dplyr::rename(
    Q_01Stat.Cry = Q_01,
    Q_02Friend.Think.Stupid = Q_02,
    Q_03SD.Excites = Q_03,
    Q_04Dream.Pearson = Q_04,
    Q_05Dnt.Under.Stats = Q_05,
    Q_06Exp.Computer = Q_06,
    Q_07Computer.Hate = Q_07,
    Q_08Never.Good.Math = Q_08,
    Q_09Friend.Better.Stats = Q_09,
    Q_10Computer.Games = Q_10,
    Q_11Bad.Math = Q_11,
    Q_12R.Not.Easier = Q_12,
    Q_13Bad.Math = Q_13,
    Q_14Computer.Mind.Own = Q_14,
    Q_15Compute.Get.Me = Q_15,
    Q_16Weep.MCT = Q_16,
    Q_17Coma.Equation = Q_17,
    Q_18R.Crashes = Q_18,
    Q_19Every.Look.R = Q_19,
    Q_20Cant.Sleep.Eigen = Q_20,
    Q_21Trapped.Normal.Dist = Q_21,
    Q_22Friend.Better.R = Q_22,
    Q_23Good.Stats.Nerd = Q_23)
##Double check that I did this right
colnames(R_Studio_AQ_r) #Looks good
##Run EFA on all variables ----      
jmv::efa(
  data = R_Studio_AQ_r,
  vars = vars(Q_01Stat.Cry, 
              Q_02Friend.Think.Stupid, 
              Q_03SD.Excites, 
              Q_04Dream.Pearson, 
              Q_05Dnt.Under.Stats, 
              Q_06Exp.Computer, 
              Q_07Computer.Hate, 
              Q_08Never.Good.Math, 
              Q_09Friend.Better.Stats, 
              Q_10Computer.Games, 
              Q_11Bad.Math, 
              Q_12R.Not.Easier, 
              Q_13Bad.Math, 
              Q_14Computer.Mind.Own, 
              Q_15Compute.Get.Me, 
              Q_16Weep.MCT, 
              Q_17Coma.Equation, 
              Q_18R.Crashes, 
              Q_19Every.Look.R, 
              Q_20Cant.Sleep.Eigen, 
              Q_21Trapped.Normal.Dist, 
              Q_22Friend.Better.R, 
              Q_23Good.Stats.Nerd),
  extraction = "pa",
  rotation = "promax",
  screePlot = TRUE,
  factorSummary = TRUE,
  kmo = TRUE)

