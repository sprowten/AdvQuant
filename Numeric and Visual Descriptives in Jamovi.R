#Package/Library Install/Load ----
pacman::p_load(jmv, tidyverse, psych, readxl, skimr)
#Import Data File ----
library(readxl)
Excel_BFRSS_Intro_to_R_Data <- 
  read_excel("Desktop/PSY 5050 ADV QUANT/Copy of BFRSS Intro to R Data.xlsx")
#From Jamovi ----
##Age Syntax Assigment Copy/Paste ----
##Descriptives&Visualization
jmv::descriptives(
  data = Excel_BFRSS_Intro_to_R_Data,
  vars = AGE,
  hist = TRUE,
  boxMean = TRUE,
  missing = FALSE,
  ci = TRUE,
  skew = TRUE,
  kurt = TRUE)
##Martial Syntax Assigment Copy/Paste ----
##Descriptives&Visualization
jmv::descriptives(
  data = Excel_BFRSS_Intro_to_R_Data,
  vars = MARITAL,
  bar = TRUE,
  min = FALSE,
  max = FALSE,
  ci = TRUE,
  skew = TRUE,
  kurt = TRUE)
class(Excel_BFRSS_Intro_to_R_Data$MARITAL) #numeric
###Recode ----
Excel_BFRSS_Intro_to_R_Data$MARITAL_r <- 
  ifelse(Excel_BFRSS_Intro_to_R_Data$MARITAL == 9, NA,
         Excel_BFRSS_Intro_to_R_Data$MARITAL)
#Check Recode with BaseR Option1
#Show frequency table with new variable 
table(Excel_BFRSS_Intro_to_R_Data$MARITAL, 
      Excel_BFRSS_Intro_to_R_Data$MARITAL_r, 
      useNA = "always")
##Redo Column labels ----
factor(Excel_BFRSS_Intro_to_R_Data$MARITAL_r,
       labels = c("Married", "Divorced", "Widowed", "Seperated", 
                  "Never Married", "Untraditional"))
class(Excel_BFRSS_Intro_to_R_Data$MARITAL_r) #still coming up numeric
#I'm still having trouble recoding the class, not necessary for assignment but
#would like to be able to do this
##Descriptives&Visualization
  jmv::descriptives(
    data = Excel_BFRSS_Intro_to_R_Data,
    vars = MARITAL_r,
    desc = "rows",
    min = FALSE,
    max = FALSE,
    ci = TRUE,
    skew = TRUE,
    kurt = TRUE)