#Install Packages ------
install.packages("tidyverse")   
install.packages("psych")    
install.packages("pacman")   
#Load Libraries -----
pacman::p_load(tidyverse, psych, readxl, skimr, ggplot2, car, janitor)
#Work with Excel BFRSS File ----
Excel_BFRSS_Intro_to_R_Data <- 
  read_excel("Desktop/PSY 5050 ADV QUANT/Copy of BFRSS Intro to R Data.xlsx")
#Check Descriptives and Visuals Before
##GREENSAL
table(Excel_BFRSS_Intro_to_R_Data$GREENSAL, useNA = 'always') #will show NA
skimr::skim(Excel_BFRSS_Intro_to_R_Data$GREENSAL) #heavy positive skew
#  skim_variable n_missing complete_rate mean   sd p0 p25 p50 p75 p100 hist 
#1 data                  7         0.989 79.5 185.  1   8  12  30  999 ▇▁▁▁▁
##VEGETABL
table(Excel_BFRSS_Intro_to_R_Data$VEGETABL, useNA = 'always') #will show NA
skimr::skim(Excel_BFRSS_Intro_to_R_Data$VEGETABL) #heavy positive skew
#  skim_variable n_missing complete_rate mean   sd p0 p25 p50 p75 p100 hist 
#1 data                  7         0.989 88.9 189.  1  13  30  60  999 ▇▁▁▁▁
##FRUIT
table(Excel_BFRSS_Intro_to_R_Data$FRUIT, useNA = 'always') #will show NA
skimr::skim(Excel_BFRSS_Intro_to_R_Data$FRUIT) #heavy positive skew
#  skim_variable n_missing complete_rate mean   sd p0 p25 p50 p75 p100 hist 
#1 data                  7         0.989 58.1 145.  1   8  28  30  777 ▇▁▁▁▁
##FRUITJUI
table(Excel_BFRSS_Intro_to_R_Data$FRUITJUI, useNA = 'always') #will show NA
skimr::skim(Excel_BFRSS_Intro_to_R_Data$FRUITJUI) #heavy positive skew
#skim_variable n_missing complete_rate mean   sd p0 p25 p50 p75 p100 hist 
#1 data                  6         0.991 109. 212.  1   8  28  30  999 ▇▁▁▁▁
##Recode Variables ----
Excel_BFRSS_Intro_to_R_Data_r <- Excel_BFRSS_Intro_to_R_Data %>%
  dplyr::mutate(GREENSAL_r = 
                  ifelse(GREENSAL == 555, 0,
                         ifelse(GREENSAL == 777, NA,
                                ifelse(GREENSAL == 999, NA,
                                       GREENSAL))),
                VEGETABL_r = 
                  ifelse(VEGETABL == 555, 0,
                         ifelse(VEGETABL == 777, NA,
                                ifelse(VEGETABL == 999, NA,
                                       VEGETABL))),
                FRUIT_r = 
                  ifelse(FRUIT == 555, 0,
                         ifelse(FRUIT == 777, NA,
                                ifelse(FRUIT == 999, NA,
                                       FRUIT))),
                FRUITJUI_r = 
                  ifelse(FRUITJUI == 555, 0,
                         ifelse(FRUITJUI == 777, NA,
                                ifelse(FRUITJUI == 999, NA,
                                       FRUITJUI))))
##Add Columns/Variables ----
##Health_Eat_Sum ----
"HEALTH_EAT_SUM" <- Excel_BFRSS_Intro_to_R_Data_r$FRUIT_r + 
  Excel_BFRSS_Intro_to_R_Data_r$GREENSAL_r + 
  Excel_BFRSS_Intro_to_R_Data_r$VEGETABL_r + 
  Excel_BFRSS_Intro_to_R_Data_r$FRUITJUI_r # Create Variable 
Excel_BFRSS_Intro_to_R_Data_r['HEALTH_EAT_SUM'] <- 
  c(HEALTH_EAT_SUM)
head(Excel_BFRSS_Intro_to_R_Data_r)
##Check Column Names
colnames(Excel_BFRSS_Intro_to_R_Data_r)
##Check Descriptives and Visuals
psych::describe(Excel_BFRSS_Intro_to_R_Data_r$HEALTH_EAT_SUM)
## HEALTH_EAT_AVG -----
##AVG number of veggies, green salads, fruit, and fruit juices
"HEALTH_EAT_AVG" <- (Excel_BFRSS_Intro_to_R_Data_r$FRUIT_r+ 
  Excel_BFRSS_Intro_to_R_Data_r$GREENSAL_r + 
  Excel_BFRSS_Intro_to_R_Data_r$VEGETABL_r + 
  Excel_BFRSS_Intro_to_R_Data_r$FRUITJUI_r)/4 #Create Variable 
Excel_BFRSS_Intro_to_R_Data_r['HEALTH_EAT_AVG'] <- 
  c(HEALTH_EAT_AVG)
##Check Column Names
colnames(Excel_BFRSS_Intro_to_R_Data_r)
##Check Descriptives and Visuals
skimr::skim(Excel_BFRSS_Intro_to_R_Data_r, HEALTH_EAT_AVG, HEALTH_EAT_SUM)
#Create Data Chart -----
suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
}) #Add this code to get around following error -- could not find function "aes"
##Histogram for Sum
ggplot2::ggplot(Excel_BFRSS_Intro_to_R_Data_r, aes(HEALTH_EAT_SUM)) + geom_histogram(binwidth = 1)
ggplot(Excel_BFRSS_Intro_to_R_Data_r, aes(HEALTH_EAT_SUM)) + geom_histogram(binwidth = 10)
#Positive Skew, Highest Outlier Near 1000
##Histogram for Average
ggplot2::ggplot(Excel_BFRSS_Intro_to_R_Data_r, aes(HEALTH_EAT_AVG)) + geom_histogram(binwidth = 1)
ggplot(Excel_BFRSS_Intro_to_R_Data_r, aes(HEALTH_EAT_AVG)) + geom_histogram(binwidth = 10)
#Positive Skew, Highest Outlier Near 330



