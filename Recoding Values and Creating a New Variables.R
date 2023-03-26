#Install Packages ------
install.packages("tidyverse")   
install.packages("psych")    
install.packages("pacman")   
#Load Libraries -----
pacman::p_load(tidyverse, psych, readxl, skimr)
#Work with Excel BFRSS File ----
library(readxl)
Excel_BFRSS_Intro_to_R_Data <- 
  read_excel("Desktop/PSY 5050 ADV QUANT/Copy of BFRSS Intro to R Data.xlsx")
##LSATISFY Variable -----
###LSATISFY Exploring----
skim(Excel_BFRSS_Intro_to_R_Data, LSATISFY)
###10 missing values
table(Excel_BFRSS_Intro_to_R_Data$LSATISFY, useNA = "always") #Show frequency table
### 1 = 249, 2 = 370, 3 = 21, 4 = 4, 7 = 2, 9 =1 
### Missing values include 5, 6, 8, 10 
##Values 7 and 9 meaningless in Codebook
###Distrubtion and Descriptives
psych::describe(Excel_BFRSS_Intro_to_R_Data$LSATISFY)
summary(Excel_BFRSS_Intro_to_R_Data$LSATISFY)
###Add variable Labels
### Recode Variable with BaseR
Excel_BFRSS_Intro_to_R_Data$LSATISFY_r <- 
  ifelse(Excel_BFRSS_Intro_to_R_Data$LSATISFY == 7, NA, 
                     ifelse(Excel_BFRSS_Intro_to_R_Data$LSATISFY == 9, NA,
                             Excel_BFRSS_Intro_to_R_Data$LSATISFY))
#Check Recode with BaseR Option1
#Show frequency table with new variable 
table(Excel_BFRSS_Intro_to_R_Data$LSATISFY, Excel_BFRSS_Intro_to_R_Data$LSATISFY_r, useNA = "always") 
#Another way to Check w/ BaseR
with(Excel_BFRSS_Intro_to_R_Data, table(LSATISFY, LSATISFY_r, useNA = "always"))
#Visualize New Variable of Life Satisfaction
LifeSatisfaction <- Excel_BFRSS_Intro_to_R_Data$LSATISFY_r
hist(LifeSatisfaction)
#Positive skew

