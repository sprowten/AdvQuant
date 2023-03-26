#Package/Library Install/Load ----
install.packages("pacman")
library(pacman)
pacman::p_load(jmv, tidyverse, psych, readxl, skimr, readxl, car)
#Load Cars Dataset----
library(haven)
Cars_file <- read_sav("Desktop/PSY 5050 ADV QUANT/Cars SPSS file.sav")
#Run Descriptives----
skimr:: skim(Cars_file)
#Nothing missing from MSRP, Cars_City, both distributions skewed right
jmv::descriptives(
  data = Cars_file,
  vars = vars(MSRP, MPG_City),
  freq = TRUE,
  hist = TRUE)
#Correlations----
jmv::corrMatrix(
  data = Cars_file,
  vars = vars(MPG_City, MSRP))
#r = -.48, p < .001
#Linear Regression----
model1 <- lm(MSRP ~ MPG_City, data = Cars_file)
summary(model1)
#               Estimate Std. Error t value Pr(>|t|) 
#(Intercept)  68124.6     3278.9   20.78   <2e-16 ***
#MPG_City     -1762.1      158.2  -11.14   <2e-16 ***
##Create Linear Regression Plot----
scatterplot(MSRP ~ MPG_City, data = Cars_file,
  xlab = "Miles per Gallon in the City", ylab = "MSRP Value",
  main = "Enhanced Scatter Plot")
abline(model1, lwd = 3, col = "red") #adds red line to plot
#Polynominal Regression----
model2 <- lm(MSRP ~ MPG_City + I(MPG_City^2), data = Cars_file)
summary(model2)
#                Estimate Std. Error t value Pr(>|t|)    
#(Intercept)   113669.058   6754.502  16.829  < 2e-16 ***
#  MPG_City       -5497.548    514.900 -10.677  < 2e-16 ***
#  I(MPG_City^2)     68.381      9.025   7.577 2.23e-13 ***
lines(smooth.spline(predict(model2)), col = "blue", lwd = 3)
