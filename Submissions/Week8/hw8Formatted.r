# 
# Course: IST687
# Name: Joyce Woznica
# Homework 8 - Making Predictions
# Due Date: 03/05/2019
# Date Submitted:
#
# Package Section
# ------------------------------------------------------------------
install.packages("readxl")
library(readxl)

# 1. Read in the data
# 2. Option to save to computer (had to based on bad URL)
# read in a dataset so that it can be useful.
# use this package to read in a XLS file

fawnData<-read_excel("C:/Users/Joyce/Desktop/Syracuse/IST687/Submissions/Week8/mlr01_2_2.xlsx")
fawnDF <- as.data.frame(fawnData)

# rename the columns to make easier:
# X1 = SpringFawns
# X2 = AdultPop
# X3 = AnnualPercip
# X4 = WinterRating

newColNames <- c("SpringFawns", "AdultPop", "AnnualPrecip", "WinterRating")
colnames(fawnDF)<-newColNames
View(fawnDF)

# 3. Confirm the data
# confirming all numbers, correct # of variables and observations
str(fawnDF)

# 4. Create bivariate plots of baby fawns versus:
#    a) Adult Population
#    b) Annual Percipitation
#    c) Severity of Winter
#   Independent variables on X axis, Dependent (SpringFawns) on the Y axis
plot(fawnDF$SpringFawns ~ fawnDF$AdultPop, xlab="Adult Antelope Population", ylab="Number of Spring Fawns")
plot(fawnDF$SpringFawns ~ fawnDF$AnnualPrecip, xlab="Annual Percipitation", ylab="Number of Spring Fawns")
plot(fawnDF$SpringFawns ~ fawnDF$WinterRating, xlab="Severity of Winter", ylab="Number of Spring Fawns")

# 5. Create 3 regression models of increasing complexity using lm().
#    a) Predict Number of Fawns from Severity of Winter
#    b) Predict Number of Fawns from Severity of Winter (and another variable)
#    c) Predict Number of Fawns from all 3 variables
oneVarM<-lm(formula=SpringFawns ~ WinterRating, data=fawnDF)
twoVarM<-lm(formula=SpringFawns ~ WinterRating + AdultPop, data=fawnDF)
threeVarM<-lm(formula=SpringFawns ~., data=fawnDF)

# inspect each model and see which is best
oneVarMSum<-summary(oneVarM)
twoVarMSum<-summary(twoVarM)
threeVarMSum<-summary(threeVarM)
ModResidsV<-residuals(threeVarM)
ModCoeffV<-coefficients(threeVarM)
ModelsRSq<-c(oneVarMSum$adj.r.squared, twoVarMSum$adj.r.squared, threeVarMSum$adj.r.squared)

# Which model works best?
# ANSWER: The model with all three variables i best because the Adjusted (multiple) R-squared is .955
#         each independent variable shows a p-value below the 0.05 alpha.
AICModels<-step(threeVarM,data=fawnDF, direction="backward")

# the model would include all variables, as we see using the AIC method and to determine the 
# optimal selection of independent variables, so the fewest is actually ALL the variables

# Bonus #5 on second page
# I was surprised to lean that the Annual Precipitation had a bearing on the number of fawns
# but then I considered the fact that the vegetation would be affecting and that would have
# an influence