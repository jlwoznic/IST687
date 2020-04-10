# 
# Course: IST687
# Name: Joyce Woznica
# Homework 6 - Viz HW: air quality Analysts
# Due Date: 2/19/2019
# Date Submitted:
#
# Step 1: Load the data
# we will use the airquality data set, which you should already have as part of your R installation
myairQ<-airquality

# Step 2: Clean the datas
#         There will be NA's - figure out what you are going to do with that
#         JLJW NOTE: I elected to use the mean for each column for the NAs as opposed to 
#                    removing all the observations that had an NA for one of the values

# create a function to replace each column NA with mean for that column
replaceNAwMeans<-function(vec)
{
  numcols<-length(colnames(vec))
  index<-1
  while(index<=numcols)
  {
    theColV <- vec[,index]
    theColV[is.na(theColV)]<-mean(theColV,na.rm=TRUE)
    vec[,index]<-theColV
    index<-index+1
  }
  return(vec)
}

# Update the dataframe with the modified frame with means applied to NA values
myairQ<-replaceNAwMeans(myairQ)

# Step 3: Understand the data distribution
#         Create the following visualizations using ggplot:
# 1) Histogrames for each of the variables
#    Get the columns of myairQ
colnames(myairQ)

# histogram for Ozone
# with ggplot
# function for correct packages
install.packages ("ggplot2")
library("ggplot2")

g <- ggplot(myairQ, aes(x=myairQ$Ozone))
g <- g + geom_histogram(bins=20, color="blue", fill="green")
g <- g+ ggtitle("Air Quality:Ozone")
g

# with hist
hist(myairQ$Ozone, 
     main="Histogram for Air Quality: Ozone", 
     xlab="Ozone", 
     border="blue", 
     col="green",
     xlim=c(min(myairQ$Ozone),max(myairQ$Ozone)),
     las=1, 
     breaks=8)

# histogram for Solar.R
# with ggplot
g <- ggplot(myairQ, aes(x=myairQ$Solar.R))
g <- g + geom_histogram(bins=20, color="red", fill="orange")
g <- g+ ggtitle("Air Quality:Solar.R")
g

# with hist
hist(myairQ$Solar.R, 
     main="Histogram for Air Quality: Solar.R", 
     xlab="Solar.R", 
     border="red", 
     col="orange",
     xlim=c(min(myairQ$Solar.R),max(myairQ$Solar.R)),
     las=1, 
     breaks=8)

# histogram for Wind
# with ggplot
g <- ggplot(myairQ, aes(x=myairQ$Wind))
g <- g + geom_histogram(bins=20, color="black", fill="blue")
g <- g+ ggtitle("Air Quality:Wind")
g

# with hist
hist(myairQ$Wind, 
     main="Histogram for Air Quality: Wind", 
     xlab="Wind", 
     border="black", 
     col="blue",
     xlim=c(min(myairQ$Wind),max(myairQ$Wind)),
     las=1, 
     breaks=8)

# histogram for Temperature
# with ggplot
g <- ggplot(myairQ, aes(x=myairQ$Temp))
g <- g + geom_histogram(bins=20, color="blue", fill="pink")
g <- g+ ggtitle("Air Quality:Temp")
g

# hist
hist(myairQ$Temp, 
     main="Histogram for Air Quality: Temp", 
     xlab="Temp", 
     border="blue", 
     col="pink",
     xlim=c(min(myairQ$Temp),max(myairQ$Temp)),
     las=1, 
     breaks=8)

# histogram for Month
# with ggplot
# ** THIS NEEDS FIXING!
g <- ggplot(myairQ, aes(x=myairQ$Month))
g <- g + geom_histogram(binwidth=1, bins=5, color="green", fill="blue")
g <- g+ ggtitle("Air Quality:Month")
g

# with hist
hist(myairQ$Month, 
     main="Histogram for Air Quality: Month", 
     xlab="Month", 
     border="green", 
     col="blue",
     xlim=c(min(myairQ$Month),max(myairQ$Month)),
     las=1, 
     breaks=max(myairQ$Month)-min(myairQ$Month))

# histogram for Day
# with ggplot
# ** WORK ON THIS TOO!
g <- ggplot(myairQ, aes(x=myairQ$Day))
g <- g + geom_histogram(bins=15, color="orange", fill="yellow")
g <- g+ ggtitle("Air Quality:Day")
g

# with hist
hist(myairQ$Day, 
     main="Histogram for Air Quality: Day", 
     xlab="Day", 
     border="orange", 
     col="yellow",
     xlim=c(min(myairQ$Day),max(myairQ$Day)),
     las=1, 
     breaks=max(myairQ$Day)-min(myairQ$Day))

# 2) Bloxplot for Ozone
# with ggplot
# Need to find "buckets" for the information
ggplot(myairQ,aes(x=factor(0),myairQ$Ozone)) + geom_boxplot()

# with boxplot
boxplot(myairQ$Ozone,data=myairQ, notch=TRUE, main="Air Quality: Ozone",
        xlab="Not Sure", ylab="Ozone Level") 

# 3) Boxplot for wind values (round the wind to get a good number of "buckets")


# Step 3(again): Explor how the data changes over time
#       First make sure to create appropriate dates (this data was from 1973). 
#       Then create line charts for 
#       ozone, temp, wind and solar.R (one line chart for each, then one chart with 
#       four lines, each having a different color).
#       create these visualizations with ggplot
#       **Note that for the chart with 4 lines, you need to think about how to 
#         effectively use the y-axis
g <- ggplot(myairQ, aes(x=myairQ$Month), group=myairQ$Day, color=myairQ$Day)
g <- g + geom_line(aes(y=myairQ$Ozone), linetype="solid", size=1)
g <- g + ylab("Ozone")
g <- g + ggtitle("Day of Month")
g

# Step 4: Look at al the data via a Heatmap
#         Create a heatmap,

