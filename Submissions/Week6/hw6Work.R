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

# Step 2: Clean the data
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

# function for correct packages
install.packages ("ggplot2")
library(ggplot2)

# histogram for Ozone with ggplot
g <- ggplot(myairQ, aes(x=Ozone))
g <- g + geom_histogram(bins=20, color="blue", fill="green")
g <- g+ ggtitle("Air Quality:Ozone")
g
gdata <- ggplot_build(g)
gdata[1]
gdata$data[[1]]$y

# histogram for Solar.R with ggplot
g <- ggplot(myairQ, aes(x=Solar.R))
g <- g + geom_histogram(bins=20, color="red", fill="orange")
g <- g+ ggtitle("Air Quality:Solar.R")
g

# histogram for Wind with ggplot
g <- ggplot(myairQ, aes(x=Wind))
g <- g + geom_histogram(bins=20, color="black", fill="blue")
g <- g+ ggtitle("Air Quality:Wind")
g

# histogram for Temperature with ggplot
g <- ggplot(myairQ, aes(x=Temp))
g <- g + geom_histogram(bins=20, color="blue", fill="pink")
g <- g+ ggtitle("Air Quality:Temperature")
g

# - do we even need these two? They are not 'variables'
# histogram for Month with ggplot
g <- ggplot(myairQ, aes(x=Month))
g <- g + geom_histogram(binwidth=1, bins=5, color="green", fill="blue")
g <- g+ ggtitle("Air Quality:Month")
g

# histogram for Day with ggplot
g <- ggplot(myairQ, aes(x=Day))
g <- g + geom_histogram(bins=15, color="orange", fill="yellow")
g <- g+ ggtitle("Air Quality:Day")
g

# 2) Bloxplot for Ozone with ggplot
# Need to find "buckets" for the information
# DO NOT MAP AGAINST MONTH!
g<- ggplot(myairQ, aes(group=Month,x=Month,y=Ozone)) 
g<- g + geom_boxplot(aes(fill=factor(Month)))
g<- g + ggtitle("Ozone by Month")
g

# 3) Boxplot for wind values (round the wind to get a good number of "buckets")
# DO NOT MAP AGAINST MONTH!
g<- ggplot(myairQ, aes(group=Month,x=Month,y=round(Wind, digits=0)))  
g<- g + geom_boxplot(aes(fill=factor(Month)))
g<- g + ggtitle("Wind by Month")+theme(plot.title=element_text(hjust=0.5))
g
# maybe don't use month - maybe do not have "wind" on the Y axis (use buckets?)

# Step 3 (again): Explore how the data changes over time
#       First make sure to create appropriate dates (this data was from 1973). 
#       Then create line charts for ozone, temp, wind and solar.R 
#         (one line chart for each, then one chart with 
#          four lines, each having a different color).
#       create these visualizations with ggplot
#       **Note that for the chart with 4 lines, you need to think about how to 
#         effectively use the y-axis

# create a function to create dates from 1973
# read month, day and create 1973-<month>-<day>
createDate <- function (yyyy,mm,dd)
{
  as.Date(paste(yyyy,mm,dd,sep='-'))
}

# function to create a new column from the created date
makeDateCol <- function(df)
{
  for (row in 1:nrow(df))
  {
    dateVal<-createDate(1973,df$Month,df$Day)
  }
  return(dateVal)
}
# now add the new column
myairQ$Date<-makeDateCol(myairQ)

g <- ggplot(myairQ, aes(x=Month, y=Ozone, group=Day, color=factor(Day)))
g <- g + geom_line(size=1)
g <- g + ylab("Ozone")
g <- g + ggtitle("Day of Month")
g

# Step 4: Look at all the data via a Heatmap
#         Create a heatmap, with each day along the x-axis and 
#         Ozone, Temp, Wind and Solar.R along the y-axis
#         Create using geom_tile (tiles instead of lines)
#         You will need to figure out how to show the relative change
#         equally across all the variables
g<- ggplot(myairQ, aes(x=Day, y=??))
g<- g + geom_tile(aes(color=Solar.R, size=Ozone))
g

# Step 5: Look at all the data via a scatter plot
#         Create a scatter chart (geom_point), 
#         x-axis is wind
#         y-axis is temperature
#         dot size represents Ozone
#         color represents Solar.R
g<- ggplot(myairQ, aes(x=Wind, y=Temp))
g<- g + geom_point(aes(color=Solar.R, size=Ozone))
g

# Step 6: Final Analysis
#         Do you see any patterns after exploring the data?
#         ANSWER: Wind decreases in late summer, but is fairly high in the late Spring
#                 Lower the wind, lower the temperature, higher the Ozone?
#                 ** MORE **
#
#         What was the most useful visualization?
#         ANSWER: **ANSWER**

