# 
# Course: IST687
# Name: Joyce Woznica
# Homework 4 - Samples HW
# Due Date: 2/5/2019
# Date Submitted:
# 
# Step 1: Write a summarizing function to understand the distribution of a vector
# 1) The function, call it 'printVecInfo' should take a vector as input
# 2) The function should print the following:
#   a) Mean
#   b) Median
#   c) Min & Max
#   d) Standard Deviation
#   e) Quantiles (at 0.05 and 0.95)
#   f) Skewness

# install package for skewness function
install.packages("moments")
library(moments)

# install package for counting strings
install.packages("stringr")
library(stringr)

# This function creates a clean dataframe of the results from a quantile
# function call with probsV as a vector of the alpha and the 1-alpha
# I use this in the printVecInfo function
myQuantDF<-function(xVect,probsV)
{
  quant<-quantile(xVect,probsV)
  quantDF <- data.frame(id=names(quant), values=unname(quant), stringsAsFactors = FALSE)
  return(quantDF)
}

# the printVecInfo function
printVecInfo <- function(vect)
{
  # change these to print statements
  # need to put these in a dataframe that gets returned
  # what statistic, value
  retv<-c(cat(sprintf("Mean is: %f\n", mean(vect))),
          cat(sprintf("Median is: %f\n", median(vect))),
          cat(sprintf("Minimum is: %f\n", min(vect))),
          cat(sprintf("Maximum is: %f\n", max(vect))), 
          cat(sprintf("Standard deviation is: %f\n", sd(vect))))
  # now I work with a nice way to print out the quantile function
  # hardcoded to be alpha of 0.05
  quantDF<-myQuantDF(vect,c(0.05,0.95))
  index<-1 # for the index of the vector
  while (index <= 2)
  {
    retv<-append(retv, cat(sprintf("Quantile for %s is: %f\n", quantDF$id[index], quantDF$values[index])))
    index<-index+1
  }
  retv<-append(retv,cat(sprintf("Skewness is: %f\n", skewness(vect))))
}

# 3) Use this vector to test function
myVect<-c(1,2,3,4,5,6,7,8,9,10,50)
printVecInfo(myVect)

# Step 2: Creating Samples in a jar
# 4) Create a variable 'jar' that has 50 red and 50 blue marbles
#    hint: the jar can have strings being 'red' and 'blue'
red<-"red"
blue<-"blue"
v.red<-replicate(50,red)
v.blue<-replicate(50,blue)
jar<-c(v.red,v.blue)

# 5) Confirm that there are 50 reds by summing the samples that are red
length(jar[jar=='red'])

# 6) Sample 10 marbles from the jar, how many are red? 
#    What percentage are red?
mySample<-sample(jar,10)
length(mySample[mySample==red])

# I decided to create a percent function
percentColor <- function(theVector,theColor)
{
  thePercent<-length(which(theVector==theColor))/length(theVector)
  return(thePercent)
}

sprintf("%1.2f%%", 100*percentColor(mySample, red))

# 7) Do the sampling 20 times, using the 'replicate' command. This should generate
#    a list of 20 numbers. Each number is the mean of how many reds there were in 10
#    samples. Use the printVectInfo to see information of the samples. Also generate
#    a histogram of the samples.
#    note: use method #2

# sum up the 1's (matches) of 'red' in the size 10 sample from jar
# to determine how many reds in 10 samples of 20 numbers
sum(replicate(20,mean(str_count(sample(jar,size=10,replace=TRUE),red))))
testvec1<-replicate(20,mean(str_count(sample(jar,size=10,replace=TRUE),red)))
printVecInfo(testvec1)
hist(testvec1)

# 8) Repeat #7, but this time, sample the jar 100 times. You should get 20 numbers
#    this time each number represents the mean of how many reds there are in the
#    100 samples. 
#    Use your PrintVecInfo to see information of the samples. Also generate a
#    histogram of the samples.
testvec2<-replicate(20,mean(str_count(sample(jar,size=100,replace=TRUE),red)))
printVecInfo(testvec2)
hist(testvec2)

# 9) Repeat #8, but this time, replicate the sampling 100 times. You should get 100
#    numbers, this time each number represents the mean of how many reds there were
#    in the 100 samples. use you printVecInfo to see information about the samples.
#    Also generate a histogram of the samples.
testvec3<-replicate(100,mean(str_count(sample(jar,size=100,replace=TRUE),red)))
printVecInfo(testvec3)
hist(testvec3)

# Step 3: Explore the airquality dataset
# 10) Store the 'airquality' dataset into a temporary variable
myairQ<-airquality

# 11) Clean the dataset by removing all the NAs
#     JLJW NOTE: I elected to use the mean for each column for the NAs as opposed to 
#                removing all the observations that had an NA for one of the values

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

# 12) Explore the Ozone, Wind and Temp by doing a 'printVecInfo' on each as well as 
#     generating a histogram for each
# first for Ozone
printVecInfo(myairQ$Ozone)
hist(myairQ$Ozone)

# Next for Wind
printVecInfo(myairQ$Wind)
hist(myairQ$Wind)

# Next for Temp
printVecInfo(myairQ$Temp)
hist(myairQ$Temp)