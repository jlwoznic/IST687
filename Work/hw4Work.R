jar <- c(1,0)
numSamples<-4
# replaces the previous number pulled back into the sample
sample(jar, numSamples, replace=TRUE)
# does NOT replace the previous number so when takes 1, leaves only 0
sample(jar, 2,replace=FALSE)
# takes the mean of the random samples of 4 numbers
mean(sample(jar,numSamples,replace=TRUE))
# builds a dataframe of the samples (5 columns, 4 rows (numSamples))
replicate(5, sample(jar,numSamples,replace=TRUE), simplify = TRUE)
replicate(2, mean(sample(jar,numSamples,replace=TRUE)), simplify = TRUE)
sampleMeans<-replicate(10000, mean(sample(jar,numSamples,replace=TRUE)), simplify = TRUE)
mean(sampleMeans)
sd(sampleMeans)
quantile(sampleMeans,c(0.025,0.975))
hist(sampleMeans)
numSamples<-50
sampleMeans<-replicate(1000, mean(sample(jar,numSamples,replace=TRUE)), simplify = TRUE)
mean(sampleMeans)
sd(sampleMeans)
quantile(sampleMeans,c(0.025,0.975))
hist(sampleMeans)
# how many times to replicate, and how many samples will force toward
# bell curve (normal distribution)
jar2<-c(-1,0,1)
mean(sample(jar2,250,replace=TRUE))

# comparing different samples
samples <- rnorm(10000, 50, 2)
mean(samples)
sd(samples)
hist(samples)

# another distribution
testData <- c(19.09, 19.55, 17.89, 17.73, 25.15, 27.27, 25.24, 21.05, 21.65, 20.92, 22.61, 15.71, 22.04, 22.50, 24.25)
hist(testData)
mean(testData)

# Package call "moments" to load
install.packages("moments")
library(moments)

# now from this package, we can use skewness
skewness(testData)

# loaded 'readStates' from hw3
mean(dfStates$Jul2010)
sample(dfStates$Jul2010, size=16,replace=TRUE)
mean(sample(dfStates$Jul2010, size=16,replace=TRUE))
mean(sample(dfStates$Jul2010, size=160,replace=TRUE))

# returns actual mean
mean(sample(dfStates$Jul2010, size=51,replace=FALSE))

trueMean<-mean(dfStates$Jul2010)
vect<-sample(dfStates$Jul2010, size=51,replace=FALSE)
hist(vect)

# now do this with a replacement of true
vect <- sample(dfStates$Jul2010, size=16, replace=TRUE)
hist(vect)
# if repeated - it changes each time
# now replicate to be better
vect <-replicate(100, mean(sample(dfStates$Jul2010, size=16,replace=TRUE)), simplify = TRUE)
hist(vect)
mean(vect)
vect <-replicate(1000, mean(sample(dfStates$Jul2010, size=16,replace=TRUE)), simplify = TRUE)
hist(vect)
mean(vect)


#####  more NA examples
#      create a sample data frame with random NA's
#
m <- matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10)
d <- as.data.frame(m)
d
any(is.na(d))        # do any NA's exist, returns T or F
length(d[d=='NA'])   # how many NA's
#
#
if (any(is.na(d)))           # if function to count NA's if they exist
{
  count<-length(d[d=='NA'])   # how many NA's
  print(c("NA Count ", count))
}  else
{
  print("no NA's")
}

d[is.na(d)] <- 0     # replace NA's with zero, entire df
d
#
m <- matrix(sample(c(NA, 1:10), 100, replace = TRUE), 10)  # re-create a matrix with random NA's
d <- as.data.frame(m)
d
d$V1[is.na(d$V1)] <- mean(d$V1,na.rm=TRUE)  # replce NA's in col with mean of the col
d

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

myairQ<-airquality
theColV<-myairQ[,2]
theColV
theColV[is.na(theColV)] <- mean(theColV,na.rm=TRUE)
theColV
myairQ
myairQ[,2]<-theColV
newAir<-replaceNAwMeans(myairQ)
myairQ
newAir
