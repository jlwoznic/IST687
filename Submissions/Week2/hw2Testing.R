age<-c(40,50,60)
wt<-c(100,130,150)
myDf<-data.frame(age,wt)

age<-age+3
myDf$age<-age

myDf

# to list out existing data sets
data()

# structure of a data frame
str(mtcars)

myCars<-mtcars
row.names(myCars)
?mtcars

# adds the row name (which is the car name) as a column
myCars$CarName <-row.names(myCars)

summary(myCars)
max(myCars$hp)
which.max(myCars$hp) # provides the row number for the max
myCars$CarName[max(myCars$hp)] # doesn't do what I want
