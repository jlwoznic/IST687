# 
# Course: IST687
# Name: Joyce Woznica
# Homework 5 - JSON & tapply Homework: Accident Analysis
# Due Date: 2/12/2019
# Date Submitted:
#
# Library to use

install.packages("RCurl")
install.packages("jsonlite")

library(RCurl)
library(jsonlite)

#install.packages("RJSONIO")
#library(RJSONIO)

# Step 1: Load the data
# 1) read JSON dataset from a URL:
#    http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD

accidentURL<-"http://data.maryland.gov/api/views/pdvh-tf2u/rows.json?accessType=DOWNLOAD"
apiResult<-getURL(accidentURL)
accidentData<- fromJSON(apiResult)

# Other Options - # see Gary's document

# Step 2: Clean the data
# first row is about the data
# second has the data
summary(accidentData[[2]])
View(accidentData[[2]])

accidentData<-accidentData[[2]]
#actual number of accidents
numrow <- length(accidentData) 
# how many columns
numcol <- ncol(accidentData)

# remove the first 8 columns
accidentData <- accidentData[,9:numcol]

# now rename the columns
# should now be 18 columns
ColNameVect <- c("CASE_NUMBER","BARRACK", "ACC_DATE", "ACC_TIME", "ACC_TIME_CODE", "DAY_OF_WEEK", 
                 "ROAD", "INTERSECT_ROAD", "DIST_FROM_INTERSECT", "DIST_DIRECTION", "CITY_NAME",
                 "COUNTY_CODE", "COUNTY_NAME", "VEHICLE_COUNT", "PROP_DEST", "INJURY", 
                 "COLLISION_WITH_1", "COLLISION_WITH_2")

# Update with new column names
colnames(accidentData)<-ColNameVect
View(accidentData)
# need to clean out extra spaces from ends of lines
trim.leading<-function(x) {sub("^\\s+","",x)}
trim.trailing<-function(x) {sub("\\s+$","",x)}
trim<-function(x) {sub("^\\s+|\\s+$","",x)}
accidentData[,6]<-trim(accidentData[,6])

# before using the data - make it a data frame
accidentData<-as.data.frame(accidentData)

# Step 3: Understad the data using SQL (via SQLDF)
install.packages("sqldf")
library(sqldf)

# 1) How many accidents happen on SUNDAY
#    ANSWER: 2373
SQLStatement<-"select count(accidentData.CASE_NUMBER) from accidentData 
             where accidentData.DAY_OF_WEEK = 'SUNDAY'"
numSundayAccidents <- (sqldf(SQLStatement, stringsAsFactors = FALSE))
numSundayAccidents

# 2) How many accidents had injuries (might need to remove NAs from the data)
#    ignore the na's - but did a check...there are no NA's in INJURY field
#    ANSWER: 6433
sqldf("select accidentData.INJURY from accidentData where 
       accidentData.INJURY = 'NA'")
# this returns 0, so nothing with NA
SQLStatement<-"select count(accidentData.INJURY) from accidentData 
               where accidentData.INJURY = 'YES'"
numInjuries<-sqldf(SQLStatement,stringsAsFactors=FALSE)
numInjuries

# 3) List the injuries by Day
#    ANSWER: See results in injuriesbyDay
# requires grouping the count of injuries by the DAY_OF_WEEK
SQLStatement<-"select accidentData.DAY_OF_WEEK, count(accidentData.INJURY) from accidentData 
               where accidentData.INJURY = 'YES' Group by accidentData.DAY_OF_WEEK"
injuriesbyDay<-sqldf(SQLStatement,stringsAsFactors=FALSE)
injuriesbyDay
View(injuriesbyDay)

# Step 4: Understand the data using tapply
#         Answer the following Questions
# Note: Could use the 'attach' function to reference by DAY_OF_WEEK instead
#       of accidentData$DAY_OF_WEEK
# 1) How many accidents happen on Sunday
#    ANSWER: 2373
# find all the match SUNDAY
sunday<-tapply(accidentData$DAY_OF_WEEK, accidentData$DAY_OF_WEEK=='SUNDAY', length)
sunday["TRUE"]

# 2) How many accidents had injuries
#    ANSWER: 6433
injury<-tapply(accidentData$INJURY, accidentData$INJURY=='YES', length)
injury["TRUE"]

# 3) List the injuries by day
#    ANSWER: see console
injurydays<-tapply(accidentData$INJURY, 
                   list(accidentData$INJURY=='YES',accidentData$DAY_OF_WEEK), length)
injurydays["TRUE",]
View(injurydays["TRUE",])
