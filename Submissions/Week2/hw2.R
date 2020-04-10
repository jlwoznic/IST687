# 
# Course: IST687
# Name: Joyce Woznica
# Homework 2
# Due Date: 1/20/2019
# Date Submitted:
#
# using mtcars dataset
# store this in a new variable (myCars)

myCars <- mtcars

# adds the row name (which is the car name) as a variable (column)
myCars$CarName <-row.names(myCars)

# Step 1: What is the hp(hp stands for "horse power")
# 1) What is the highest hp?
#    ANSWER: 335 hp
maxHP = max(myCars$hp)
# another way is to use
subset(myCars, hp == maxHP)[,"hp"]

# 2) Which car has the highest hp?
#    ANSWER: Maserati Bora with 335 hp
# the code below looks for the row where the 'hp' is equal to the maxHP and
# drops all the rows that are not TRUE (the only TRUE value would be the one with the maxHP)
myCars[myCars$hp == maxHP, "hp", drop = FALSE]
# could have also ordered the data descending and picked the top row
myCarsOrderedbyHP <- myCars[order(-myCars$hp),]
head(myCarsOrderedbyHP, n=1)

# Step 2: Explore mpg (miles per gallon)
# 3) What is the highest mpg?
#    ANSWER: 33.9 mpg
maxMPG <- max(myCars$mpg)

# 4) Which car as the highest mpg?
#    ANSWER: Toyota Corolla with 33.9 mpg
myCars[myCars$mpg == maxMPG, "mpg", drop = FALSE]

# 5) Create a sorted dataframe, based on the mpg
myCarsSortedbyMPG <- myCars[order(myCars$mp),]

# Step 3: which car has the best combination of mpg and hp?
# 6) What logic did you use?
#    ANSWER: For this question, I determined the higher the mpg/hp ratio is what
#            will be considered the "best combination".
# 7) Which car as the best combination of mpg and hp?
#    ANSWER: Honda Civic with a ratio of 0.5846154
# adds the row name (which is the ratio of mpg over hp
myCars$mpg2hp <- myCars$mpg/myCars$hp
# This provides the row with the best mpg2hp
head(myCars[order(-myCars$mpg2hp),],n=1)
# Alternatively, I can locate the max by just finding the only row that matches it
maxMPG2HP <- max(myCars$mpg2hp)
myCars[myCars$mpg2hp == maxMPG2HP, "mpg2hp", drop = FALSE]
# another option would be to use which.max and the CarName variable to determine the correct car
myCars$CarName[which.max(myCars$mpg2hp)]

# Step 4: Which car as the "best" car combination of mpg and hp, where mpg and hp
#         must be given equal weight
#         DO NOT use a ratio, considering adding hp and mpg (mentioned in class)
#         Again, I determined that the largest number when scaled and combined for 
#         mpg and hp would be considered "best" for this question
#         ANSWER: Maserati Bora with the maximum scaled value of 1.901923
myCars$scaled <- scale(myCars$mpg) + scale(myCars$hp)
maxScaled <- max(myCars$scaled)
myCars[myCars$scaled == maxScaled, "scaled", drop = FALSE]
# another option would be to use which.max and the CarName variable to determine the correct car
myCars$CarName[which.max(myCars$scaled)]
