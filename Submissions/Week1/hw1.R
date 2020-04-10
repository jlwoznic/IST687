# 
# Course: IST687
# Name: Joyce Woznica
# Homework 1
# Due Date: 1/13/2019
# Date Submitted:
#
# Define vectors for height and weight
height <- c(59,60,61,58,67,72,70)
weight <- c(150,140,180,220,160,140,130)

# define a variable "a"
a <- 150

# Step 1: Calculating means
#
# 1) Compute, using R, the average height (called mean in R)
# 2) Compute, using R, the average weight (called mean in R)
meanHeight <- mean(height)
meanHeight
meanWeight <- mean(weight)
meanWeight

#
# 3) Calculate the length of the vector 'height' and 'weight'
lenHeight <- length(height)
lenHeight
lenWeight <- length(weight)
lenWeight

#
# 4) Calculate the sum of the heights
sumHeight <- sum(height)
sumHeight
sumWeight <- sum(weight)
sumWeight

#
# 5) Compute the average of both height and weight by dividing
#    the sum by the length of the vector.
avgHeight <- sumHeight/lenHeight
avgHeight
avgWeight <- sumWeight/lenWeight
avgWeight

#    How does this compare to the 'mean' function
#    ANSWER: They are the same

# Step 2: Using max/min functions
#
# 6) Compute the max height, store the result in 'maxH'
# 7) Compute the min weight, store the result in 'minW'
maxH <- max(height)
minW <- min(weight)

# Step 3: Vector Math
#
# 8) Create a new vector, which is the weight+5 (every person 
#    gained 5 pounds)
# 9) Compute the weight/height for each person, using the new weight
#    just created
newWeight <- weight+5
newWeight
nwhVector <- newWeight/height
nwhVector

# Step 4: Using Conditional if statements
#
# 10) Write the R code to test if max height is greater than 60 
#     (output "yes" or "no")
# 11) Write the R code to test if min weight is greater than 
#     the variable 'a' - which is 150 (output "yes" or "no")
if (maxH > 60) "yes" else "no"
if (minW > a) "yes" else "no"

