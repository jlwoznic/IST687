age <- c(25,35,50)
salary<- c(200000,1200000,2000000)
df <- data.frame("Age" = age, "Salary" = salary, stringsAsFactors = FALSE)

normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

dfNorm <- as.data.frame(lapply(df[2],normalize))

dfNorm <- as.data.frame(lapply(df["Salary"],normalize))

dfNormZ <- as.data.frame( scale(df[1:2]))

# --- Not sure if naything below is helpful
# My approach was to normalize the data
# create a column for normalized mpg
myCars$normMPG <- scale(myCars[1:1])
# create a column for normalized hp
myCars$normHP <- scale(myCars[4:4])
# create a column that is the ratio of normalized information
myCars$normMPG2HP <- myCars$normMPG/myCars$normHP
maxRatio <- max(myCars$normMPG2HP)
myCars[myCars$normMPG2HP == maxRatio, "normMPG2HP", drop = FALSE]

# but approach
# combine hp and mpg
myCars$mpg + myCars$hp
myCars$combined <- myCars$mpg + myCars$hp
myCars$norm <- scale(myCars[17:17])
maxNorm <- max(myCars$norm)
myCars[myCars$norm == maxNorm, "norm", drop = FALSE]

