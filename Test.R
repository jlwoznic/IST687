# Require RODBC for use in this script
require(RODBC)

# Create a connection to SQL Server using our 64-bit DSN
myconn <- odbcConnect ("VidCastTest")

# Ready the SQL to send to the server
sqlSelectStatement <-
"SELECT
    vc_VidCast.vc_VidCastID,
    vc_VidCast.VidCastTitle,
    DATEPART(dw, StartDateTime) as StartDayofWeek,
    DATEDIFF(n, StartDateTime, EndDateTime) as ActualDuration,
    ScheduleDurationMinutes,
    vc_User.vc_UserID,
    vc_User.UserName
FROM vc_VidCast
JOIN vc_User ON vc_User.vc_UserID = vc_VidCast.vc_UserID"

# send the request to the server and store the results in a variable
sqlResult <- sqlQuery(myconn, sqlSelectStatement)

# Create a list of days of the week for charting later
days <- c("Sun", "Mon", "Tues", "Weds", "Thurs", "Fri", "Sat")

# Create a histogram of durations (appears in the Plots tab)
hist(sqlResult$ActualDuration,
     main="sa How long are the VidCasts?",
     xlab="Minutes",
     ylab="VidCasts",
     border="blue",
     col="grey",
     labels=TRUE)

# Plot a bar chart of video counts by day of the week
dayCounts <- table(sqlResult$StartDayOfWeek)
barplot(dayCounts,
        main="sa VidCasts by Day of week",
        ylab="Day of Week",
        xlab="Count of VidCasts",
        border="blue",
        names.arg=days)

# Close all connections
odbcCloseAll()

# Fin