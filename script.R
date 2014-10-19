setwd("./5.ReproducibleResearch")

## Load required packages.
##############################################################################
install.packages("plyr")
library(plyr)


## Load and process the data.
##############################################################################

# Define Url for download.
if (!file.exists("data.zip")) 
  {url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"}
# Download file.
download.file (url, "data.zip")

#Unzip file.
if (!file.exists ("activity.csv")) {unzip ("data.zip")}

# Read file into an R object.
data <- read.csv("activity.csv",stringsAsFactors=FALSE)

# Create a new factor variable representing the days.
data$fDate <- as.factor (data$date)

# Convert date column to time.
data$date <- strptime(data$date, format="%Y-%m-%d")

###############################################################################


## Mean number of steps per day (NAs ignored) using mean and median.
###############################################################################

# Create a new table calculating the sums of steps taken each day.
data_step_sums_day <- ddply (data, .(date), summarise, stepsum=sum(steps))

# Make a histogram of steps per day
hist(data_step_sums_day$stepsum, col="blue", main="Distribution of total 
     number of steps taken per day", xlab="Total number of stpes",
     ylab="Number of days")

# Output the mean and median for each day.
data_step_mean_day <- aggregate(steps~fDate, data=data, FUN=mean)
data_step_mean_day$fDate <- strptime(as.character(data_step_mean_day$fDate), 
                                     format="%Y-%m-%d")
data_step_median_day <- aggregate(steps~fDate, data=data, FUN=median)
data_step_median_day$fDate <- strptime(as.character(data_step_median_day$fDate), 
                                     format="%Y-%m-%d")
par(mfrow=c(1,2), mar=c(4,2,2,1))
with(data_step_mean_day, { plot (fDate, steps, ylab="average steps", 
                          xlab="date", main="Steps mean per day",
                          col="red", pch=20)})
with(data_step_median_day, { plot (fDate, steps, ylab="median steps", 
                            xlab="date", main="Median steps per day",
                            col="green", pch=20)})

################################################################################


## Plot (with line) average daily activity pattern: time series (x-time, y-steps).
#################################################################################

data_step_mean_interval <- aggregate (steps~interval, data=data, FUN=mean)
par (mfrow=c(1,1), mar=c(4,4,2,1))
with (data_step_mean_interval, { plot(interval, steps, type="l", 
                  xlab="interval identifier",
                  ylab="average steps",
                  main="Average number of steps per interval",
      )})
max <- max(data_step_mean_interval$steps)
print(data_step_mean_interval$interval[which.max(data_step_mean_interval$steps)])

##################################################################################


## Input missing values.
##################################################################################

# Total number of missing values
any (is.na(data$date))
any (is.na(data$interval))
any (is.na(data$steps))
print (sum(is.na(data$steps)))

# Filling in of missing values. The values will be imputed 
# using average values from the corresponding intervals.
colnames(data_step_mean_interval)[2] <- "average"
fdata <- merge(data, data_step_mean_interval, by="interval")

# New data frame with filled in missing data
fdata$steps[is.na(fdata$steps)] <- fdata$average[is.na(fdata$steps)]
fdata <- fdata[,-c(4,5)]
fdata$date <- as.factor(as.character(fdata$date))

# Histogram of the total number of steps taken each day.
fdata_step_sums_day <- ddply (fdata, .(date), summarise, stepsum=sum(steps))
hist(fdata_step_sums_day$stepsum, col="blue", main="Distribution of total 
     number of steps taken per day", xlab="Total number of stpes",
     ylab="Number of days")

# Calculate and report mean and median.
fdata_step_mean_day <- aggregate(steps~date, data=fdata, FUN=mean)
fdata_step_mean_day$fDate <- strptime(as.character(fdata_step_mean_day$date), 
                                     format="%Y-%m-%d")
fdata_step_median_day <- aggregate(steps~date, data=fdata, FUN=median)
fdata_step_median_day$fDate <- strptime(as.character(fdata_step_median_day$date), 
                                       format="%Y-%m-%d")

par(mfrow=c(1,2), mar=c(4,2,2,1))
with(fdata_step_mean_day, { plot (fDate, steps, ylab="average steps", 
                                 xlab="date", main="Steps mean per day",
                                 col="red", pch=20)})
with(fdata_step_median_day, { plot (fDate, steps, ylab="median steps", 
                                   xlab="date", main="Median steps per day",
                                   col="green", pch=20)})


## Plot weekdays versus weekends with filled-in missing values.
weekend <- c("Sat", "Sun")
fdata$date <- as.POSIXlt(as.character(fdata$date))
fdata$day <- weekdays(fdata$date, abbreviate=TRUE)
fdata[fdata$day %in% weekend,4] <- "Weekend" 
fdata[fdata$day != "Weekend",4] <- "Weekday"
fdata_weekday <- fdata[fdata$day=="Weekday",]
fdata_weekend <- fdata[fdata$day=="Weekend",]
fdata_step_weekday <- aggregate (steps~interval, data=fdata_weekday, FUN=mean)
fdata_step_weekend <- aggregate (steps~interval, data=fdata_weekend, FUN=mean)
par (mfrow=c(2,1), mar=c(4,4,2,1))
with (fdata_step_weekday, { plot(interval, steps, type="l", 
                                      xlab="interval identifier",
                                      ylab="average steps",
                                      main="Weekdays"
)})
with (fdata_step_weekend, { plot(interval, steps, type="l", 
                                 xlab="interval identifier",
                                 ylab="average steps",
                                 main="Weekend"
)})
