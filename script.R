setwd("./[DataScience]/5.[ReproducibleResearch]/PA1/data")

## Download and process the data:
install.packages("plyr")
library(plyr)
if (!file.exists("data.zip")) {
  url <- "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
  download.file (url, "data.zip")
}   # Download data.
if (!file.exists ("activity.csv")) {unzip ("data.zip")} # Unzip data.
data <- read.csv("activity.csv",stringsAsFactors=FALSE) # Read file.
# Process data:
data$fDate <- as.factor (data$date)
data$date <- strptime(data$date, format="%Y-%m-%d") # Convert to time.

## Mean/Median steps per day (NAs ignored):
data_step_sums_day <- ddply (data, .(date),
                             summarise, stepsum=sum(steps)) # Sum per day.
hist(data_step_sums_day$stepsum, col="blue", main="Distribution of total 
     number of steps taken per day", xlab="Total number of stpes",
     ylab="Days") # Histogram for sum per day.
# Output the mean and median for each day.
data_step_mean_day <- aggregate(steps~fDate, 
                                data=data, FUN=mean) # Takes the mean per day.
data_step_mean_day$fDate <- strptime(as.character(data_step_mean_day$fDate), 
                                     format="%Y-%m-%d") # Formats as "date".
data_step_median_day <- aggregate(steps~fDate, 
                                  data=data, FUN=median) # Takes median/day.
data_step_median_day$fDate <- strptime(as.character(data_step_median_day$fDate), 
                                     format="%Y-%m-%d") # Formats as "date".
par(mfrow=c(1,2), mar=c(4,2,2,1))
with(data_step_mean_day, { plot (fDate, steps, ylab="average steps", 
                          xlab="date", main="Steps mean per day",
                          col="red", pch=20)}) # Plots mean.
with(data_step_median_day, { plot (fDate, steps, ylab="median steps", 
                            xlab="date", main="Median steps per day",
                            col="green", pch=20)}) # Plots median.

## Plot average daily activity pattern:
data_step_mean_interval <- aggregate (steps~interval, data=data, FUN=mean)
par (mfrow=c(1,1), mar=c(4,4,2,1))
with (data_step_mean_interval, { plot(interval, steps, type="l", 
                  xlab="5-min-interval identifier",
                  ylab="average steps",
                  main="Average number of steps per interval",
      )}) # Graph steps vs time of day (5-min-interval).
print(data_step_mean_interval$interval[which.max(data_step_mean_interval$steps)])

## Input missing values:
# Total number of missing values.
any (is.na(data$date))
any (is.na(data$interval))
any (is.na(data$steps))
print (sum(is.na(data$steps)))

colnames(data_step_mean_interval)[2] <- "average" # Name column containing averages.
fdata <- merge(data, data_step_mean_interval, by="interval") # Add averages.
# Substitute NA with average value.
fdata$steps[is.na(fdata$steps)] <- fdata$average[is.na(fdata$steps)] 
fdata <- fdata[,-c(4,5)] # Remove unnecessary columns.
fdata$date <- as.factor(as.character(fdata$date)) # Convert date to factor.
# Histogram of the total number of steps taken each day.
fdata_step_sums_day <- ddply (fdata, .(date), summarise, stepsum=sum(steps))
hist(fdata_step_sums_day$stepsum, col="blue", main="Distribution of total 
     number of steps taken per day", xlab="Total number of stpes",
     ylab="Days")
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
fdata$day <- weekdays(fdata$date, abbreviate=TRUE) # Get weekday names for date.
fdata[fdata$day %in% weekend,4] <- "Weekend" # Convert to weekend.
fdata[fdata$day != "Weekend",4] <- "Weekday" # Convert to weekday.
fdata_weekday <- fdata[fdata$day=="Weekday",] # Split fdata based
fdata_weekend <- fdata[fdata$day=="Weekend",] # on weekday.
# Get average for intervals for weekdays and weekends.
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
