---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
rawData <- read.csv("activity.csv", as.is=TRUE)
#remove records with missing values 
cleanData <- rawData[complete.cases(rawData),]
```

## What is mean total number of steps taken per day?


```r
#calculate the sum of all steps taken for each day
data1 <- aggregate(steps ~ date, data=cleanData, sum)

#calculate the mean and median total steps taken per day
clean_mn <- format(mean(data1$steps),scientific=FALSE, digits=0)
clean_mdn <- format(median(data1$steps),scientific=FALSE, digits=0)
hist(data1$steps, breaks = 20, main="Freq Distribution of Total Steps Taken Per Day", xlab="Total Steps Taken Per Day")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

### The mean total number of steps taken per day is 10766 steps
### The median total number of steps taken per day is 10765 steps


## What is the average daily activity pattern?


```r
# compute average number of steps taken for each interval across all days
data1 <- aggregate(steps ~ interval, cleanData, mean)
# get the record for the interval with the maximum average steps taken
recMax <- data1[data1$steps  == max(data1$steps),]

plot(steps ~ interval, data1, type="l", main="Average Number of Steps Taken Per Interval Across All Days")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

### 5-minute interval 835 has the maximum average number of steps taken with 206.1698113 steps

## Imputing missing values



```r
# compute total number of missing values
numNAs = nrow(rawData[is.na(rawData$steps),])

# Replace missing step data for an interval with the average steps taken for that interval across all days (as computed in the previous section)

#merge the average steps taken values into the original raw data for easy use
data1 <- merge(rawData, aggregate(steps ~ interval, cleanData, mean), by="interval")
#set missing step values to the mean steps taken for the same interval
data1[,2] <- ifelse(is.na(data1[,2]), data1[,4],data1[,2])

#massage data set to equal the original data set but with the missing data filled in
#remove unneeded mean steps taken column merged in above
data1[,4] = NULL
#set the name of the steps column back to 'steps'
names(data1)[2] = "steps"

#With imputed values for missing step data, calculate the total number of steps taken each day
data1 <- aggregate(steps ~ date, data=data1, sum)

imputed_mean <- format(mean(data1$steps),scientific=FALSE, digits=0)
imputed_median <- format(median(data1$steps),scientific=FALSE,digits=0)

#With imputed values for missing step data, create a historgram showing the frequency distribution of the total number of steps taken per day
hist(data1$steps, breaks=20, main="Freq Distribution of Total Steps Taken Per Day (w/Imputed Values)", xlab="Total Steps Taken Per Day")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 
### The number of missing steps taken values is 2304
### Mean of the total number of steps taken per day with imputed data is 10766 and with clean data is 10766
### Median of the total number of steps taken per day with imputed data is 10766 and with clean data is 10765
### There is neglible impact of imputing missing data on the estimates of the total number of steps


## Are there differences in activity patterns between weekdays and weekends?


```r
data1 <- cleanData

# Create new factor variable 'period' with 2 levels = 'weekday' and 'weekend' indicating whether a given data is a weekday or weekend day
data1$period  <- ifelse(weekdays(as.Date(data1$date)) %in% c('Monday','Tuesday','Wednesday','Thursday','Friday'), 'weekday', 'weekend')
data1$period <- as.factor(data1$period)

# Compute average number of steps taken for each interval across all days
data1 <- aggregate(data1$steps, list(data1$interval,data1$period), mean, na.action = na.omit)
data1 <- setNames(data1,c("interval","period","steps"))

# Make a panel plot containing a time series plot of the sverage number of steps taken per 5 minute interval across all weekend and weekday days
library(lattice)
xyplot(steps ~ interval | period, data1, layout = c(1,2), type = "l")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

### There are noticeable differences in activity patterns between weekdays and weekends
