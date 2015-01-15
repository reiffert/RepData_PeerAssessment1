---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data


```r
data <- read.csv("activity.csv", as.is=TRUE)
cleanData <- data[complete.cases(data),]
```

## What is mean total number of steps taken per day?


```r
data1 <- aggregate(steps ~ date, data=cleanData, sum)

mn <- format(mean(data1$steps),scientific=FALSE, digits = 0 )
mdn <- median(data1$steps)
hist(data1$steps, breaks = 20)
```

![plot of chunk unnamed-chunk-1](figure/unnamed-chunk-1-1.png) 

### The mean total number of steps taken per day is 10766
### The median total number of steps taken per day is 10765


## What is the average daily activity pattern?


```r
# compute mean number of steps taken for each interval across all days
data1 <- aggregate(steps ~ interval, cleanData, mean)
recMax <- data1[data1$steps  == max(data1$steps),]

 plot(steps ~ interval, data1, type="l")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

### 5-minute interval 835 has the maximum number of average number of steps with 206.1698113 average steps

## Imputing missing values



```r
numNAs = nrow(data[is.na(data$steps),])

# set the NA values to the mean steps take for that interval across all days computed in previous section

data1 <- merge(data, aggregate(steps ~ interval, cleanData, mean), by="interval")
data1[,2] <- ifelse(is.na(data1[,2]), data1[,4],data1[,2])

#massage new data set to equal the original data set but with the missing data filled in
data1[,4] = NULL
names(data1)[2] = "steps"

#Calculate the total number of steps taken each day
data1 <- aggregate(steps ~ date, data=data1, sum)

imputed_mean <- format(mean(data1$steps),scientific=FALSE, digits=0)
imputed_median <- format(median(data1$steps),scientific=FALSE,digits=0)

# Create historgram showing the frequency distribution of the total number of steps take per day
hist(data1$steps, breaks=20)
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 
### The number of NAs is 2304
### Mean of the total number of steps taken per day with imputed data is 10766, with clean data is 10766
### Mean of the total number of steps taken per day with imputed data is 10766, with clean data is 10765
### There is neglible difference between the estimates without missins data and with imputed  data


## Are there differences in activity patterns between weekdays and weekends?


```r
#dataWithoutNA$isWeekDay  <- ifelse(dataWithoutNA$date, true, false)
data1 <- cleanData

data1$period  <- ifelse(weekdays(as.Date(data1$date)) %in% c('Monday','Tuesday','Wednesday','Thursday','Friday'), 'weekday', 'weekend')
data1$period <- as.factor(data1$period)

data1 <- aggregate(data1$steps, list(data1$interval,data1$period), mean, na.action = na.omit)
data1 <- setNames(data1,c("interval","period","steps"))

library(lattice)
xyplot(steps ~ interval | period, data1, layout = c(1,2), type = "l")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 


