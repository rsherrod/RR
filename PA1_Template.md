---
title: 'Reproducible Research: Peer Assessment 1'
author: "Randy Sherrod"
date: "December 7, 2015"
output: html_document
keep_md: true
---
###This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

##Loading and preprocessing the data

```r
setwd("C:\\Training\\RR\\data")
data <- read.csv("activity.csv")
summary(data)
```

```
##      steps                date          interval     
##  Min.   :  0.00   2012-10-01:  288   Min.   :   0.0  
##  1st Qu.:  0.00   2012-10-02:  288   1st Qu.: 588.8  
##  Median :  0.00   2012-10-03:  288   Median :1177.5  
##  Mean   : 37.38   2012-10-04:  288   Mean   :1177.5  
##  3rd Qu.: 12.00   2012-10-05:  288   3rd Qu.:1766.2  
##  Max.   :806.00   2012-10-06:  288   Max.   :2355.0  
##  NA's   :2304     (Other)   :15840
```
##What is mean total number of steps taken per day?

```r
#Calculate the total number of steps taken per day
steps_per_day <- aggregate(steps ~ date, data, sum)
#Make a histogram of the total number of steps taken each day
hist(steps_per_day$steps,breaks=10, main="Steps per Day", xlab="Steps", col="grey")
```

![plot of chunk summary](figure/summary-1.png) 

```r
#calculate Mean/mediam
mean <- mean(steps_per_day$steps, na.rm=TRUE)
median <- median(steps_per_day$steps, na.rm=TRUE)
```
The mean steps per day is 1.0766189 &times; 10<sup>4</sup>.  The median steps per day is 10765.

##What is the average daily activity pattern?

```r
steps_interval <- aggregate(data$steps,by = list(interval = data$interval),
                            data = data, FUN=mean, na.rm=TRUE)
names(steps_interval) <- c("interval","steps")
plot(steps ~ interval, data = steps_interval, type = "l", xlab = "Time Intervals (5-minute)", 
     ylab = "Mean number of steps taken", main = "Average number of steps taken at 5 minute intervals",  col= "black")
```

![plot of chunk pattern](figure/pattern-1.png) 

```r
##Maximum steps by interval
maxSteps <- max(steps_interval$steps)
##Which interval contains the maximum average number of steps
max_interval <- steps_interval[steps_interval$steps==maxSteps,1]
```
835 is the 5-minute interval that contains the maximum number of steps (206.1698113).

##Imputing missing values


```r
##Number of NAs in original data set
missing <- nrow(data[is.na(data$steps),])

#impute missing values using mean of interval
names(steps_interval) <- c("interval","mean_steps")
w_imputed <- merge(data,steps_interval, by="interval", all.x=TRUE)
w_imputed$steps[is.na(w_imputed$steps)]=w_imputed$mean_steps[is.na(w_imputed$steps)]

steps_interval2 <- aggregate(steps ~ interval, data = w_imputed, sum, na.rm = TRUE)
hist(steps_interval2$steps,breaks=10, main="Steps per Day with Imputed Values", xlab="Steps", col="grey")
```

![plot of chunk impute](figure/impute-1.png) 

```r
#mean with NA's imputed
mean <- mean(steps_interval2$steps, na.rm=TRUE)

#median with NAs imputed
median <- median(steps_interval2$steps, na.rm=TRUE)
```
The number of missing values in the dataset is: 2304.

After imputing, the mean steps per day is 2280.3385744.  The median steps per day is 2080.9056604.

##Are there differences in activity patterns between weekdays and weekends?

```r
w_imputed$day <- ifelse(as.POSIXlt(as.Date(w_imputed$date))$wday%%6 == 0, "weekend", "weekday")
w_imputed$day <- factor(w_imputed$day, levels = c("weekday", "weekend"))
steps.interval= aggregate(w_imputed$steps, by=list(w_imputed$interval,w_imputed$day), data = w_imputed, FUN=mean)
names(steps.interval) <- c("interval","day","steps")
library(lattice)
xyplot(steps ~ interval | factor(day), data = steps.interval, aspect = 1/2, type = "l")
```

![plot of chunk days](figure/days-1.png) 

The plot clearly shows differences in activity patterns between weekdays and weekends.
