My markdown file for the Reproducible Research peer assignment #1
========================================================

Load and preprocess the data
Load activity data and transform the date variable to date class 

```r
library(stats)
setwd("~/Documents/KMOC/Coursera/5_Reproducible Research/Data")
data = read.csv("./activity.csv", header = TRUE)
#data$date = as.Date(data$date, "%Y-%m-%d")
```
What is mean total number of steps taken per day?
Make a histogram of the total number of steps taken each day

```r
stepsbyday = aggregate(steps ~ date, data, sum)
hist(stepsbyday$steps,main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 

Calculate and report the mean and median total number of steps taken per day

```r
meanSteps = mean(stepsbyday$steps, na.rm = TRUE)
medianSteps = median(stepsbyday$steps, na.rm = TRUE)
```
The mean total number of steps is:

```
## [1] 10766
```
The median total number of steps is:

```
## [1] 10765
```
What is the average daily activity pattern?

Make a time series plot (i.e. type = “l”) of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)

```r
stepsbyinterval <- aggregate(steps ~ interval, data, mean)
plot(stepsbyinterval$interval,stepsbyinterval$steps, type="l", xlab="Interval", ylab="Number of Steps",main="Average Number of Steps per Day by Interval")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

Find interval with most average steps.

```r
max_interval <- stepsbyinterval[which.max(stepsbyinterval$steps),1]
```
The interval with the most average steps is:

```
## [1] 835
```
Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)
Impute strategy was to replace NAs with the interval's average

```r
incomplete <- sum(!complete.cases(data))
imputed_data <- transform(data, steps = ifelse(is.na(data$steps), stepsbyinterval$steps[match(data$interval, stepsbyinterval$interval)], data$steps))
```
The total number of missing values is:

```
## [1] 2304
```

Using imputed data, make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. 

```r
stepsbyday_i <- aggregate(steps ~ date, imputed_data, sum)
hist(stepsbyday_i$steps,main = paste("Total Steps Each Day"), col="blue", xlab="Number of Steps")

#Create Histogram to show difference. 
hist(stepsbyday$steps, main = paste("Total Steps Each Day"), col="red", xlab="Number of Steps", add=T)
legend("topright", c("Imputed", "Non-imputed"), col=c("blue", "red"), lwd=10)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 
Calculate new mean and median for imputed data.

```r
imputed_mean <- mean(stepsbyday_i$steps)
imputed_median <- median(stepsbyday_i$steps)
```
Calculate difference between imputed and non-imputed data.

```r
mean_diff <- imputed_mean - meanSteps
med_diff <- imputed_median - medianSteps
```
Calculate total difference.

```r
total_diff <- sum(stepsbyday_i$steps) - sum(stepsbyday$steps)
```
The imputed mean is:

```
## [1] 10766
```
The imputed median is:

```
## [1] 10766
```

Are there differences in activity patterns between weekdays and weekends?
Create a new factor variable in the dataset with two levels – “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
weekdays <- c("Monday", "Tuesday", "Wednesday", "Thursday", 
              "Friday")
imputed_data$dow = as.factor(ifelse(is.element(weekdays(as.Date(imputed_data$date)),weekdays), "Weekday", "Weekend"))
stepsbyinterval_i <- aggregate(steps ~ interval + dow, imputed_data, mean)
library(lattice)
xyplot(stepsbyinterval_i$steps ~ stepsbyinterval_i$interval|stepsbyinterval_i$dow, main="Average Steps per Day by Interval",xlab="Interval", ylab="Steps",layout=c(1,2), type="l")
```

![plot of chunk unnamed-chunk-16](figure/unnamed-chunk-16.png) 
