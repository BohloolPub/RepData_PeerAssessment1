# Reproducible Research: Peer Assessment 1
This assignment makes use of data from a personal activity monitoring device. This device collects data at 5 minute intervals through out the day. The data consists of two months of data from an anonymous individual collected during the months of October and November, 2012 and include the number of steps taken in 5 minute intervals each day.

## Loading and preprocessing the data
Load the data and convert date field to Date objects using as.Date() function

```r
unzip("activity.zip")
activity <- read.csv("activity.csv", stringsAsFactors = F)
activity$date <- as.Date(activity$date)
```




## What is mean total number of steps taken per day?

For this part of the assignment, we ignore the missing values in the dataset. 
We make a histogram of the total number of steps taken each day

```r
library(ggplot2)
activity.steps_per_day <- tapply(activity$steps, activity$date, sum)
activity_per_day <- data.frame(steps = activity.steps_per_day)
ggplot(data = activity_per_day, aes(steps)) + geom_histogram(binwidth = 1500)
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2.png) 


Calculating the mean total number of steps taken per day

```r
mean(activity.steps_per_day, na.rm = T)
```

```
## [1] 10766
```

Calculating the median total number of steps taken per day

```r
median(activity.steps_per_day, na.rm = T)
```

```
## [1] 10765
```


## What is the average daily activity pattern?
We make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days 

```r
average_daily_activity <- tapply(activity$steps, activity$interval, mean, na.rm = T)
daily_activity <- data.frame(interval = as.integer(row.names(average_daily_activity)), 
    steps = average_daily_activity)
ggplot(data = daily_activity, aes(interval, steps)) + geom_line()
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5.png) 

Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?

```r
as.integer(names(which.max(average_daily_activity)))
```

```
## [1] 835
```




## Imputing missing values
There are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.

Calculating the total number of missing values in the dataset (i.e. the total number of rows with NAs)

```r
sum(is.na(activity$steps))
```

```
## [1] 2304
```


We use the mean for that 5-minute interval for filling in all of the missing values in the dataset.

```r
na_indices <- which(is.na(activity$steps))
interval <- as.character(activity[na_indices, 3])
imputed_steps <- average_daily_activity[interval]
names(imputed_steps) <- na_indices
```


Create a new dataset that is equal to the original dataset but with the missing data filled in.

```r
for (i in na_indices) {
    activity$steps[i] = imputed_steps[as.character(i)]
}
```



Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?


```r
activity.steps_per_day <- tapply(activity$steps, activity$date, sum)
activity_per_day <- data.frame(steps = activity.steps_per_day)
ggplot(data = activity_per_day, aes(steps)) + geom_histogram(binwidth = 1500)
```

![plot of chunk unnamed-chunk-10](figure/unnamed-chunk-10.png) 



Calculating the mean total number of steps taken per day

```r
mean(activity.steps_per_day, na.rm = T)
```

```
## [1] 10766
```

Calculating the median total number of steps taken per day

```r
median(activity.steps_per_day, na.rm = T)
```

```
## [1] 10766
```




## Are there differences in activity patterns between weekdays and weekends?
We use the dataset with the filled-in missing values for this part.

Creating a new factor variable in the dataset with two levels  weekday and weekend indicating whether a given date is a weekday or weekend day.

```r
activity$day_type <- ifelse(weekdays(activity$date, abbreviate = T) %in% c("Sat", 
    "Sun"), "weekend", "weekday")
```


Creating a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). The plot should look something like the following, which was creating using simulated data:

```r
average_daily_activity <- tapply(activity$steps, list(activity$interval, activity$day_type), 
    mean, na.rm = T)
daily_activity <- data.frame(interval = as.integer(row.names(average_daily_activity)), 
    average_daily_activity)

library(reshape2)
```

```
## Warning: package 'reshape2' was built under R version 3.0.3
```

```r
daily_activity.long = melt(daily_activity, id.vars = "interval", variable.name = "day_type", 
    value.name = "steps")

ggplot(data = daily_activity.long, aes(interval, steps)) + facet_wrap(~day_type, 
    ncol = 1) + geom_line()
```

![plot of chunk unnamed-chunk-14](figure/unnamed-chunk-14.png) 


