---
title: 'Reproducible Research: Peer Assessment 1'
output:
  html_document:
    keep_md: yes
  pdf_document: default
---


## Loading and preprocessing the data
I begin by loading the necessary packages and importing the data.


```r
library(dplyr)
library(ggplot2)
data = read.csv(unz("activity.zip", "activity.csv"), colClasses = c("integer", "Date", "integer"))
```

## What is mean total number of steps taken per day?
Here is the distribution of the total number of steps taken in a day.

```r
dailysteps = summarize(group_by(data,date), numsteps = sum(steps,na.rm = TRUE))
qplot(dailysteps$numsteps)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

The mean number of steps taken in a day is:

```r
mean(dailysteps$numsteps)
```

```
## [1] 9354.23
```
and the median number of steps is:

```r
median(dailysteps$numsteps)
```

```
## [1] 10395
```

## What is the average daily activity pattern?
Below is a plot of the average steps taken in each five minute interval across all days. 

```r
intervalsteps = summarize(group_by(data,interval), avesteps = mean(steps,na.rm = TRUE))
qplot(interval,avesteps, data = intervalsteps, geom = "path")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 

The maximum average number of steps occurs during this five minute interval:

```r
intervalsteps$interval[max(intervalsteps$avesteps)==intervalsteps$avesteps]
```

```
## [1] 835
```

## Imputing missing values

There are:

```r
sum(is.na(data$steps))
```

```
## [1] 2304
```
intervals with missing values.

To impute these missing values I use the average number of steps taken in the five minute interval across all days when that interval is not missing.

```r
data_imp = data %>% 
    group_by(interval) %>% 
    mutate(imputesteps = mean(steps ,na.rm = TRUE))

data_imp$steps[is.na(data_imp$steps)] = data_imp$imputesteps[is.na(data_imp$steps)]
data_imp = select(data_imp,-imputesteps)
```

Now that the data is imputed we can view the histogram for the total steps taken in a day.

```r
dailysteps_imp = summarize(group_by(data_imp,date), numsteps = sum(steps,na.rm = TRUE))
qplot(dailysteps_imp$numsteps)
```

```
## stat_bin: binwidth defaulted to range/30. Use 'binwidth = x' to adjust this.
```

![plot of chunk unnamed-chunk-9](figure/unnamed-chunk-9-1.png) 

Note that the imputation has increased the estimates of the average and median number of steps taken on each day:

```r
mean(dailysteps_imp$numsteps)
```

```
## [1] 10766.19
```

```r
median(dailysteps_imp$numsteps)
```

```
## [1] 10766.19
```

## Are there differences in activity patterns between weekdays and weekends?
Unsurprisingly, there are differences between weekday and weekends in terms of steps taken in any five minute interval.

```r
data_imp$weekday = weekdays(data_imp$date)
data_imp$weekend = "Weekday"
data_imp$weekend[data_imp$weekday == "Saturday" | data_imp$weekday == "Sunday"] = "Weekend"
data_imp$weekend = as.factor(data_imp$weekend)

intervalsteps = summarize(group_by(data_imp,interval,weekend), avesteps = mean(steps,na.rm = TRUE))
qplot(interval,avesteps, data = intervalsteps, facets = weekend ~ ., geom = "path")
```

![plot of chunk unnamed-chunk-11](figure/unnamed-chunk-11-1.png) 
