---
title: 'Reproducible Research: HW1'
author: "Rocco Lucero"
date: "March 14, 2016"
output: html_document
keep_md: true
---

## Loading and preprocessing the data
Import the biometric data from the web and load into the R environment:

```r
temp = tempfile()
file.url ="https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
timestamp()
download.file(url = file.url,destfile = temp)
data = read.csv(unz(temp,"activity.csv"),sep = ',',header = T,na.strings = "NA")
unlink(temp)
```
## What is mean total number of steps taken per day?:
Histogram of the number of steps taken per day for the 61 days examined:

```r
library(plyr)
steps.by.date = ddply( data, .(date),summarize,steps = sum(steps,na.rm = T) )
hist(steps.by.date$steps,col ="blue",xlim =c(0,25000), main = "Histogram of daily step totals",breaks =10 )
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png)

## What is the average daily activity pattern?:
Display the mean and median values for daily step totals:

```r
mean(steps.by.date$steps,na.rm = T)
```

```
## [1] 9354.23
```

```r
median(steps.by.date$steps,na.rm = T)
```

```
## [1] 10395
```

Display the average activity patterns per day:
The Interval 835-840 AM contains the peak of activity
  during the observation period


```r
av.steps.by.interval = ddply(data, .(interval),summarize,steps = mean(steps,na.rm = T))
with(av.steps.by.interval,plot(interval,steps,type ='l', main = 'Average daily activity per 5-min interval'))
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png)

```r
max(av.steps.by.interval$steps,na.rm =T)
```

```
## [1] 206.1698
```

```r
av.steps.by.interval[which(av.steps.by.interval$steps == max(av.steps.by.interval$steps,na.rm =T)),]
```

```
##     interval    steps
## 104      835 206.1698
```

## Imputing missing values:

```r
my.nas = data[which(complete.cases(data) == F),]
nrow(my.nas)
```

```
## [1] 2304
```

```r
impute.mean.steps = function(dat.df,imp.df,imp.var = "steps",fac.var = "interval"){
                        for (x in 1:nrow(dat.df)){
                            intv = (dat.df[x,fac.var])
                            intv.mean = imp.df[imp.df[,fac.var] == intv,imp.var]
                            
                            if(is.na(dat.df[x,imp.var])){
                                dat.df[x,imp.var] = intv.mean            
                            }
                        }
                        dat.df
}

data.imputed.steps = impute.mean.steps(data,av.steps.by.interval)
```

## Are there differences in activity patterns between weekdays and weekends?

```r
steps.by.date.imputed = ddply(data.imputed.steps,.(date),summarize,steps = sum(steps,na.rm = T))
hist(steps.by.date.imputed$steps,col ="lightblue",
    xlim =c(0,25000), xlab = "binned 5-min interval step counts",
    main = "Histogram of daily step totals",breaks =10 )
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6-1.png)

```r
mean = mean(steps.by.date.imputed$steps,na.rm = T)
medi = median(steps.by.date.imputed$steps,na.rm = T)
```
The mean and median values differ after imputation of missing data. For example
after imputation the medain and the mean are identical, and differ from the preimputation
estimators. Both estimates increase and converge after imputation, in this data set.


```r
week.end = weekdays(as.Date(data.imputed.steps$date))
week.end = sapply(week.end,(function(x){if (x %in% c("Saturday","Sunday")){week.end[x] = factor("weekend")}else{ week.end[x] = factor("weekday")}
       })
)

data.imputed.steps = cbind(data.imputed.steps,week.end)
steps.by.interval = ddply(data.imputed.steps, .(interval,week.end),summarize,steps = mean(steps,na.rm = T))
par(mfcol = c(2,1))
with(steps.by.interval,plot(interval[week.end == 'weekday'],steps[week.end == 'weekday'],type ='l', main = 'Weekday average daily activity per 5-min interval',xlab = "5-min interval",ylab = "interval average steps per day"))
with(steps.by.interval,plot(interval[week.end == 'weekend'],steps[week.end == 'weekend'],type ='l', main = 'Weekend average daily activity per 5-min interval',xlab = "5-min interval",ylab = "interval average steps per day"))
```

![plot of chunk unnamed-chunk-7](figure/unnamed-chunk-7-1.png)

On weekends, high activity levels ramp later in the day but persist for more of the day.

