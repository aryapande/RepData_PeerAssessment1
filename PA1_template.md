---
title: "Reproducible Research: Peer Assessment 1 - Aryaman Pande"
output: 
  html_document:
    keep_md: true
---




## Loading and preprocessing the data

```r
setwd("C:/Users/arya/Desktop/coursera/R_pro/Rstudio/reproducible/week2")  
data<-read.csv("activity.csv")  
library(ggplot2)  
summary(data)  
```

```
##      steps            date              interval     
##  Min.   :  0.00   Length:17568       Min.   :   0.0  
##  1st Qu.:  0.00   Class :character   1st Qu.: 588.8  
##  Median :  0.00   Mode  :character   Median :1177.5  
##  Mean   : 37.38                      Mean   :1177.5  
##  3rd Qu.: 12.00                      3rd Qu.:1766.2  
##  Max.   :806.00                      Max.   :2355.0  
##  NA's   :2304
```

```r
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```


## What is mean total number of steps taken per day?

```r
stepsPerDay<- aggregate(data$steps, list(data$date), FUN=sum)
hist(stepsPerDay$x,breaks=8,xlab = "Number of Steps per Day",col = "azure",xlim = c(0,25000),main="Histogram of Total Steps per Day")  
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

#### Mean total number of steps

```r
m <- mean(stepsPerDay$x, na.rm=TRUE)
m
```

```
## [1] 10766.19
```

#### Median total number of steps

```r
median(stepsPerDay$x, na.rm=TRUE)  
```

```
## [1] 10765
```

## What is the average daily activity pattern?

```r
stepsPerTime <- aggregate(steps~interval,data=data,FUN=mean,na.action=na.omit)  
h <- ggplot(stepsPerTime, aes(interval, steps))
h+geom_line(col="blue")+ggtitle("Average steps per time interval")+xlab("Time")+ylab("Steps")
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->

#### 5-minute interval (on average across all the days) with the max steps

```r
stepsPerTime[which.max(stepsPerTime$steps), ]$interval  
```

```
## [1] 835
```

## Imputing missing values
#### Total number of missing values

```r
na = is.na(data$steps)
sum(na)  
```

```
## [1] 2304
```

#### Devise a strategy for filling in all of the missing values in the dataset. 

```r
data$steps[is.na(data$steps)]<-round(m/10000)
```
#### New dataset that is equal to the original dataset but with the missing data filled in 

```r
dataFull <- data.frame(steps=data$steps, interval=data$interval, date=data$date)
head(dataFull)
```

```
##   steps interval       date
## 1     1        0 2012-10-01
## 2     1        5 2012-10-01
## 3     1       10 2012-10-01
## 4     1       15 2012-10-01
## 5     1       20 2012-10-01
## 6     1       25 2012-10-01
```

#### Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?

```r
stepsPerDayFull <- aggregate(dataFull$steps, list(dataFull$date), FUN=sum)
head(stepsPerDayFull)
```

```
##      Group.1     x
## 1 2012-10-01   288
## 2 2012-10-02   126
## 3 2012-10-03 11352
## 4 2012-10-04 12116
## 5 2012-10-05 13294
## 6 2012-10-06 15420
```

```r
# draw the histogram
hist(stepsPerDayFull$x,breaks=8,xlab = "Number of Steps per Day",col = "azure",main="New Histogram of Total Steps per Day")  
```

![](PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

```r
# The values do differ from first part since we have filled in the man/10000 amt. of steps in each interval.  
# This has led to an increase in total number of steps per day
```


## Are there differences in activity patterns between weekdays and weekends?

#### Create a new factor variable in the dataset with two levels - “weekday” and “weekend” indicating whether a given date is a weekday or weekend day.

```r
dataFull$RealDate <- as.Date(dataFull$date, format = "%Y-%m-%d")

dataFull$weekday <- weekdays(dataFull$RealDate)
# create a new variable indicating weekday or weekend
dataFull$DayType <- ifelse(dataFull$weekday=='Saturday' | dataFull$weekday=='Sunday', 'weekend','weekday')
# see first 10 values
head(dataFull, n=10)
```

```
##    steps interval       date   RealDate weekday DayType
## 1      1        0 2012-10-01 2012-10-01  Monday weekday
## 2      1        5 2012-10-01 2012-10-01  Monday weekday
## 3      1       10 2012-10-01 2012-10-01  Monday weekday
## 4      1       15 2012-10-01 2012-10-01  Monday weekday
## 5      1       20 2012-10-01 2012-10-01  Monday weekday
## 6      1       25 2012-10-01 2012-10-01  Monday weekday
## 7      1       30 2012-10-01 2012-10-01  Monday weekday
## 8      1       35 2012-10-01 2012-10-01  Monday weekday
## 9      1       40 2012-10-01 2012-10-01  Monday weekday
## 10     1       45 2012-10-01 2012-10-01  Monday weekday
```
#### Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis).

```r
a <- aggregate(steps~interval+DayType,data=dataFull,FUN=mean,na.action=na.omit)
a$time <- stepsPerTime$interval/100
j <- ggplot(a, aes(time, steps))
j+geom_line(col="blue")+ggtitle("Average steps per time interval: weekdays vs. weekends")+xlab("Time")+ylab("Steps")+theme(plot.title = element_text(face="bold", size=12))+facet_grid(DayType ~ .)
```

![](PA1_template_files/figure-html/unnamed-chunk-12-1.png)<!-- -->
