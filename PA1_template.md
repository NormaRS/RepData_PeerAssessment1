---
title: "PA1_template.Rmd"
author: "Norma Ruiz"
date: "16 de julio de 2015"
output: html_document
---

"Reproducible Research: Peer Assessment 1"
====================================================

## Loading and preprocessing the data

Show any code that is needed to  
1. Load the data (i.e. read.csv())  
2. Process/transform the data (if necessary) into a format suitable for your analysis


```r
# set up working directory
setwd("~/Documents/NRS_iMAC/norma_2015/infomedia/data science/5 Reproducible Research/peer assessments/peer assessment 1")
# load data
activity <- read.csv("activity.csv",header=TRUE)
# first rows
head(activity)
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

For this part of the assignment, you can ignore the missing values in the dataset.  
1. Calculate the total number of steps taken per day  
2. If you do not understand the difference between a histogram and a barplot, research the difference between them. Make a histogram of the total number of steps taken each day  
3. Calculate and report the mean and median of the total number of steps taken per day


```r
suppressWarnings(library(sqldf))
# total number of steps taken per day
s <- sqldf("select date,sum(steps) as sumsteps
            from activity
            where steps is not null
            group by date")
# first rows
head(s)
```

```
##         date sumsteps
## 1 2012-10-02      126
## 2 2012-10-03    11352
## 3 2012-10-04    12116
## 4 2012-10-05    13294
## 5 2012-10-06    15420
## 6 2012-10-07    11015
```

```r
# base plotting histogram total number of steps taken per day
hist(s$sumsteps, col="magenta",main="Total number of steps taken per day",
     xlab="Intervals",ylab="Distribution steps")
```

![plot of chunk unnamed-chunk-2](figure/unnamed-chunk-2-1.png) 

```r
# Calculate and report the mean and median of the total number of steps taken per day
summary(s$sumsteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    8841   10760   10770   13290   21190
```

## What is the average daily activity pattern?

1. Make a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all days (y-axis)  
2. Which 5-minute interval, on average across all the days in the dataset, contains the maximum number of steps?  


```r
# calculate average number of steps taken accross all days
int <- sqldf("select interval,avg(steps) as avgsteps
            from activity
            where steps is not null
            group by interval")
# first rows
head(int)
```

```
##   interval  avgsteps
## 1        0 1.7169811
## 2        5 0.3396226
## 3       10 0.1320755
## 4       15 0.1509434
## 5       20 0.0754717
## 6       25 2.0943396
```

```r
# plot 5-minute interval (x-axis) and avegrage number of steps accoss all days (y-axis)
plot (int$interval, int$avgsteps,
      type = "l", lwd=3, col="blue",
      xlab = "5-minute interval [0:2355]", 
      ylab = "Average number steps taken",
      main = "Average daily activity pattern")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3-1.png) 

```r
# 5-minute interval contains the maximum number of steps
maxint <- sqldf("select interval, avgsteps 
                 from int
                 where avgsteps = (select max(avgsteps) from int)")
maxint
```

```
##   interval avgsteps
## 1      835 206.1698
```

## Imputing missing values

Note that there are a number of days/intervals where there are missing values (coded as NA). The presence of missing days may introduce bias into some calculations or summaries of the data.  
1. Calculate and report the total number of missing values in the dataset (i.e. the total number of rows with NAs)  
2. Devise a strategy for filling in all of the missing values in the dataset. The strategy does not need to be sophisticated. For example, you could use the mean/median for that day, or the mean for that 5-minute interval, etc.  
3. Create a new dataset that is equal to the original dataset but with the missing data filled in.  
4. Make a histogram of the total number of steps taken each day and Calculate and report the mean and median total number of steps taken per day. Do these values differ from the estimates from the first part of the assignment? What is the impact of imputing missing data on the estimates of the total daily number of steps?  


```r
# total missing values in the dataset (rows with NAs)
rowsNA <- sqldf("select count(*) 
            from activity
            where steps is NULL ") 
rowsNA
```

```
##   count(*)
## 1     2304
```

```r
# strategy for filling in "steps" missing values in the dataset
# with mean of that 5-min interval
siNA <- sqldf("select a.date, a.interval, b.avgsteps
                from activity a, int b
                where a.interval = b.interval
                and a.steps is NULL")
noNA <- sqldf("select a.date, a.interval, a.steps
                from activity a
                where a.steps is NOT NULL")      
fillactivity <- sqldf("select steps, date, interval from noNA 
                       UNION 
                       select avgsteps, date, interval from siNA
                       order by date, interval")
# first rows
head(fillactivity)
```

```
##   steps       date interval
## 1     1 2012-10-01        0
## 2     0 2012-10-01        5
## 3     0 2012-10-01       10
## 4     0 2012-10-01       15
## 5     0 2012-10-01       20
## 6     2 2012-10-01       25
```

```r
## create a new dataset = original but with missing data filled in
write.table(as.data.frame(fillactivity),"activity_filledin.csv",
            sep=",",
            quote = FALSE, row.names = FALSE, col.names = TRUE)
# make histogram total steps taken each day with no missing data
fills <- sqldf("select date,sum(steps) as sumsteps
            from fillactivity
            group by date")
# first rows
head(fills)
```

```
##         date sumsteps
## 1 2012-10-01    10641
## 2 2012-10-02      126
## 3 2012-10-03    11352
## 4 2012-10-04    12116
## 5 2012-10-05    13294
## 6 2012-10-06    15420
```

```r
# base plotting with no missing data
hist(fills$sumsteps, col="green",main="Total number of steps per day [no NA]",
     xlab="Intervals",ylab="Distribution steps")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4-1.png) 

```r
# Calculate and report the mean and median of the total number of steps taken per day
summary(fills$sumsteps)
```

```
##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
##      41    9819   10640   10750   12810   21190
```


## Are there differences in activity patterns between weekdays and weekends?

For this part the weekdays() function may be of some help here. Use the dataset with the filled-in missing values for this part.  
1. Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day.  
2. Make a panel plot containing a time series plot (i.e. type = "l") of the 5-minute interval (x-axis) and the average number of steps taken, averaged across all weekday days or weekend days (y-axis). See the README file in the GitHub repository to see an example of what this plot should look like using simulated data.  


```r
# create new factor variable "weekday"/"weekend"
fillactivity$dayname <- weekdays(as.Date(fillactivity$date))
fillactivity$daytype[fillactivity$dayname == "Saturday" | 
                     fillactivity$dayname == "Sunday"] <- "weekend" 
fillactivity$daytype[fillactivity$dayname == "Monday" |
                     fillactivity$dayname == "Tuesday" |
                     fillactivity$dayname == "Wednesday" |
                     fillactivity$dayname == "Thursday" |
                     fillactivity$dayname == "Friday"] <- "weekday" 
fillactivity$daytype <- as.factor(fillactivity$daytype)
# grouping weekday/weekend
resact <- sqldf("select daytype, interval, avg(steps) as avgsteps
                 from fillactivity
                 group by daytype, interval ") 
# plot 5-minute interval and average number of steps taken across weekday/weekend
suppressWarnings(library(ggplot2))
ggplot(resact,
       aes(x = interval, y = avgsteps)) +
  geom_line(colour='purple',lwd=1) +
  facet_grid(daytype ~ .) +
  labs(title = "Comparing activity patterns weekdays/weekends",
       x = "Interval",
       y = "Average number of steps")
```

![plot of chunk unnamed-chunk-5](figure/unnamed-chunk-5-1.png) 
