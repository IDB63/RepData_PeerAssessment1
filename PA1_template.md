---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

```r
library(ggplot2)
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
```

```
## The following objects are masked from 'package:stats':
## 
##     filter, lag
```

```
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity <- read.csv("activity.csv", sep=",", header = TRUE, na.strings = "NA")
```

## What is mean total number of steps taken per day?

```r
activity1 <- activity %>% filter(activity$steps != "NA") %>% group_by(date) %>% dplyr::summarise(sum_step=sum(steps),mean_step=mean(steps),median_step=median(steps[steps>0]))
ggplot(activity1,aes(x=sum_step))+geom_histogram(bins=20,fill="steelblue",color="white")+labs(title = "Total daily steps")+theme(plot.title=element_text(hjust=0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png)<!-- -->

```r
mean(activity1$mean_step)
```

```
## [1] 37.3826
```

```r
median(activity1$median_step)
```

```
## [1] 56
```

## What is the average daily activity pattern?

```r
activity2 <- activity %>% filter(activity$steps != "NA") %>% group_by(interval) %>% dplyr::summarise(mean_step = mean(steps))
plot(mean_step~interval,data=activity2,type="l")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

```r
activity2[which.max(activity2$mean_step),]
```

```
## # A tibble: 1 × 2
##   interval mean_step
##      <int>     <dbl>
## 1      835      206.
```


## Imputing missing values

```r
activity3 <- activity
colSums(is.na(activity))
```

```
##    steps     date interval 
##     2304        0        0
```

```r
for (i in 1:nrow(activity3)) {
  if (is.na(activity3$steps[i])){
    activity3$steps[i] = activity2$mean_step[which(activity2$interval == activity3$interval[i])]
  } 
}
activity4 <- activity3 %>% group_by(date) %>% dplyr::summarise(sum_step=sum(steps),mean_step=mean(steps),median_step=median(steps))
ggplot(activity4,aes(x=sum_step))+geom_histogram(bins=20,fill="steelblue",color="white")+labs(title = "total daily steps")+theme(plot.title=element_text(hjust=0.5))
```

![](PA1_template_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
## Are there differences in activity patterns between weekdays and weekends?

```r
activity5 <- activity3
activity5$date <- as.Date(activity5$date)
activity5$week <- weekdays(activity5$date)
activity5$weekends_or_not <- ifelse(activity5$week %in% c("Sunday","Saturday"),"weekend","weekday")
activity6 <- activity5 %>% group_by(interval,weekends_or_not) %>% dplyr::summarise(mean_step=mean(steps))
```

```
## `summarise()` has grouped output by 'interval'. You can override using the `.groups` argument.
```

```r
ggplot(activity6,aes(x=interval,y=mean_step))+geom_line()+facet_grid(weekends_or_not~.)
```

![](PA1_template_files/figure-html/unnamed-chunk-5-1.png)<!-- -->
