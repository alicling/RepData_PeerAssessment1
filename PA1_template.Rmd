---
title: "PA1_Template.RMD"
output: html_document
author: Xiang
---
## Index
- Loading and preprocessing the data
- What is mean total number of steps taken per day?
- What is the average daily activity pattern?
- Imputing missing values
- Are there differences in activity patterns between weekdays and weekends?

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

---
## Loading and preprocessing the data
#### Library packages
```{r echo=TRUE}
library(dplyr)
library(ggplot2)
```

#### Read the data
```{r echo=TRUE} 
activity_csv<-read.csv("activity.csv")
activity<-tbl_df(activity_csv)
head(activity)
```

#### Basics of data
```{r}
summary(activity)
```

## What is mean total number of steps taken per day?
#### Ignore the missing values and calculate the total number of steps of each day
```{r}
steps_perday=summarise(group_by(filter(activity,!is.na(steps)),date),sum(steps))
colnames(steps_perday)=c("date","steps_perday")
head(steps_perday)
```

#### Calculate the mean and median
```{r echo=TRUE}
mean(steps_perday$steps_perday)
median(steps_perday$steps_perday)
```

#### Draw the plot
```{r}
hist_steps_perday<-hist(steps_perday$steps_perday,breaks = 15,col = "grey",main = "Steps Per Day",xlab = "Total Steps")
```


## What is the average daily activity pattern?
#### Calculate the average steps of each interval
```{r}
steps_interval<-summarise(group_by(filter(activity,!is.na(steps)),interval),mean(steps,na.rm = TRUE))
colnames(steps_interval)<-c("interval","mean")
head(steps_interval)
```

#### Draw the plot of average steps of each interval
```{r echo=TRUE}
with(steps_interval,plot(interval,mean,type="l"),col="blue",main="Average Steps of Each Interval")
title(main = "Average Steps of Each Interval")
```

#### Calculate the maximum
```{r}
steps_interval[which.max(steps_interval$mean),]
```


## Imputing missing values
#### Calculate the number of NAs
```{r}
sum(is.na(activity))
```

#### Create a strategy to fill in the NAs: using mean of 5 minutes interval to replace all NAs
```{r}
activity2<-activity
na<-is.na(activity$steps)
activity2[na,1]<-steps_interval[2,2]
head(activity2)
```

#### Check if there is no NAs in the data
```{r}
sum(is.na(activity2))
```
#### Calculate the total number of steps per day [new]
```{r}
steps_perday_new=summarise(group_by(activity2,date),sum(steps))
colnames(steps_perday_new)<-c("date","steps_perday")
head(steps_perday_new)
```
#### Draw a new hist of steps per day
```{r echo=TRUE}
hist_steps_perday_new<-hist(steps_perday_new$steps_perday,breaks = 15,col = "grey",main = "Steps Per Day",xlab = "Total Steps")
```
#### Calculate the mean and the median
```{r}
mean(steps_perday_new$steps_perday)
median(steps_perday_new$steps_perday)
```


## Are there differences in activity patterns between weekdays and weekends?
#### Change the date(factor) to Date
```{r}
activity_date<-as.Date(activity$date)
head(activity_date)
```
#### Create a weekday variable
```{r}
activity_week<-weekdays(activity_date)
head(activity_week)
```
#### Add the new variable to the original data
```{r}
activity3<-mutate(activity2,activity_week)
activity3<-mutate(activity3,weektype= ifelse(activity3$activity_week=="Saturday" |activity3$activity_week=="Sunday", "Weekend", "Weekday"))
head(activity3)
```
#### Calculate the steps of each interval of both weekends and weekdays
```{r}
steps_interval_weektypes<-summarise(group_by(activity3,interval,weektype),mean(steps))
colnames(steps_interval_weektypes)<-c("interval","weektype","steps")
head(steps_interval_weektypes)
```
#### Draw the plot
```{r echo=TRUE}
plot<- ggplot(steps_interval_weektypes, aes(x =interval , y=steps, color=weektype)) +
  geom_line() +
  labs(title = "Steps by Weektype", x = "Interval", y = "No. of Steps") +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(plot)
```

