#read the data
activity_csv<-read.csv("activity.csv")
#table the data by using package "dplyr"
library(dplyr)
activity<-tbl_df(activity_csv)
#basics of data
head(activity)
summary(activity)
# What is mean total number of steps taken per day?
##ignore the missing values and calculate the total number of steps of each day
steps_perday=summarise(group_by(filter(activity,!is.na(steps)),date),sum(steps))
##rename the colume of steps_perday
colnames(steps_perday)=c("date","steps_perday")
head(steps_perday)
##calculate the mean and median
mean(steps_perday$steps_perday)
median(steps_perday$steps_perday)
## draw the plot
hist_steps_perday<-hist(steps_perday$steps_perday,breaks = 15,col = "grey",main = "Steps Per Day",xlab = "Total Steps")

#What is the average daily activity pattern?
## calculate the average steps of each interval
steps_interval<-summarise(group_by(filter(activity,!is.na(steps)),interval),mean(steps,na.rm = TRUE))
##rename the columes and look at the data
colnames(steps_interval)<-c("interval","mean")
head(steps_interval)
##draw the plot of average steps of each interval
line_step_interval<-with(steps_interval,plot(interval,mean,type="l"),col="blue",main="Average Steps of Each Interval")
title(main = "Average Steps of Each Interval")
## calculate the maximum number of steps
interval_max<-steps_interval[which.max(steps_interval$mean),]
interval_max

#imputing missing values
## calculate the number of NAs
sum(is.na(activity))
## create a strategy to fill in the NAs: using mean of 5 minutes interval to replace all NAs
activity2<-activity
activity2[na,1]<-steps_interval[2,2]
head(activity2)
sum(is.na(activity2))
## calculate the total number of steps per day [new]
steps_perday_new=summarise(group_by(activity2,date),sum(steps))
colnames(steps_perday_new)<-c("date",steps_perday)
steps_perday_new
## draw a new hist of steps per day
hist_steps_perday_new<-hist(steps_perday_new$steps_perday,breaks = 15,col = "grey",main = "Steps Per Day",xlab = "Total Steps")
## calculate the mean and median
mean(steps_perday_new$steps_perday)
median(steps_perday_new$steps_perday)

#Are there differences in activity patterns between weekdays and weekends?
## change the date(factor) to Date
activity_date<-as.Date(activity$date)
## create a weekday variable
activity_week<-weekdays(activity_date)
## add the new variable to the original data
activity3<-mutate(activity2,activity_week)
activity3<-mutate(activity3,weektype= ifelse(activity3$activity_week=="Saturday" |activity3$activity_week=="Sunday", "Weekend", "Weekday"))
head(activity3)
## calculate the steps of each interval of both weekends and weekdays
steps_interval_weektype<-summarise(group_by(activity3,interval,weektype),mean(steps))
steps_interval_weektypes
colnames(steps_interval_weektypes)<-c("interval","weektype","steps")
head(steps_interval_weektypes)
## draw the plot
plot<- ggplot(steps_interval_weektypes, aes(x =interval , y=steps, color=weektype)) +
  geom_line() +
  labs(title = "Steps by Weektype", x = "Interval", y = "No. of Steps") +
  facet_wrap(~weektype, ncol = 1, nrow=2)
print(plot)
