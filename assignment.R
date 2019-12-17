library("knitr")
library("dplyr")

##Reading and processing data
doc<- read.csv("activity.csv")


##Histogram for total no. of steps
doc_sum<- group_by(doc,date)%>%summarize(sum= sum(steps,na.rm = TRUE))
doc_sum$sum[which(is.na(doc_sum$sum))] <- 0
hist(doc_sum$sum,
     xlab = "Total no. of steps each day",
     ylab = "Frequency",
     main ="Histogram of total steps travelled each day")

##mean and median for no. of steps
summary(doc_sum$sum)[3:4]


##average no. of steps taken vs time
doc_avg<- doc%>%group_by(interval=factor(doc$interval))%>%summarize(average=mean(steps,na.rm = TRUE))
plot(doc_avg$interval,
     doc_avg$average,
     type="h",
     xlab="Time in the day(in 24-hr format)",
     ylab="average steps taken each day ")
title("Average steps taken for each interval")


##The 5-minute interval that, on average, contains the maximum number of steps
doc_avg[which(doc_avg$average==max(doc_avg$average)),1]

##Code to describe and show a strategy for imputing missing data
##Mean imputation
doc_1<-doc
vec<-  which(is.na(doc_1$steps))
for(i in vec){
  interval<- doc_1$interval[i]
  doc_1$steps[i]<- doc_avg$average[which(doc_avg$interval==interval)]
}


##Histogram for imputed data 
doc_sum1<- group_by(doc_1,date)%>%summarize(sum= sum(steps))
hist(doc_sum1$sum,
     xlab = "Total no. of steps each day(without na)",
     ylab = "Frequency",
     main ="Histogram of total steps travelled each day")


##Panel plot for weekdays and weekends
time<- as.Date(doc_1$date)
days<- weekdays(time)
doc_new<- cbind(doc_1,days)
doc_weekday<- filter(doc_new,days!="Saturday" & days!="Sunday")
doc_weekends<- filter(doc_new,days=="Saturday" | days=="Sunday")

doc_avg_weekday<- doc_weekday%>%group_by(interval=factor(interval))%>%summarize(average=mean(steps))
doc_avg_weekend<- doc_weekends%>%group_by(interval=factor(interval))%>%summarize(average=mean(steps))
par(mfrow=c(1,2))
plot(doc_avg_weekday$interval,
     doc_avg_weekday$average,
     type="h",
     xlab="Time in the day",
     ylab="average steps taken each day ")
title("At weekdays")

plot(doc_avg_weekend$interval,
     doc_avg_weekend$average,
     type="l",
     xlab="Time in the day",
     ylab="average steps taken each day",
     ylim=c(0,200))

title("At weekends")