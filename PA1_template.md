# Reproducible Research: Peer Assessment 1


## 1. Loading and preprocessing the data
## Make sure the file "activity.csv" is under your current working directory
rawData <- read.csv("activity.csv")
orgData<-rawData


## 2. What is mean total number of steps taken per day?


sumStepsPerDay<-data.frame(row.names =NULL,levels(rawData$date),
			   tapply (rawData$steps,rawData$date,sum))

names(sumStepsPerDay)<-c("date","totalSteps")

png("sumSteps_vs_Date.png",height = 480,width = 480)

hist(sumStepsPerDay[,2]/1000,
     breaks=8,xlab = "Total Number of Steps (in thousands)",main = "Total Number of Steps Per Day")

dev.off()

meanStepsPerDay<-mean(sumStepsPerDay$totalSteps,na.rm=TRUE)

medianStepsPerDay<-median(sumStepsPerDay$totalSteps,na.rm=TRUE)

## Answer:The mean total number of steps taken per day is 10767.1887
## The median total number of steps taken per day is 10765
## The histogram of the total number of steps taken each day is the file "sumSteps_vs_Date.png"

## 3. What is the average daily activity pattern?

orgData$interval<-as.factor(orgData$interval)

png("average_daily_activity_pattern.png",height = 480,width = 480)

plot(levels(orgData$interval),tapply(orgData$steps,orgData$interval,mean,na.rm=TRUE),type = "l",xlab = "5-min Interval",ylab = "Average Steps",main="The Average Daily Activity Pattern")

dev.off()


meanStepsMins<-data.frame(levels(orgData$interval),tapply(orgData$steps,orgData$interval,mean,na.rm=TRUE))

names(meanStepsMins)<-c("interval","steps")

maxInterval<-max(meanStepsMins$steps)

maxInterval<-as.numeric(as.character(meanStepsMins[meanStepsMins$steps==maxInterval,1]))

##Answer: The average daily activity pattern is in the file "average_daily_activity_pattern"
## The 835 interval contains the maximum number of steps

## 4. Imputing missing data

naIndex<-which(is.na(rawData$steps))

## There is 2304 missing data

for(i in c(1:2304)){

	orgData[naIndex[i],1]<-	meanStepsMins[meanStepsMins$interval==orgData[naIndex[i],3],2]
}

## Replace each missing data with the mean steps of the same interval
sumOrgStepsPerDay<-data.frame(row.names =NULL,levels(orgData$date),
			   tapply (orgData$steps,orgData$date,sum))

names(sumOrgStepsPerDay)<-c("date","totalSteps")

meanOrgStepsPerDay<-mean(sumOrgStepsPerDay$totalSteps,na.rm=TRUE)

medianOrgStepsPerDay<-median(sumOrgStepsPerDay$totalSteps,na.rm=TRUE)

png("sumSteps_vs_Date(na_get_filled).png",height = 480,width = 480)

hist(sumOrgStepsPerDay[,2]/1000,
     breaks=8,xlab = "Total Number of Steps (in thousands)",main = "Total Number of Steps Per Day")

dev.off()

##Answer: After all the missing data get filled
## The mean total number of steps taken per day is 10767.1887
## The median total number of steps taken per day is 10766.1887
## The mean stays the same in two cases. The median get a little bit higher after all the missing data get replaced
## Imputing missing data does not have a significant impact on the estimates of the total daily number of steps
weekdays<-c('Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday')
orgData$date<-strptime(orgData$date, "%Y-%m-%d")

weekdayIndicator<-factor(weekdays(orgData$date)%in%weekdays,levels = c(TRUE,FALSE),
			 labels=c("weekday","weekend"))
orgData<-data.frame(orgData,weekdayIndicator)

## Create a new factor variable in the dataset with two levels - "weekday" and "weekend" indicating whether a given date is a weekday or weekend day

##5. Are there differences in activity patterns between weekdays and weekends?
weekdayData<-split(orgData,orgData$weekdayIndicator)[[1]]
weekendData<-split(orgData,orgData$weekdayIndicator)[[2]]


meanWeekdaysStepsMins<-data.frame(levels(weekdayData$interval),tapply(weekdayData$steps,weekdayData$interval,mean,na.rm=TRUE),"weekday")

names(meanWeekdaysStepsMins)<-c("interval","steps","weekdayIndicator")				

meanWeekendStepsMins<-data.frame(levels(weekendData$interval),tapply(weekendData$steps,weekendData$interval,mean,na.rm=TRUE),"weekend")
names(meanWeekendStepsMins)<-c("interval","steps","weekdayIndicator")

meanOrgStepsMins<-rbind.data.frame(meanWeekdaysStepsMins,meanWeekendStepsMins)

library(lattice)

png("Activity_patterns_between_weekdays_and_weekends.png",width = 480,height = 480)

xyplot((steps/100)~as.numeric(as.character((interval))) | weekdayIndicator, data = meanOrgStepsMins,
       type = "l",
       xlab = "5-min Interval",
       ylab = "Average Steps",
       layout = c(1,2)
       )

dev.off()

## Answer: The activitty patterns in two case are in the file "Activity_patterns_between_weekdays_and_weekends.png"

## There is not a big different between weekday and weekend.
