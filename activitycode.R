##PEER ASSIGNMENT 1
## 1. loading data

activity <- read.csv("Reprod Resrch/activity.csv")


### 1. HISTOGRAM

aggdata<-aggregate(steps ~ date,data= activity,sum,na.rm=TRUE) 

png(filename="RRplot1.png",width=480,height=480)

hist(aggdata$steps,main="Histogram",col="yellow",
     xlab="total number of steps per day",ylim=c(0,30)) ## group by date
dev.off()
## mean and median
MeanSteps <- mean(aggdata$steps)
MeanSteps 
MedianSteps <- median(aggdata$steps)
MedianSteps                  
## time series plot

Step_Interval <- aggregate(steps ~ interval, data = activity, mean, na.rm = TRUE)

png(filename="RRplot2.png",width=480,height=480)
plot(steps ~ interval, data = Step_Interval, type = "l",  
     main = "Average  Steps at 5 minute Intervals",
     xlab = "Time Interval(5min)", ylab = "Mean number of steps taken",
     xlim=c(0,2500),col = "blue")
dev.off()
## interval having max numbe of steps
Max_Step_Interval <- Step_Interval[which.max(Step_Interval$steps), ]$interval


## total no of missing values
TOt_NA<- sum(is.na(activity))
TOt_NA


### fill the NA's
interval_steps <- function(interval) {
  Step_Interval[Step_Interval$interval == interval, ]$steps
}


NA_Filled <- activity
count = 0
for (i in 1:nrow(NA_Filled)) {
  if (is.na(NA_Filled[i, ]$steps)) {
    NA_Filled[i, ]$steps <- interval_steps(NA_Filled[i, ]$interval)
    count = count + 1
  }
}

sum(is.na(NA_Filled$steps))

##hist of total number of steps per day

totStepsDays <- aggregate(steps ~ date, data = NA_Filled, sum)


png(filename="RRplot3.png",width=480,height=480)

hist(totStepsDays$steps, col = "yellow", xlab = "Total Number of Steps", 
     ylab = "Frequency", main = "Histogram of Total Number of Steps taken each Day")
dev.off()
### mean and median of new filled data

mean(totStepsDays$steps)

median(totStepsDays$steps)

###NEW FACTOR VARIABLE

NA_Filled$day = ifelse(as.POSIXlt(as.Date(NA_Filled$date))$wday%%6 == 
                              0, "weekend", "weekday")


NA_Filled$day = factor(NA_Filled$day, levels = c("weekday", "weekend"))

### TIME SERIES PLOT

steps_Intervalnew = aggregate(steps ~ interval + day, NA_Filled, mean)
library(lattice)

png(filename="RRplot4.png",width=480,height=480)
xyplot(steps ~ interval | factor(day), data = steps_Intervalnew, aspect = 1/2, 
       type = "l")

dev.off()


