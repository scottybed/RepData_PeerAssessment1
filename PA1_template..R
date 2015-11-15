library(dplyr)
library(ggplot2)
library(knitr)
setwd('D:\\Data\\Repositories\\Coursera\\PeerAssessment1')
data <- read.csv("activity.csv")
stepsPerDay <- summarise(group_by(data, date), steps=sum(steps))
q <- qplot(stepsPerDay$steps, geom="histogram",binwidth = 250,  
           main = "Histogram for Steps", 
           xlab = "Steps per Day",
           ylab = "Frequency (bin size = 250)",
           fill=I("blue"), 
           col=I("red"), 
           alpha=I(.2))
print(q)
meanSPD <- round(mean(stepsPerDay$steps, na.rm=TRUE), 3)
medianSPD <- median(stepsPerDay$steps, na.rm=TRUE)
stepsPerTimeInterval <- data[c("steps","interval")] %>% group_by(interval) %>% summarise_each(funs(mean(., na.rm = TRUE)))
g <- ggplot(stepsPerTimeInterval, aes(interval, steps)) + 
    geom_line() +
    xlab("5-minute interval") +
    ylab("average number of steps taken") 
print(g)
maxIntervalSteps <- max(stepsPerTimeInterval$steps)
maxInterval <- stepsPerTimeInterval[stepsPerTimeInterval$steps==maxIntervalSteps,]$interval
numNA <- sum(is.na(data$steps))

dataMerge <- merge(data, stepsPerTimeInterval, by="interval", all.x=TRUE)
dataMerge$steps.x[is.na(dataMerge$steps.x)] = as.integer(dataMerge$steps.y[is.na(dataMerge$steps.x)])
dataImputed <- dataMerge[order(dataMerge$date,dataMerge$interval),c("steps.x", "interval", "date")]
stepsPerDayImputed <- summarise(group_by(dataImputed, date), steps=sum(steps.x))
qImputed <- qplot(stepsPerDayImputed$steps, geom="histogram",binwidth = 100,  
           main = "Histogram for Imputed Steps", 
           xlab = "Steps per Day",
           ylab = "Frequency (bin size = 250)",
           fill=I("blue"), 
           col=I("red"), 
           alpha=I(.2))
print(qImputed)

meanSPDImputed <- round(mean(stepsPerDayImputed$steps), 3)
medianSPDImputed <- median(stepsPerDayImputed$steps)

dataImputed$dateType <-  ifelse(as.POSIXlt(dataImputed$date)$wday %in% c(0,6), 'weekend', 'weekday')

stepsIntervaleDateType <- dataImputed[c("steps.x","interval", "dateType")] %>% group_by(interval, dateType) %>% summarise_each(funs(mean(., na.rm = TRUE)))

gDateType = ggplot(data=stepsIntervaleDateType, aes(x=interval, y=steps.x)) +
    geom_line() +
    facet_grid(dateType ~ ., scales="free") +
    ylab("Steps") + xlab("Interval")

print(gDateType)

