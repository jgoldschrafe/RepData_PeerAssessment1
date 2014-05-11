# Reproducible Research: Peer Assessment 1

```r
calculateIntervalMeans <- function(knownData) {
    intervalMeans <- aggregate(knownData$steps, list(interval = knownData$interval), 
        mean)
    setNames(intervalMeans, c("interval", "steps"))
}

plotIntervalMeans <- function(intervalMeans, main) {
    plot(intervalMeans$interval, intervalMeans$steps, type = "l", main = main, 
        xlab = "Interval ID", ylab = "Steps walked")
}

par(mfrow = c(1, 1))
```



## Loading and preprocessing the data
Load the data using the `read.csv()` function. Since the dates are parsed as
strings, convert them using `as.Date()` and store them back into the data table.


```r
data <- read.csv("activity.csv", stringsAsFactors = FALSE)
data$date <- as.Date(data$date)
```



## What is mean total number of steps taken per day?
 * *Mean total steps:* 10766.19
 * *Median total steps:* 10765

We aggregate the number of steps using the `aggregate()` function to calculate
the mean number of steps for each day in the data set.


```r
knownData <- subset(data, !is.na(steps))
dailyTotals <- aggregate(knownData$steps, list(date = knownData$date), sum)
dailyTotals <- setNames(dailyTotals, c("date", "steps"))
print(paste("Mean:", mean(dailyTotals$steps)))
```

```
## [1] "Mean: 10766.1886792453"
```

```r
print(paste("Median:", median(dailyTotals$steps)))
```

```
## [1] "Median: 10765"
```

```r
hist(dailyTotals$steps, main = "Total steps walked per day", xlab = "Steps walked")
```

![plot of chunk unnamed-chunk-3](figure/unnamed-chunk-3.png) 




## What is the average daily activity pattern?
To arrive at these numbers, we aggregate by the interval identifier associated
with each data point. We then plot it as a time series chart.


```r
intervalMeans <- calculateIntervalMeans(knownData)
plotIntervalMeans(intervalMeans, "Average steps walked per interval")
```

![plot of chunk unnamed-chunk-4](figure/unnamed-chunk-4.png) 




## Imputing missing values
Now, we attempt to impute missing values by replacing them with the average for
all measurements in that interval.


```r
for (i in 1:nrow(data)) {
    if (is.na(data[[i, "steps"]])) {
        data[[i, "steps"]] <- intervalMeans[intervalMeans$interval == data[[i, 
            "interval"]], "steps"]
    }
}
```




## Are there differences in activity patterns between weekdays and weekends?
There is substantial more midday activity in the sample set on weekends as
compared to weekdays.

Differences in weekday/weekend activity patterns can be viewed in the charts
below.


```r
days.weekday <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday")
days.weekend <- c("Saturday", "Sunday")
data$weekday <- weekdays(data$date)
weekdayData <- subset(data, weekday %in% days.weekday)
weekendData <- subset(data, weekday %in% days.weekend)

weekdayMeans <- calculateIntervalMeans(weekdayData)
weekendMeans <- calculateIntervalMeans(weekendData)

par(mfrow = c(2, 1))
plotIntervalMeans(weekdayMeans, "Average steps walked per interval (Weekdays)")
plotIntervalMeans(weekendMeans, "Average steps walked per interval (Weekends)")
```

![plot of chunk unnamed-chunk-6](figure/unnamed-chunk-6.png) 

