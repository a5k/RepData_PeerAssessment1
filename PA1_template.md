# Reproducible Research: Peer Assessment 1


## Loading and preprocessing the data
For loading the data we will assume that the dataset
has already been unzipped in the target directory. 
After loading the dataset, we find that the data consists of 
17568 observations of 3 variables. 
We convert the date column to date values and then create a
data table for next steps

```r
## read the csv file and show the structure of the dataframe loaded
data <- read.csv("./activity/activity.csv")
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Factor w/ 61 levels "2012-10-01","2012-10-02",..: 1 1 1 1 1 1 1 1 1 1 ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
## convert the dates to date types in R
data$date <- as.Date(data$date, "%Y-%m-%d")
##now show the structure again 
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
##read into a data table to sum by dates
library(data.table)
data1<-data.table(data)
```


## What is mean total number of steps taken per day?
To calculate the total steps taken by day we create a new frame called
dailysteps. 

```r
## find total steps taken by day
dailysteps <-data1[,as.numeric(sum(steps, na.rm = T)), by = "date"]
str(dailysteps)
```

```
## Classes 'data.table' and 'data.frame':	61 obs. of  2 variables:
##  $ date: Date, format: "2012-10-01" "2012-10-02" ...
##  $ V1  : num  0 126 11352 12116 13294 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
# we then plot the histogram of the calculated sum
hist(dailysteps$V1, xlab = "Daily Steps",
      main = "Histogram of Daily steps taken",
      col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
## we also calculate the mean and median of daily steps taken
dailysteps_mean <- format(mean(dailysteps$V1), digits = 6)
dailysteps_median <-format(median(dailysteps$V1), digits = 8)

##post the figure in the figure folder
dev.copy(png, './instructions_fig/steps_hist.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```
The mean total number of steps taken per day is 
**9354.23**

The median total number of steps taken per day is 
**10395**

## What is the average daily activity pattern?
To find the average daily pattern we create a new frame called
avgsteps. We aren't converting intervals to timestamps yet


```r
avgsteps <- data1[,as.numeric(mean(steps, na.rm = T)), by = "interval"]
str(avgsteps)
```

```
## Classes 'data.table' and 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ V1      : num  1.717 0.3396 0.1321 0.1509 0.0755 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
##now show the plot
plot(avgsteps$V1~avgsteps$interval, type = 'l', xlab = "Time Interval",
      ylab = "Average step count",
     col = "blue")
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
## find the interval with the max average step count
max_interval <- format(avgsteps$interval[which.max(avgsteps$V1)], 
                       digits = 3)

##copy figure to figure folder
dev.copy(png, './instructions_fig/interval.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```


The interval with the max average step count is 
**835**

## Imputing missing values
We find the number of rows in the dataset that have missing values
using the following code

```r
NA_rows = nrow(data[which(is.na(data$steps)),])
```
There are **2304** that have NA values in the dataset

We will impute missing values with **the average steps for that interval** and replace NA values with these averages. The following code shows the method

```r
## we've calculated the avgsteps data frame above
## merge that with the NA data
NAdata <- data[which(is.na(data$steps)),]
newNA <- merge(NAdata, avgsteps, by.x = "interval", by.y = "interval")

## now replace the steps NA data with these new values
newdata <- data
newdata$steps <- replace(newdata$steps, which(is.na(newdata$steps)), newNA$V1)
str(data)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : int  NA NA NA NA NA NA NA NA NA NA ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
str(newdata)
```

```
## 'data.frame':	17568 obs. of  3 variables:
##  $ steps   : num  1.72 1.72 1.72 1.72 1.72 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
```

```r
data2 <- data.table(newdata)
```

Now we repeat the histogram work

```r
## find total steps taken by day
dailysteps <-data2[,as.numeric(sum(steps, na.rm = T)), by = "date"]
str(dailysteps)
```

```
## Classes 'data.table' and 'data.frame':	61 obs. of  2 variables:
##  $ date: Date, format: "2012-10-01" "2012-10-02" ...
##  $ V1  : num  138 126 11352 12116 13294 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
# we then plot the histogram of the calculated sum
hist(dailysteps$V1, xlab = "Daily Steps",
      main = "Histogram of Daily steps taken",
      col = "red")
```

![](PA1_template_files/figure-html/unnamed-chunk-6-1.png) 

```r
## we also calculate the mean and median of daily steps taken
dailysteps_mean_new <- format(mean(dailysteps$V1), digits = 6)
dailysteps_median_new <-format(median(dailysteps$V1), digits = 8)

##post the figure in the figure folder
dev.copy(png, './instructions_fig/steps_hist_newdata.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

The mean total number of steps taken per day is 
**10766.2**

The median total number of steps taken per day is 
**11015**

As we can see, the mean and median total steps taken are different for this new dataset (with NA values imputed). 

Old mean value was 9354.23. 
new mean value is 10766.2

Old median value was 10395. 
new median value was 11015

Both new values are higher than the old values


## Are there differences in activity patterns between weekdays and weekends?

We first create the weekday/weekend column as follows. then plot them in a single panel. The layout won't match the sample plots exactly due to time constraints on submission. 


```r
## add a new column to the newdata frame
newdata[, "day"] <- weekdays(newdata$date)
##now replace Monday-Friday with "weekday"
newdata$day <- replace(newdata$day, which(newdata$day != c("Sunday", "Saturday")), "Weekday")
## now replace Saturday and Sunday with "weekend"
newdata$day <- replace(newdata$day, which(newdata$day == c("Sunday", "Saturday")), "Weekend")

#now we form two frames for weekdays and weekends
newdata_wkday <- subset(newdata, day == "Weekday")
newdata_wkend <- subset(newdata, day == "Weekend")

## create data tables to calculate average steps per interval for both sets
newdata_wkday_t1 <- data.table(newdata_wkday)
newdata_wkend_t1 <- data.table(newdata_wkend)

str(newdata_wkday_t1)
```

```
## Classes 'data.table' and 'data.frame':	15264 obs. of  4 variables:
##  $ steps   : num  1.72 1.72 1.72 1.72 1.72 ...
##  $ date    : Date, format: "2012-10-01" "2012-10-01" ...
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ day     : chr  "Weekday" "Weekday" "Weekday" "Weekday" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
str(newdata_wkend_t1)
```

```
## Classes 'data.table' and 'data.frame':	2304 obs. of  4 variables:
##  $ steps   : num  0 0 0 0 0 0 0 0 0 0 ...
##  $ date    : Date, format: "2012-10-06" "2012-10-06" ...
##  $ interval: int  5 15 25 35 45 55 105 115 125 135 ...
##  $ day     : chr  "Weekend" "Weekend" "Weekend" "Weekend" ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
## now find mean steps per interval for both

dailysteps_wkday <-newdata_wkday_t1[,as.numeric(mean(steps, na.rm = T)), by = "interval"]

dailysteps_wkend <- newdata_wkend_t1[,as.numeric(mean(steps, na.rm = T)), by = "interval"]

str(dailysteps_wkday)
```

```
## Classes 'data.table' and 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  0 5 10 15 20 25 30 35 40 45 ...
##  $ V1      : num  5.18 5.95 3.6 5.76 3.54 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
str(dailysteps_wkend)
```

```
## Classes 'data.table' and 'data.frame':	288 obs. of  2 variables:
##  $ interval: int  5 15 25 35 45 55 105 115 125 135 ...
##  $ V1      : num  3.75 3.75 3.75 3.75 5.26 ...
##  - attr(*, ".internal.selfref")=<externalptr>
```

```r
## now plot them in a panel
par(mfrow = c(2,1))
par(mar = c(3,3,1,1))

plot(dailysteps_wkend$V1~dailysteps_wkend$interval, 
     type = 'l', xlab = "",
      ylab = "",
     col = "blue",
     main = "Weekend")

plot(dailysteps_wkday$V1~dailysteps_wkday$interval, 
     type = 'l', xlab = "Time Interval",
      ylab = "",
     col = "blue",
     main = "Weekday")
```

![](PA1_template_files/figure-html/unnamed-chunk-7-1.png) 

```r
# post this panel in the figures folders
##post the figure in the figure folder
dev.copy(png, './instructions_fig/AvgSteps_Panel.png')
```

```
## png 
##   3
```

```r
dev.off()
```

```
## pdf 
##   2
```

There are differences in the activity patterns between weekdays and weekends, particularly as the day progresses (intervals > 1000) suggesting increased activity in the afternoons and evenings on weekends compared to weekends. 
