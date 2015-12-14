# Reproducible Research: Peer Assessment 1
Veeresh Taranalli  
December 20, 2015  

## Loading and preprocessing the data

```r
# Read the given data
activity_df <- read.csv("activity.csv")   

# Use dplyr for processing and computing results.
# Create a tbl_df from the loaded dataframe.
library(dplyr)
```

```
## 
## Attaching package: 'dplyr'
## 
## The following objects are masked from 'package:stats':
## 
##     filter, lag
## 
## The following objects are masked from 'package:base':
## 
##     intersect, setdiff, setequal, union
```

```r
activity_tbl_df <- tbl_df(activity_df)
```


## What is mean total number of steps taken per day?

```r
# Compute the total number of steps day, 
# mean and median of total number of steps per day
activity_tbl_df_group_by_date <- group_by(activity_tbl_df, date)
results_totalsteps <- summarise(activity_tbl_df_group_by_date, 
                                totalsteps=sum(steps))

mean_totalsteps <- mean(results_totalsteps$totalsteps, na.rm = TRUE)
median_totalsteps <- median(results_totalsteps$totalsteps, na.rm = TRUE)

# Plot the histogram of total number of steps per day
library(ggplot2)
g <- ggplot(data=results_totalsteps, aes(results_totalsteps$totalsteps))
g <- g + geom_histogram(breaks=seq(0, 25000, by=500), fill="#00407B")
g <- g + labs(title="Histogram for Total No. of Steps per Day")
g <- g + labs(x="Total No. of Steps per Day", y="Number of Days (count)")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-2-1.png) 

```r
# Report mean and median of total number of steps per day
cat("Mean Total No. of Steps per Day: ", mean_totalsteps, fill = TRUE)
```

```
## Mean Total No. of Steps per Day:  10766.19
```

```r
cat("Median Total No. of Steps per Day: ", median_totalsteps, fill = TRUE)
```

```
## Median Total No. of Steps per Day:  10765
```

## What is the average daily activity pattern?

```r
# Compute the daily activity pattern and plot the resulting time series
activity_tbl_df_group_by_interval <- group_by(activity_tbl_df, interval)
results_pattern <- summarise(activity_tbl_df_group_by_interval, 
                             avgsteps=mean(steps, na.rm = TRUE))

g <- ggplot(data=results_pattern, aes(results_pattern$interval, 
                                      results_pattern$avgsteps))
g <- g + geom_line(col="#00407B")
g <- g + labs(title="Daily Activity Pattern (Avg. no. of steps)")
g <- g + labs(x="5-minute intervals", y="Average no. of steps")
print(g)
```

![](PA1_template_files/figure-html/unnamed-chunk-3-1.png) 

```r
# Identify the 5-minute interval containing max. no. of steps
max_avgsteps_loc <- which.max(results_pattern$avgsteps)
intvl_start <- max_avgsteps_loc*5 - 5
intvl_end <- max_avgsteps_loc*5
cat("5-minute interval containing maximum no. of steps:",
    intvl_start, "-", intvl_end)
```

```
## 5-minute interval containing maximum no. of steps: 515 - 520
```
## Imputing missing values



## Are there differences in activity patterns between weekdays and weekends?
