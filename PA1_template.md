# Reproducible Research Assignment1

## Data

Read data from file activity.csv


```r
data <- read.csv("./repdata-data-activity/activity.csv", stringsAsFactors = FALSE)
head(data)
```

```
##   steps       date interval
## 1    NA 2012-10-01        0
## 2    NA 2012-10-01        5
## 3    NA 2012-10-01       10
## 4    NA 2012-10-01       15
## 5    NA 2012-10-01       20
## 6    NA 2012-10-01       25
```

## Calculate total steps per day

Calculate the total steps per day and show the results on a histogram. 


```r
library(dplyr)

totstepsdata <- data %>% 
    na.omit() %>% 
    group_by(date) %>% 
    summarize(totsteps = sum(steps))

hist(totstepsdata$totsteps, main = "Total steps taken per day October/November 2012", 
     ylim = c(0, 20), 
     xlim = c(0, 25000), xlab = "Total steps per day", col = "darkgreen",
     breaks = 11)
```

![plot of chunk total steps](figure/total steps-1.png) 



Total number of steps **mean**: 10766.19

Total number of steps **median**: 10765

# Average daily activity plan

![plot of chunk dailyplan](figure/dailyplan-1.png) 

```r
maxavr <- averagedailydata[averagedailydata$avractivity == max(averagedailydata$avractivity), "interval"]
```

5 minutes interval with **maximum** average steps across all days: 8 hours and 35 minutes

# Missing values


The total number of missing values in the dataset is 2304.

In order to fill in the missing values in the dataset (only applies to the steps variable) we will use
the average number of steps across all days for the same time interval.


```r
missingdata <- data %>%
    merge(averagedailydata) %>%
    mutate(steps = ifelse(is.na(steps), round(avractivity, 0), steps)) %>%
    select(-avractivity) %>%
    arrange(date, interval)
```


```r
totstepsdata <- missingdata %>% 
    na.omit() %>% 
    group_by(date) %>% 
    summarize(totsteps = sum(steps))

hist(totstepsdata$totsteps, 
     main = "Total steps taken per day October/November 2012\n(includes estimatation of missing values)", 
     ylim = c(0, 25), 
     xlim = c(0, 25000), xlab = "Total steps per day", col = "lightgreen",
     breaks = 11)
```

![plot of chunk histogram2](figure/histogram2-1.png) 



Total number of steps **mean** (includes estimation for missing values): 10765.64

Total number of steps **median** (includes estimation for missing values): 10762

The values of the mean and the median don't differ much from the first analysis (ignoring the missing values). That implies that the method to estimate the missing values is good.
