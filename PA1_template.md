# Reproducible Research: Peer Assessment 1
Alex  
`r Sys.Date()`  


## Loading and preprocessing the data

First of all loading libraries 

```r
library( data.table)
library( dplyr)
```

Loading data and checking structure


```r
dt <- fread( input = "./activity.csv")
```
There is no real need to process/transform the data at this stage. Removing NAs option will be done later on in this assignment.

Also did a visual exploration of data with my favorite spreadsheet soft (LibreOffice Calc). 

## What is mean total number of steps taken per day?

The average total number of steps per day for the period is the sum of all steps par day divided by the number of dates :

```r
sum( dt$steps, na.rm = TRUE) / length( unique( dt$date))
```

```
## [1] 9354.23
```

#### 2 Make a histogram of the total number of steps taken each day   
The total number of steps per day for every day is :

```r
StepsDay <- aggregate(steps ~ date, data = dt, sum, na.rm = TRUE)
par( mfrow = c( 1, 1))
hist( StepsDay$steps, 
      main = "Histogram of total steps by day", 
      xlab = "Number of steps per day", 
      breaks = 50, 
      xlim = c( 0, 25000), 
      col = "SteelBlue")
abline( v = median( StepsDay$steps, na.rm = TRUE), col = "red", lwd = 3)
mtext( side = 1, col = "red", line = 3, 
       text = paste( "                                ",
                     "                           (median)"))
```

![](/home/alex/Documents/R/DSS/5 - Reproductible Research/Week2/Assignment/PA1_template_files/figure-html/unnamed-chunk-3-1.png)<!-- -->

The mean is :

```r
mean( StepsDay$steps)
```

```
## [1] 10766.19
```
And the median is : 

```r
median( StepsDay$steps)
```

```
## [1] 10765
```
They are almost identical and represented by the red line on the graph.

## What is the average daily activity pattern?



```r
dt2 <- dt %>%
       group_by( interval) %>%
       summarize( avedays = mean( steps, na.rm=TRUE))

plot( dt2$interval, dt2$avedays,
                    type="l",
                    col="SteelBlue",
                    main="Average Daily Activity Pattern",
                    xlab="Interval (time of the day)",
                    ylab="Average steps / day")
```

![](/home/alex/Documents/R/DSS/5 - Reproductible Research/Week2/Assignment/PA1_template_files/figure-html/unnamed-chunk-6-1.png)<!-- -->

The max number of averaged steps per day is for interval : 


```r
as.numeric( as.character( dt2[ which( dt2$avedays == max( dt2$avedays)), "interval"]$interval))
```

```
## [1] 835
```


## Imputing missing values


The percent of NAs in the data set is :

```r
paste0( sprintf( fmt = "%2.f", 
                 sum( is.na( dt$steps)) * 100 / nrow( dt)),
        "%")
```

```
## [1] "13%"
```
I propose to fill in missing value with the average on all the period for each interval because NAs are only in a few days full of NAs


```r
dt3 <- dt
AverageSteps <- sum( dt$steps, na.rm = TRUE) /  length( dt$date)
dt3$steps[ is.na( dt3$steps)] <- AverageSteps
```

The total number of steps per day for every day is :

```r
StepsDay <- aggregate(steps ~ date, data = dt3, sum, na.rm = TRUE)
par( mfrow = c( 1, 1))
hist( StepsDay$steps, 
      main = "Histogram of total steps by day", 
      xlab = "Number of steps per day", 
      breaks = 50, 
      xlim = c( 0, 25000), 
      col = "SteelBlue")
abline( v = median( StepsDay$steps, na.rm = TRUE), col = "red", lwd = 3)
mtext( side = 1, col = "red", line = 3, 
       text = paste( "                                ",
                     "                           (median)"))
```

![](/home/alex/Documents/R/DSS/5 - Reproductible Research/Week2/Assignment/PA1_template_files/figure-html/unnamed-chunk-10-1.png)<!-- -->

The mean is :

```r
mean( StepsDay$steps)
```

```
## [1] 10581.01
```
And the median is : 

```r
median( StepsDay$steps)
```

```
## [1] 10395
```
Data are almost identical as with NAs because we replaced NAs with the mean value. Median and mean are identical and represented by the red line on the graph. 


## Are there differences in activity patterns between weekdays and weekends?


```r
dt3$days <- weekdays( as.Date( dt3$date))
dt3$days <- factor( dt3$days,
                    levels = c( "lundi",
                                "mardi",
                                "mercredi",
                                "jeudi",
                                "vendredi",
                                "samedi",
                                "dimanche"),
                    ordered = TRUE)
weekdays <- c( "lundi", 
               "mardi", 
               "mercredi", 
               "jeudi", 
               "vendredi")
weekends <- c( "samedi", "dimanche")

dt3$wd[ dt3$days %in% weekdays] <- "weekday"
dt3$wd[ dt3$days %in% weekends] <- "weekend"
dt3$wd <- factor( dt3$wd,
                    levels = c( "weekday", "weekend"),
                    ordered = TRUE)
par( mfrow = c( 1,2))
par( oma = c( 0, 1, 2, 0))

tssteps3 <- ts( dt3 %>%
                  filter( wd == "weekday") %>% 
                  group_by( interval) %>% 
                  summarize( mean( steps, na.rm = TRUE)))

plot( tssteps3[ , 1], tssteps3[ , 2] , type = "l", 
      xlab = "Intervals (time of the day)",  
      ylab = "Average steps across weekdays",
      col = "SteelBlue",
      ylim = c( 0, 250)) 


tssteps3 <- ts( dt3 %>%
                  filter( wd == "weekend") %>% 
                  group_by( interval) %>% 
                  summarize( mean( steps, na.rm = TRUE)))

plot( tssteps3[ , 1], tssteps3[ , 2] , type = "l",
      xlab = "Interval (time of the day)", 
      ylab = "Average steps across weekends",
      col = "SteelBlue",
      ylim = c( 0, 250))


mtext( text = paste( "Walking activity differences",
                     "between weekdays and week-ends"),
       side = 3, 
       cex = 1.3, 
       line = 0, 
       outer = TRUE)  
```

![](/home/alex/Documents/R/DSS/5 - Reproductible Research/Week2/Assignment/PA1_template_files/figure-html/unnamed-chunk-13-1.png)<!-- -->

### --------------------------------------------------------------------------------------------------
