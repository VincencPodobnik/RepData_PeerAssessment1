################################################################################
# code.R
#
#                 Data Science Specialization via Coursera
#
#                        Prepoducable research
#                           Course project I
#                    UCI Human activity recognition
#
#                       Author: Vincenc Podobnik
#
################################################################################
rm(list = ls())

library(dplyr)

# Load the data
data <- read.csv(
    unz("activity.zip", "activity.csv"),
    colClasses = c('integer', 'POSIXct', 'integer'),
    na.strings = 'NA'
)

# Just browsing the data
str(data)
head(data, 120)
tail(data)
#


# Transformations
#
# interval appears to be time in 5 minute increments
#
# data <- mutate(data, datetime = as.POSIXct( strptime(paste( date, as.character(sprintf("%04d", interval))), '%Y-%m-%d %H%M'))$time)
# data <- data[, c( 'steps', 'datetime' )]


# How many records expected per day
expectedDailyRecordsCount <- 24 * 60 / 5



# Drop NA values using complete.cases
complete <- data[ complete.cases( data ), ]

dailySteps <- group_by( complete, date ) %>% summarise( steps = sum( steps ) )
mean(dailySteps$steps)

hist( dailySteps$steps )

dailyStepsMean <- mean( dailySteps$steps )
dailyStepsMedian <- median( dailySteps$steps )


groupedByInterval <- group_by(complete, interval) %>% summarise( avgSteps = mean(steps))

plot(
    groupedByInterval,
    type='l'
)


maxSteps <- (groupedByInterval %>% summarise( maxSteps = max(avgSteps)))$maxSteps
maxSteps

maxInterval <- groupedByInterval[groupedByInterval$avgSteps == maxSteps, 'interval']
maxInterval


# Check NAs
nrow( data[ is.na( data$steps ), ] )
dailyNARecordCount <- (group_by(data[ is.na( data$steps ), ], date) %>% summarise( NARecordsCount = n()))$NARecordsCount
sum(dailyNARecordCount)


# Imputing strategy
# plot( data$steps, type = 'l' )

# Judging by the above plot it would be simpletest to fill the missing values
# with mean average steps count of the same interval from other days in the set.
# This would minimally affect the overall results.
#
# A better, but more lengthy approach would be to determine workday/weekend trends
# and fill the missing data appropreately.
#



# create a new dataset
filled <- data

# loop through the data
updated <- 0

for(i in 1 : nrow(filled)){
    if(is.na(filled[i, "steps"])){
        filled[i, "steps" ] <- groupedByInterval[ groupedByInterval$interval == filled[i, 'interval'], 'avgSteps']
        updated <- updated + 1
    }

}
print( paste( "Updated", as.character(updated), "records") )



# recalculate new mean and median
#
dailyStepsFilled <- group_by( filled, date ) %>% summarise( steps = sum( steps ) )
hist( dailyStepsFilled$steps )

dailyStepsMeanFilled <- mean( dailyStepsFilled$steps )
dailyStepsMedianFilled <- median( dailyStepsFilled$steps )

print((dailyStepsMeanFilled - dailyStepsMean) / dailyStepsMean)
print(100 * (dailyStepsMedianFilled - dailyStepsMedian) / dailyStepsMedian )

# Weekday weekend difference
#
filled$day <- as.factor( ifelse(as.POSIXlt(data$date)$wday %in% c( 0, 6 ), 'weekend', 'weekday' ) )
groupedByIntervalAndDay <- group_by( filled, interval, day ) %>% summarise( avgSteps = mean(steps) )


par( mfrow = c( 2, 1 ) )

# Weekday plot
plot(
    groupedByIntervalAndDay[groupedByIntervalAndDay$day == 'weekday', c( 'interval', 'avgSteps') ],
    type = 'l',
    main = 'Weekday daily averaged interval steps',
    ylab = 'Average stepps taken'
)

# Weekend plot
plot(
    groupedByIntervalAndDay[groupedByIntervalAndDay$day == 'weekend', c( 'interval', 'avgSteps') ],
    type = 'l',
    main = 'Weekend daily averaged interval steps',
    ylab = 'Average stepps taken'
)

par( mfrow = c( 1, 1 ) )