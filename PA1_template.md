---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---


## Loading and preprocessing the data

data <- read.csv("activity.csv")

data$date <- as.Date(data$date, format="%Y-%m-%d")

head(data)

## What is mean total number of steps taken per day?

steps_per_day <- data %>% group_by(date) %>% summarise(total_steps = sum(steps, na.rm = TRUE))


head(steps_per_day)


hist(steps_per_day$total_steps, main="Total Steps per Day", xlab="Steps", ylab="Frequency", col="blue")


mean_steps <- mean(steps_per_day$total_steps, na.rm = TRUE)
median_steps <- median(steps_per_day$total_steps, na.rm = TRUE)


mean_steps
median_steps




## What is the average daily activity pattern?

average_steps_interval <- data %>% group_by(interval) %>% summarise(mean_steps = mean(steps, na.rm = TRUE))


plot(average_steps_interval$interval, average_steps_interval$mean_steps, type="l", xlab="5-minute Interval", ylab="Average Number of Steps", main="Average Daily Activity Pattern")


max_interval <- average_steps_interval[which.max(average_steps_interval$mean_steps),]


max_interval


## Imputing missing values

total_na <- sum(is.na(data$steps))

total_na

data_filled <- data

data_filled$steps <- ifelse(is.na(data_filled$steps), 
                            ave(data_filled$steps, data_filled$interval, FUN = function(x) mean(x, na.rm = TRUE)), 
                            data_filled$steps)
steps_per_day_filled <- data_filled %>% group_by(date) %>% summarise(total_steps = sum(steps))


hist(steps_per_day_filled$total_steps, main="Total Steps per Day (Filled Data)", xlab="Steps", ylab="Frequency", col="green")


mean_steps_filled <- mean(steps_per_day_filled$total_steps)
median_steps_filled <- median(steps_per_day_filled$total_steps)

mean_steps_filled
median_steps_filled



## Are there differences in activity patterns between weekdays and weekends?

data_filled$day_type <- ifelse(weekdays(data_filled$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
data_filled$day_type <- as.factor(data_filled$day_type)


average_steps_day_type <- data_filled %>% group_by(interval, day_type) %>% summarise(mean_steps = mean(steps))


ggplot(average_steps_day_type, aes(x = interval, y = mean_steps, color = day_type)) +
  geom_line() +
  facet_wrap(~ day_type, ncol = 1) +
  labs(title = "Average Daily Activity Pattern: Weekdays vs Weekends", x = "5-minute Interval", y = "Average Number of Steps")

