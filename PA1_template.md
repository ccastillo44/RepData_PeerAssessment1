---
title: "Reproducible Research: Peer Assessment 1"
output: 
  html_document:
    keep_md: true
---
install.packages("dplyr")
install.packages("tidyverse")
install.packages("rmarkdown")
library(tidyverse)
library(dplyr)
library(lattice)
library(rmarkdown)

### Loading and Preprocessing the Data ####

activity <- read.csv("activity.csv", stringsAsFactors = FALSE)
activity$date <- as.Date(activity$date)

### What is Mean Total Number of Steps Taken Per Day? ####
steps_per_day <- activity %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps, na.rm = TRUE))
  
{r fig1, echo=FALSE}
hist(steps_per_day$total_steps, main = "Total Steps per Day", xlab = "Steps", col = "blue")
mean_steps <- mean(steps_per_day$total_steps, na.rm = TRUE)
median_steps <- median(steps_per_day$total_steps, na.rm = TRUE)
mean_steps
median_steps


### What is the Average Daily Activity Pattern? ####
avg_pattern <- activity %>%
  group_by(interval) %>%
  summarize(mean_steps = mean(steps, na.rm = TRUE))

{r fig2, echo=FALSE}
plot(avg_pattern$interval, avg_pattern$mean_steps, type = "l", xlab = "5-Minute Interval", ylab = "Average Steps")
max_interval <- avg_pattern[which.max(avg_pattern$mean_steps), ]
max_interval

### Imputing Missing Values ####
missing_values <- sum(is.na(activity$steps))
missing_values

# Impute missing values with the average for that interval
activity_imputed <- activity
activity_imputed$steps[is.na(activity_imputed$steps)] <- avg_pattern$mean_steps[match(activity_imputed$interval[is.na(activity_imputed$steps)], avg_pattern$interval)]

# Histogram after imputation
steps_imputed_per_day <- activity_imputed %>%
  group_by(date) %>%
  summarize(total_steps = sum(steps))

{r fig3, echo=FALSE}
hist(steps_imputed_per_day$total_steps, main = "Total Steps per Day (Imputed)", xlab = "Steps", col = "green")
mean_steps_imputed <- mean(steps_imputed_per_day$total_steps)
median_steps_imputed <- median(steps_imputed_per_day$total_steps)
mean_steps_imputed
median_steps_imputed

### Are There Differences in Activity Patterns Between Weekdays and Weekends? ####
activity_imputed$day_type <- ifelse(weekdays(activity_imputed$date) %in% c("Saturday", "Sunday"), "weekend", "weekday")
activity_imputed$day_type <- as.factor(activity_imputed$day_type)

avg_by_daytype <- activity_imputed %>%
  group_by(interval, day_type) %>%
  summarize(mean_steps = mean(steps))

{r fig4, echo=FALSE}
xyplot(mean_steps ~ interval | day_type, data = avg_by_daytype, type = "l", layout = c(1, 2),
       xlab = "Interval", ylab = "Number of steps", main = "Steps by Day Type")
       
rmarkdown::render("PA1_template.Rmd", output_format = "html_document")
