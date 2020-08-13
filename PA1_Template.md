---
title: "Reproductible research project 1"
author: "Christophe Imbert de la Platière"
date: "8/13/2020"
output:
  html_document: default
  word_document: default
  pdf_document: default
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

## R Markdown

This is an R Markdown document. Markdown is a simple formatting syntax for authoring HTML, PDF, and MS Word documents. For more details on using R Markdown see <http://rmarkdown.rstudio.com>.

When you click the **Knit** button a document will be generated that includes both content as well as the output of any embedded R code chunks within the document. You can embed an R code chunk like this:

# 1. Code for reading in the dataset and/or processing the data
```{r loaddata}
download.file("https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip", destfile = "activity.zip", mode="wb")
# unzip and read dataset
unzip("activity.zip")
stepdata <- read.csv("activity.csv", header = TRUE)
head(stepdata)
```

## load magrittr and dplyr packages
```{r}
library(magrittr)
library(dplyr)

## calculate number of steps by date --> stepsbydate variable

stepsbydate <- stepdata %>% select(date, steps) %>% group_by(date) %>% summarize(tsteps= sum(steps)) %>%na.omit()

# 2. display histogram of the total number of steps each day 

hist(stepsbydate$tsteps, xlab = "Total daily Steps",main="Histogram of Total Steps by day", breaks = 20)
```

```{r}
# 3a. mean value of steps by date 
mean(stepsbydate$tsteps)
```

```{r}
# 3b. median value of steps by date
median(stepsbydate$tsteps)

```
```{r}
## install package ggplot2
library(ggplot2)

## calculate average number of steps taken 
stepsbyinterval <- stepdata%>% select(interval, steps) %>% na.omit() %>% group_by(interval) %>% summarize(tsteps= mean(steps)) 


# 4. time series plot display 

ggplot(stepsbyinterval, aes(x=interval, y=tsteps))+ geom_line()
```

```{r}
# 5. The 5-minute interval that, on average, contains the maximum number of steps
stepsbyinterval[which(stepsbyinterval$tsteps== max(stepsbyinterval$tsteps)),]


## imputing missing values NAs
missingVals <- sum(is.na(data))

## display missing values
missingVals
```

```{r}

# 6. Code to describe and show a strategy for imputing missing data
library(magrittr)
library(dplyr)

replacewithmean <- function(x) replace(x, is.na(x), mean(x, na.rm = TRUE))
meandata <- stepdata%>% group_by(interval) %>% mutate(steps= replacewithmean(steps))
head(meandata)



FullSummedDataByDay <- aggregate(meandata$steps, by=list(meandata$date), sum)

names(FullSummedDataByDay)[1] ="date"
names(FullSummedDataByDay)[2] ="totalsteps"
head(FullSummedDataByDay,15)



## Summary of new data : mean & median
summary(FullSummedDataByDay)
```

```{r}
# 7. Histogram of the total number of steps taken each day after missing values are imputed

hist(FullSummedDataByDay$totalsteps, xlab = "Steps", ylab = "Frequency", main = "Total Daily Steps", breaks = 20)
```



## 8. compare mean and median of these new data vs initial dataset 

oldmean <- mean(databydate$tsteps, na.rm = TRUE)
newmean <- mean(FullSummedDataByDay$totalsteps)
# Old mean and New mean
oldmean
newmean


oldmedian <- median(databydate$tsteps, na.rm = TRUE)
newmedian <- median(FullSummedDataByDay$totalsteps)
# Old median and New median
oldmedian
newmedian


meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )


```{r}

## 9. Panel plot comparing the average number of steps taken per 5-minute interval across weekdays and weekends

meandata$date <- as.Date(meandata$date)
meandata$weekday <- weekdays(meandata$date)
meandata$weekend <- ifelse(meandata$weekday=="Saturday" | meandata$weekday=="Sunday", "Weekend", "Weekday" )

library(ggplot2)
meandataweekendweekday <- aggregate(meandata$steps , by= list(meandata$weekend, meandata$interval), na.omit(mean))
names(meandataweekendweekday) <- c("weekend", "interval", "steps")

ggplot(meandataweekendweekday, aes(x=interval, y=steps, color=weekend)) + geom_line()+
facet_grid(weekend ~.) + xlab("Interval") + ylab("Mean of Steps") +
    ggtitle("Comparison of Average Number of Steps in Each Interval")
    

```
