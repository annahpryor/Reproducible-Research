setwd("C:/Users/Anna/Documents/Data science Series/data")

data = read.csv("activity.csv")
library(dplyr)

str(data)

data_summary = data %>% group_by(date) %>% summarise_each(funs(sum), steps)

hist(data_summary$steps,xlab="steps",col='red',main="Histogram of Steps")

mean(data_summary$steps,na.rm=TRUE)
median(data_summary$steps,na.rm=TRUE)

data_summary2 = data %>% group_by(interval) %>% summarise_each(funs(mean(.,na.rm=TRUE)), steps)
plot(data_summary2$interval,data_summary2$steps,type='l')

data_summary2[which.max(data_summary2$steps),]

sum(is.na(data$steps))

##Replace NA values in data with averages over
##the intervals
for(i in 1:length(data$steps)){
  if (is.na(data$steps[i])){
    data$steps[i] = data_summary2$steps[
      which(data_summary2$interval == 
              data$interval[i])]
  }
}

data_summary3 = data %>% group_by(date) %>% summarise_each(funs(sum), steps)

hist(data_summary3$steps,xlab="steps",col='red',main="Histogram of Steps")
mean(data_summary3$steps)
median(data_summary3$steps)


for(i in 1:length(data$steps)){ 
  if(weekdays(as.Date(data$date[i]))==
     "Saturday" || weekdays(as.Date(
       data$date[i]))=="Sunday"){
    data$day[i] = "weekend"} else{
      data$day[i]="weekday"} }


data_summary4 = data %>% group_by(day,interval)%>%
  summarise_each(funs(mean(.,na.rm=TRUE)), 
                 steps)

data_weekday = subset(data_summary4,day==
                        "weekday",select = 
                        interval:steps)

data_weekend = subset(data_summary4,day==
                        "weekend",select =
                        interval:steps)

plot(data_weekend$interval,data_weekend$steps,
     type = 'l')