###########################################
#                                         #
#            Foreign Logic                # 
#              2021.01.29                 #
#                                         #
#              by Kevin                   #
###########################################

library(zoo)
library(timeDate)
library(DT)
library(dplyr)
library(tidyr)
library(stringr)
library(data.table)
library(reshape2)
library(lubridate)
library(readr)
library(rpivotTable)

# Analysis period
start_date <- '2019-10-01'
end_date <- '2019-12-31'

#####################################
#         Basic Analysis            #
#####################################

############ Read Session file ############
session_201910 <- fread("session_20stations_201910.csv", header = TRUE)
session_201911 <- fread("session_20stations_201911.csv", header = TRUE)
session_201912 <- fread("session_20stations_201912.csv", header = TRUE)
session_201910_12 <- rbind(session_201910, session_201911, session_201912)

session_two <- subset(session_201910_12, session_201910_12$shop_id %in% c('1227','1435'))
session_two <- session_two %>% subset(session_two$date >= '2019-10-01' & session_two$date <= '2019-12-31')

foreign_sessions <- subset(session_two, session_two$dwell_time > 0)
foreign_sessions$country <- gsub("\\,.*","",foreign_sessions$country)
foreign_sessions <- subset(foreign_sessions, foreign_sessions$country != "NULL")
foreign_sessions <- subset(foreign_sessions, foreign_sessions$country != "ZM")
foreign_sessions <- subset(foreign_sessions, foreign_sessions$country != "JP")

foreign_sessions <- subset(foreign_sessions, foreign_sessions$country == "JP")

enrichment <- function(foreign_sessions) {
  foreign_sessions <- foreign_sessions[order(foreign_sessions$date, foreign_sessions$time), ]
  foreign_sessions$date <- as.Date(foreign_sessions$date, "%Y-%m-%d")
  foreign_sessions$hour <- substr(foreign_sessions$time, 1, 2)
  foreign_sessions$weekday <- weekdays.Date(foreign_sessions$date)
  foreign_sessions$weekday<-factor(foreign_sessions$weekday, levels = c("Monday", "Tuesday", "Wednesday","Thursday","Friday","Saturday","Sunday"))
  foreign_sessions$wDays <- isWeekday(foreign_sessions$date, wday = 1:5)
  foreign_sessions$wDays <- ifelse(foreign_sessions$wDays == TRUE, "Weekday", "Weekend")
  foreign_sessions$Month_Yr <- format(as.Date(foreign_sessions$date), "%Y-%m")
  return(foreign_sessions)
}

foreign_sess <- enrichment(foreign_sessions)

foreign_cnt <- foreign_sess %>% group_by(date, country) %>% summarise(cnt = length(unique(wifi_id)))
foreign_cnt <- foreign_cnt %>% group_by(country) %>% summarise(cnt = sum(cnt))
foreign_cnt <- foreign_cnt[order(-foreign_cnt$cnt),]
foreign_cnt

# foreigner 
dwell_sessions <- foreign_sess
dwell_sessions$date <- as.Date(dwell_sessions$date)
dwell_sessions$hour <- substr(dwell_sessions$time, 1, 2)
dwell_sessions <- dwell_sessions[order(dwell_sessions$wifi_id, dwell_sessions$date, dwell_sessions$time), ]

dwell_sessions1 <- dwell_sessions %>% group_by(wifi_id) %>% mutate(min_date = min(date), max_date = max(date), date_difference = max_date - min_date)
#dwell_sessions1 <- dwell_sessions %>% group_by(wifi_id, shop_id) %>% mutate(min_date = min(date), max_date = max(date), date_difference = max_date - min_date)
#dwell_sessions1 <- dwell_sessions %>% group_by(wifi_id) %>% mutate(min_time = min(time), max_time = max(time), date_time = date_difference*60*60*24, time_difference = (max_time - min_time))
#dwell_sessions2 <- dwell_sessions1 %>% group_by(shop_id, date_difference) %>% summarise(cnt = length(unique(wifi_id)))
dwell_sessions2 <- dwell_sessions1 %>% group_by(date_difference) %>% summarise(cnt = length(unique(wifi_id)))

#在日外国人＆訪日外国人
foreign <- dwell_sessions1 %>% subset(dwell_sessions1$date_difference < 10)
zai <- dwell_sessions1 %>% subset(dwell_sessions1$date_difference > 9)

foreign_cnt <- foreign %>% group_by(date, country) %>% summarise(cnt = length(unique(wifi_id)))
#foreign_cnt <- zai %>% group_by(date, country) %>% summarise(cnt=length(unique(wifi_id)))
foreign_cnt <- foreign_cnt %>% group_by(country) %>% summarise(cnt=sum(cnt))
foreign_cnt <- foreign_cnt[order(-foreign_cnt$cnt),]
foreign_cnt

# Monthly
monthly_cnt <- foreign %>% group_by(date, Month_Yr) %>% summarise(cnt = length(unique(wifi_id)))
monthly_cnt <- zai %>% group_by(date, Month_Yr) %>% summarise(cnt = length(unique(wifi_id)))
monthly_cnt <- foreign_sess %>% group_by(date, Month_Yr) %>% summarise(cnt = length(unique(wifi_id)))

monthly_cnt <- monthly_cnt %>% group_by(Month_Yr) %>% summarise(cnt = mean(cnt))
monthly_cnt <- monthly_cnt %>% spread(Month_Yr, cnt)
monthly_cnt

# weekday/weekend
wDays_cnt <- foreign %>% group_by(date, wDays) %>% summarise(cnt = length(unique(wifi_id)))
wDays_cnt <- zai %>% group_by(date, wDays) %>% summarise(cnt = length(unique(wifi_id)))
wDays_cnt <- foreign_sess %>% group_by(date, wDays) %>% summarise(cnt = length(unique(wifi_id)))

wDays_cnt <- wDays_cnt %>% group_by(wDays) %>% summarise(cnt = mean(cnt))
wDays_cnt <- wDays_cnt %>% spread(wDays, cnt)
wDays_cnt

# weekday/weekend
weekday_cnt <- foreign %>% group_by(date, weekday) %>% summarise(cnt = length(unique(wifi_id)))
weekday_cnt <- zai %>% group_by(date, weekday) %>% summarise(cnt = length(unique(wifi_id)))
weekday_cnt <- foreign_sess %>% group_by(date, weekday) %>% summarise(cnt = length(unique(wifi_id)))

weekday_cnt <- weekday_cnt %>% group_by(weekday) %>% summarise(cnt = mean(cnt))
weekday_cnt <- weekday_cnt %>% spread(weekday, cnt)
weekday_cnt

# hourly
hourly_session <- foreign %>% group_by(date, hour) %>% summarise(cnt = length(unique(wifi_id)))
hourly_session <- zai %>% group_by(date, hour) %>% summarise(cnt = length(unique(wifi_id)))
hourly_session <- foreign_sess %>% group_by(date, hour) %>% summarise(cnt = length(unique(wifi_id)))

hourly_result <- hourly_session %>% group_by(hour) %>% summarise(cnt = mean(cnt))
hourly_result <- hourly_result %>% spread(hour, cnt) 
hourly_result

