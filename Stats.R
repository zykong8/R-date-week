# ENV
rm(list = ls())
gc()

library(readxl)
library(dplyr)
library(magrittr)
library(sysfonts)
library(showtextdb)
library(showtext)
library(ggplot2)
showtext_auto()

stat <- read_excel("demo.xlsx", col_names = TRUE)
stat <- stat %>% filter(status==2)

#' @title Get the week of the current month for a specific date
#' @param dates description The data type is POSIXct (1970-01-01 8:00)
#' @param WRange description Which unit is used to calculate the number of weeks("month", "year")
#' @param FDay Which specific day should be used as the first day of the week
#' @return In the first week of a specific unit(WRange)
FetchNthWeek <- function(dates = NULL, WRange = c("month","year"), FDay = "Sunday"){
  # Packages
  require(lubridate)
  
  # Parameters
  WRange <- match.arg(WRange)
  WRange<- tolower(WRange[1])
  FDay <- match.arg(FDay, choices = c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday"))
  
  # dayNames and dayIndex are for beginning the week on a day other than Sunday
  # (this vector ordering matters, so careful about changing it)
  dayNames<- c("Monday","Tuesday","Wednesday","Thursday","Friday","Saturday","Sunday")
  
  # index integer of first match
  dayIndex<- pmatch(tolower(FDay), tolower(dayNames))[1]
  
  ### Calculate week index of each day
  if (!is.na(pmatch(WRange, "year"))){
    # For year:
    # sum the day of year, index for day of week at start of year, and constant 5 
    #  then integer divide quantity by 7   
    # (explicit on package so lubridate and data.table don't fight)
    nWeek<- (5 + lubridate::yday(dates) + lubridate::wday(floor_date(dates, 'year'), week_start = dayIndex)) %/% 7
  }else{
    # For month:
    # same algorithm as above, but for month rather than year
    nWeek<- (5 + lubridate::day(dates) + lubridate::wday(floor_date(dates, 'month'), week_start = dayIndex)) %/% 7
  }
  
  # naming very helpful for review
  names(nWeek)<- paste0(lubridate::wday(dates,T), '-', dates)
  
  return(nWeek)
  
}

# 获取指定日期，在日历中真实的第一个星期
FetchNthWeek(as.Date("2024-06-30"), WRange = "month")

# 将一个月按照7天进行拆分，分为4周
diffTimes <- data.frame(
  "StartTime" = stat$start_time,
  "StartTime2"= lapply(stat$start_time, function(s){
    str_split(s, pattern = " ")[[1]][1]
  }) %>% unlist,
  "StartDays" = lapply(stat$start_time, function(s){
    tmp <- str_split(s, pattern = "-")[[1]][3]
    as.numeric(str_split(tmp, pattern = " ")[[1]][1])
  }) %>% unlist,
  "EndTime" = stat$end_time,
  "DiffTime" = as.numeric(difftime(stat$end_time, stat$start_time, units = "mins")),
  "Years" = lapply(stat$start_time, function(x){
    format(x, format = "%Y")
  }) %>% unlist(),
  "Months" = lapply(stat$start_time, function(x){
    format(x, format = "%B")
  }) %>% unlist(),
  "Weeks" = lapply(stat$start_time, function(x){
    FetchNthWeek(x, WRange = "month")
  }) %>% unlist()
) %>% mutate(
  "YearsMonths" = factor(
    x = paste0(Years, "-", Months),
    levels = c(
      "2023-十一月", "2023-十二月",    
      "2024-一月", "2024-二月", "2024-三月", "2024-四月", "2024-五月", "2024-六月",
      "2024-七月", "2024-八月", "2024-九月", "2024-十月"
    )
  ),
  .before = "Weeks"
) %>% mutate(
  "Weeks" = factor(
    x = paste0("W", Weeks),
    levels = paste0("W", 1:6)
  ),
  "Weeks2" = case_when(
    StartDays >= 22 ~ "W4",
    StartDays >= 15 ~ "W3",
    StartDays >= 8 ~ "W2",
    TRUE ~ "W1"
  ),
  "Weeks2" = factor(
    x = Weeks2,
    levels = paste0("W", 1:4)
  )
)

pek <- diffTimes %>% group_by(YearsMonths, Weeks2) %>%
  summarise(
    n = n()
  )

ggplot(data = pek, aes(x = Weeks2, y = n, fill = YearsMonths, group = 1)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(y = n*1.1,label = n)
  ) +
  scale_x_discrete(
    name = "Weeks"
  ) +
  scale_y_continuous(
    name = "Numbers of project"
  ) +
  scale_fill_discrete(
    name = NULL
  ) +
  facet_wrap(.~YearsMonths)









