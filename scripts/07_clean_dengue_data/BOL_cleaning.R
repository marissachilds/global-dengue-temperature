#dengue in Bolivia BOL
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Bolivia
setwd("~/Documents/projects/cases per degree/")
setwd("bol_adm_gb2014_shp")
BOLmunic <- st_read("bol_admbnda_adm1_gov_2020514.shp") #ADM1_ES
BOLmunic$ADM1_ES = as.factor(tolower(stri_trans_general(str = BOLmunic$ADM1_ES, id = "Latin-ASCII")))
setwd("..")
BOLdata <- read.csv("Bolivia20142021state.csv") 
BOLdata = plyr::rename(BOLdata, c("Department" = "ADM1_ES"))
BOLdata$ADM1_ES = as.factor(tolower(stri_trans_general(str = BOLdata$ADM1_ES, id = "Latin-ASCII")))
setdiff(levels(BOLdata$ADM1_ES),levels(BOLmunic$ADM1_ES))

#export new data
#write.csv(BOLdata, "BOL_cleaned.csv")

#weekly to monthly
library(lubridate)
#get start and end dates for each week of years 2014-2021
#BOLdata$startdate = lubridate::parse_date_time(paste(BOLdata$Year, BOLdata$Epi.Week, 1, sep="/"),'Y/W/u') #this is an established lubridate issue with year 2021 https://petrbouchal.xyz/post/weekly-dates-r/ start date should be in 2020
library(ISOweek)
week = sprintf("W%02d", BOLdata$Epi.Week)
BOLdata$startdate = ISOweek2date(paste(BOLdata$Year,"-",week,"-",1, sep="")) #"2020-W53-1"
BOLdata$enddate = BOLdata$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- BOLdata %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

BOLmonthly_long = tbl_split_long %>% 
  group_by(ADM1_ES, Epi.Week, Year = year(.start), month = month(.start)) 
BOLmonthly_long$duration = difftime(BOLmonthly_long$.end, BOLmonthly_long$.start, units = "days")
BOLmonthly_long$duration = as.numeric(BOLmonthly_long$duration, units = "days")
BOLmonthly_long$Total.Cases = as.numeric(BOLmonthly_long$Total.Cases)
#multiply cases by fraction days in each month
BOLmonthly_long$CountValue = BOLmonthly_long$Total.Cases*(BOLmonthly_long$duration/7)

#aggregate cases by adm/year/month
BOLmonthly = aggregate(CountValue ~ ADM1_ES + month + Year, data = BOLmonthly_long, FUN = sum)
#BOLmonthly= plyr::rename(BOLmonthly, c("ADM1_ES" = "ADM"))

#fill in zeros
BOLmonthly <- BOLmonthly %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year))
#export new data
write.csv(BOLmonthly, "BOL_cleaned_2018.csv")

xtabs(~ ADM1_ES, data = BOLmonthly)
