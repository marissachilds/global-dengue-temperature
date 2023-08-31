#dengue in Venezuela VEN
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Venezuela
setwd("~/Documents/projects/cases per degree")
setwd("ven_adm_ine_20210223_shp")
  VENmunic <- st_read("ven_admbnda_adm1_ine_20210223.shp") #ADM1_ES
  VENmunic$ADM1_ES = as.factor(tolower(stri_trans_general(str = VENmunic$ADM1_ES, id = "Latin-ASCII")))

setwd("..")
  VENdata <- read.csv("tycho_venezuela.csv") #Admin1Name
  VENdata = plyr::rename(VENdata, c("Admin1Name" = "ADM1_ES"))
  VENdata$ADM1_ES = as.factor(tolower(stri_trans_general(str = VENdata$ADM1_ES, id = "Latin-ASCII")))
  setdiff(levels(VENdata$ADM1_ES),levels(VENmunic$ADM1_ES))

VENdata = VENdata[,c("ADM1_ES","PeriodStartDate","CountValue")] %>% 
  mutate(Year = format(mdy(VENdata$PeriodStartDate), "%Y"), month = format(mdy(VENdata$PeriodStartDate), "%m"))

VENdata = aggregate(CountValue ~ ADM1_ES + month + Year, data = VENdata, FUN = sum)

#fill in zeros
VENdata <- VENdata %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year))

write.csv(VENdata, "VEN_cleaned_2003.csv")

#xtabs(~ ADM1_ES, data = VENdata)

  
#new paho data 2018-2021
setwd("Venezuela")
  VENdata_recent <-ldply(list.files(), read.csv, skip = 1, fileEncoding = "UTF-16", sep = "\t", header = FALSE) #case data paho
  colnames(VENdata_recent) <- c('Year','Week', 'ADM1_ES','Total.Cases','Probable.Cases')
  #delete the total rows
  VENdata_recent = VENdata_recent[which(VENdata_recent$Year != "Total"),1:5]
  VENdata_recent = VENdata_recent %>% mutate_at(c('Year', 'Week','Total.Cases','Probable.Cases'), as.numeric)
  #vargas became La Guaira in 2019
  VENdata_recent$ADM1_ES = as.factor(tolower(VENdata_recent$ADM1_ES))
  VENdata_recent$ADM1_ES[which(VENdata_recent$ADM1_ES == "vargas")] <- "la guaira"
  #setdiff(levels(VENdata_recent$ADM1_ES),levels(VENmunic$ADM1_ES)) 

#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", VENdata_recent$Week)
VENdata_recent$startdate = ISOweek2date(paste(VENdata_recent$Year,"-",week,"-",1, sep="")) 
VENdata_recent$enddate = VENdata_recent$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- VENdata_recent %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

VENmonthly_long = tbl_split_long %>% 
  group_by(ADM1_ES, Week, Year = year(.start), month = month(.start)) 
VENmonthly_long$duration = difftime(VENmonthly_long$.end, VENmonthly_long$.start, units = "days")
VENmonthly_long$duration = as.numeric(VENmonthly_long$duration, units = "days")
VENmonthly_long$Total.Cases = as.numeric(VENmonthly_long$Total.Cases)
#multiply cases by fraction days in each month
VENmonthly_long$CountValue = VENmonthly_long$Total.Cases*(VENmonthly_long$duration/7)

#aggregate cases by adm/year/month
VENmonthly = aggregate(CountValue ~ ADM1_ES + month + Year, data = VENmonthly_long, FUN = sum)

#fill in zeros
VENmonthly <- VENmonthly %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year)) 


#export new data
setwd("..")
write.csv(VENmonthly, "VEN_cleaned_2020.csv")
