#dengue in Honduras HND
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Honduras
setwd("~/Documents/projects/cases per degree")
setwd("hnd_adm_sinit_20161005_SHP")
HNDmunic <- st_read("hnd_admbnda_adm1_sinit_20161005.shp") #ADM1_ES
HNDmunic$ADM1_ES = as.factor(tolower(stri_trans_general(str = HNDmunic$ADM1_ES, id = "Latin-ASCII")))

setwd("..")
HNDdata <- read.csv("HND 2017-2019 wk.csv") #Admin1Name
HNDdata = plyr::rename(HNDdata, c("Department" = "ADM1_ES"))
HNDdata$ADM1_ES = as.factor(tolower(stri_trans_general(str = HNDdata$ADM1_ES, id = "Latin-ASCII")))
setdiff(levels(HNDdata$ADM1_ES),levels(HNDmunic$ADM1_ES))

#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", HNDdata$Week)
HNDdata$startdate = ISOweek2date(paste(HNDdata$Year,"-",week,"-",1, sep="")) 
HNDdata$enddate = HNDdata$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- HNDdata %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

HNDmonthly_long = tbl_split_long %>% 
  group_by(ADM1_ES, Week, Year = year(.start), month = month(.start)) 
HNDmonthly_long$duration = difftime(HNDmonthly_long$.end, HNDmonthly_long$.start, units = "days")
HNDmonthly_long$duration = as.numeric(HNDmonthly_long$duration, units = "days")
HNDmonthly_long$Cases = as.numeric(HNDmonthly_long$Cases)
#multiply cases by fraction days in each month
HNDmonthly_long$CountValue = HNDmonthly_long$Cases*(HNDmonthly_long$duration/7)

#aggregate cases by adm/year/month
HNDmonthly = aggregate(CountValue ~ ADM1_ES + month + Year, data = HNDmonthly_long, FUN = sum)

#fill in zeros
HNDmonthly <- HNDmonthly %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year))

#export new data
write.csv(HNDmonthly, "HND_cleaned_2018.csv")

