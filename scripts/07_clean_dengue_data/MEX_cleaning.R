#dengue in Mexico
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Mexico
setwd("~/Documents/projects/cases per degree")
setwd("mex_admbnda_govmex_20210618_SHP")
  MEXmunic <- st_read("mex_admbnda_adm1_govmex_20210618.shp") #ADM1_ES
  MEXmunic$ADM1_ES = tolower(stri_trans_general(str = MEXmunic$ADM1_ES, id = "Latin-ASCII"))

setwd("..")
  MEXdata <- read.csv("Mexico_data_subset03-20.csv") #Admin1Name
  MEXdata$ADM1_ES = tolower(stri_trans_general(str = MEXdata$ADM1_ES, id = "Latin-ASCII"))

#coahuila to coahuila de zaragoza
#michoacan to michoacan de ocampo
#queretaro to queretaro de arteaga
#veracruz to veracruz de ignacio de la llave
MEXdata$ADM1_ES[which(MEXdata$ADM1_ES == "coahuila")] <- "coahuila de zaragoza"
MEXdata$ADM1_ES[which(MEXdata$ADM1_ES == "michoacan")] <- "michoacan de ocampo"
MEXdata$ADM1_ES[which(MEXdata$ADM1_ES == "queretaro")] <- "queretaro de arteaga"
MEXdata$ADM1_ES[which(MEXdata$ADM1_ES == "veracruz")] <- "veracruz de ignacio de la llave"

  setdiff(as.factor(levels(MEXdata$ADM1_ES)),as.factor(levels(MEXmunic$ADM1_ES)))

#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", MEXdata$semana)
MEXdata$startdate = ISOweek2date(paste(MEXdata$Year,"-",week,"-",1, sep="")) 
MEXdata$enddate = MEXdata$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- MEXdata %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

MEXmonthly_long = tbl_split_long %>% 
  group_by(ADM1_ES, semana, Year = year(.start), month = month(.start)) 
MEXmonthly_long$duration = difftime(MEXmonthly_long$.end, MEXmonthly_long$.start, units = "days")
MEXmonthly_long$duration = as.numeric(MEXmonthly_long$duration, units = "days")
MEXmonthly_long$CountValue = as.numeric(MEXmonthly_long$CountValue)
#multiply CountValue by fraction days in each month
MEXmonthly_long$CountValue = MEXmonthly_long$CountValue*(MEXmonthly_long$duration/7)

#aggregate CountValue by adm/year/month
MEXmonthly = aggregate(CountValue ~ ADM1_ES + month + Year, data = MEXmonthly_long, FUN = sum)

#fill in zeros
MEXmonthly <- MEXmonthly %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year)) %>%
  mutate(ADM1_ES = tolower(ADM1_ES)) 

#export new data
#write.csv(MEXmonthly, "MEX_cleaned_2011.csv")

  
