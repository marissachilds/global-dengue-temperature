#dengue in Nicaragua NIC
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)

#Nicaragua
setwd("~/Documents/projects/cases per degree")
setwd("nic_admbnda_inide_itos_20210826_shp")
NICmunic <- st_read("nic_admbnda_adm1_inide_itos_20210826.shp") #ADM1_ES
NICmunic$ADM1_ES = as.factor(tolower(stri_trans_general(str = NICmunic$ADM1_ES, id = "Latin-ASCII")))

setwd("..")
NICdata = read.csv("tycho_nicaragua_v2.csv") #Admin1Name
NICdata = plyr::rename(NICdata, c("Admin1Name" = "ADM1_ES"))
NICdata$ADM1_ES = as.factor(tolower(stri_trans_general(str = NICdata$ADM1_ES, id = "Latin-ASCII")))
setdiff(levels(NICdata$ADM1_ES),levels(NICmunic$ADM1_ES))

NICdata = NICdata[,c("ADM1_ES","PeriodStartDate","CountValue")] %>% 
  mutate(Year = format(ymd(NICdata$PeriodStartDate), "%Y"), month = format(ymd(NICdata$PeriodStartDate), "%m"))
NICdata = NICdata[which(NICdata$Year < 2005),]
#write.csv(NICdata, "NIC_cleaned_2003.csv")

#new paho data 2018-2021
setwd("Nicaragua")
#NICdata_recent <- ldply(list.files(), read.csv, skip = 1, fileEncoding = "UTF-16", sep = "\t", header = FALSE) #case data paho
NICdata_recent = read.csv("NIC_Paho_data.csv",fileEncoding = "UTF-16",sep="\t")
colnames(NICdata_recent) <- c("Year","Epi_weeks", "ADM1_ES","Indicator", "Week","Total.Cases")
NICdata_recent = NICdata_recent[which(NICdata_recent$Indicator=="Casos Confirmados"),]
NICdata_recent$ADM1_ES = tolower(NICdata_recent$ADM1_ES)
#fix spelling differences, "region autonoma del atlantico sur" = "region autonoma atlantico sur"
NICdata_recent$ADM1_ES[which(NICdata_recent$ADM1_ES == "region autonoma del atlantico sur")] <- "region autonoma atlantico sur"
# "las minas" and "bilwi" make up the left and right halves of the region autonoma atlantico norte according to the paho map, so renamed *and need to be added together
NICdata_recent$ADM1_ES[which(NICdata_recent$ADM1_ES == "las minas" |NICdata_recent$ADM1_ES == "bilwi" )] <- "region autonoma atlantico norte"
#"zelaya central" is a de facto department that is located within region autonoma atlantico sur. *they need to be added together 
NICdata_recent$ADM1_ES[which(NICdata_recent$ADM1_ES == "zelaya central" )] <- "region autonoma atlantico sur"
NICdata_recent$ADM1_ES = as.factor(NICdata_recent$ADM1_ES)
setdiff(levels(NICdata_recent$ADM1_ES),levels(NICmunic$ADM1_ES))
                              
#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", NICdata_recent$Week)
NICdata_recent$startdate = ISOweek2date(paste(NICdata_recent$Year,"-",week,"-",1, sep="")) 
NICdata_recent$enddate = NICdata_recent$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- NICdata_recent %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

NICmonthly_long = tbl_split_long %>% 
  group_by(ADM1_ES, Week, Year = year(.start), month = month(.start)) 
NICmonthly_long$duration = difftime(NICmonthly_long$.end, NICmonthly_long$.start, units = "days")
NICmonthly_long$duration = as.numeric(NICmonthly_long$duration, units = "days")
NICmonthly_long$Total.Cases = as.numeric(NICmonthly_long$Total.Cases)
#multiply cases by fraction days in each month
NICmonthly_long$CountValue = NICmonthly_long$Total.Cases*(NICmonthly_long$duration/7)

#aggregate cases by adm/year/month
NICmonthly = aggregate(CountValue ~ ADM1_ES + month + Year, data = NICmonthly_long, FUN = sum)

#fill in zeros
NICmonthly <- NICmonthly %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year))

#export new data
setwd("..")
#write.csv(NICmonthly, "NIC_cleaned_2012.csv")
