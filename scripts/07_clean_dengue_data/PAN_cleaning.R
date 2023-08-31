#dengue in Panama PAN
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Panama
setwd("~/Documents/projects/cases per degree")
setwd("pan_adm_gadm_20211020_shp")
PANmunic <- st_read("pan_admbnda_adm2_gadm_20211020.shp")
PANmunic$ADM1_ES = tolower(PANmunic$ADM1_ES)
#base[, terme := stri_trans_general(str = terme, id = "Latin-ASCII")]
PANmunic$ADM1_ES = as.factor(stri_trans_general(str = PANmunic$ADM1_ES, id = "Latin-ASCII"))

setwd("../Panama")
PANdata <- ldply(list.files(), read.csv, header=TRUE) #case data, Regiones
PANdata = plyr::rename(PANdata, c("Regiones" = "ADM1_ES"))
PANdata$ADM1_ES = tolower(PANdata$ADM1_ES)
#C. NGOBE BUGLE = ngobe bugle, SAN MIGUELITO is a district in Panama, Panama Metro and Panama Este are in Panama, I must assume Panama Norte is too.
PANdata$ADM1_ES[which(PANdata$ADM1_ES == "c. ngobe bugle" | PANdata$ADM1_ES == "ngobe-bugle" )] <- "ngobe bugle"
PANdata$ADM1_ES[which(PANdata$ADM1_ES == "panama este" | PANdata$ADM1_ES == "panama metro"| PANdata$ADM1_ES == "panama norte" | PANdata$ADM1_ES == "san miguelito")] <- "panama"
PANdata = PANdata[which(PANdata$ADM1_ES != "total" & PANdata$ADM1_ES != "importado"),]
PANdata$ADM1_ES = as.factor(PANdata$ADM1_ES)
PANdata = plyr::rename(PANdata, c("Año" = "Year", "Semana.Epi" = "Week"))
PANdata$Week = as.factor(PANdata$Week)
setdiff(levels(PANdata$ADM1_ES),levels(PANmunic$ADM1_ES)) #check they match

#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", PANdata$Week)
PANdata$startdate = ISOweek2date(paste(PANdata$Year,"-",week,"-",1, sep="")) 
PANdata$enddate = PANdata$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- PANdata %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

PANmonthly_long = tbl_split_long %>% 
  group_by(ADM1_ES, Week, Year = year(.start), month = month(.start)) 
PANmonthly_long$duration = difftime(PANmonthly_long$.end, PANmonthly_long$.start, units = "days")
PANmonthly_long$duration = as.numeric(PANmonthly_long$duration, units = "days")
PANmonthly_long$Total.de.Casos = as.numeric(PANmonthly_long$Total.de.Casos)
#multiply cases by fraction days in each month
PANmonthly_long$CountValue = PANmonthly_long$Total.de.Casos*(PANmonthly_long$duration/7)

#aggregate cases by adm/year/month
PANmonthly = aggregate(CountValue ~ ADM1_ES + month + Year, data = PANmonthly_long, FUN = sum)

#fill in zeros
PANmonthly <- PANmonthly %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year))

#export new data
setwd("..")
write.csv(PANmonthly, "PAN_cleaned_2020.csv")
