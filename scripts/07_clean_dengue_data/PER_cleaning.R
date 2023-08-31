#dengue in Peru PER
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Peru admin 2 (province)
setwd("~/Documents/projects/cases per degree/")
setwd("per_adm_ign_20200714_shp")
PERmunic <- st_read("per_admbnda_adm2_ign_20200714.shp") #ADM1_EN
PERmunic$ADM2_PCODE = as.factor(PERmunic$ADM2_PCODE)

setwd("../Peru")
PERdata <- read.csv("dengue_cases_by_district_week_2010_2020.csv") #had to open and save as UTF-8 #Distrito
#add PE and leading 0
PERdata$ADM2_PCODE = paste("PE",sprintf("%04d",PERdata$Cd.Prov), sep="")
PERdata = PERdata[which(PERdata$ADM2_PCODE != "PE00NA" ),] #remove NA
setdiff(levels(as.factor(PERdata$ADM2_PCODE)),levels(PERmunic$ADM2_PCODE))

PERdata_weekly = PERdata %>% count(Ano, Semana, ADM2_PCODE)

#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", PERdata_weekly$Semana)
PERdata_weekly$startdate = ISOweek2date(paste(PERdata_weekly$Ano,"-",week,"-",1, sep="")) 
PERdata_weekly$enddate = PERdata_weekly$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- PERdata_weekly %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

PERmonthly_long = tbl_split_long %>% 
  group_by(ADM2_PCODE, Semana, Year = year(.start), month = month(.start)) 
PERmonthly_long$duration = difftime(PERmonthly_long$.end, PERmonthly_long$.start, units = "days")
PERmonthly_long$duration = as.numeric(PERmonthly_long$duration, units = "days")
PERmonthly_long$n = as.numeric(PERmonthly_long$n)
#multiply cases by fraction days in each month
PERmonthly_long$CountValue = PERmonthly_long$n*(PERmonthly_long$duration/7)

#aggregate cases by adm/year/month
PERmonthly = aggregate(CountValue ~ ADM2_PCODE + month + Year, data = PERmonthly_long, FUN = sum)

#fill in zeros
PERmonthly <- PERmonthly %>%
  spread(ADM2_PCODE, CountValue, fill = 0) %>%
  gather(ADM2_PCODE, CountValue, -c(month,Year)) %>%
  mutate(ADM2_PCODE = tolower(ADM2_PCODE)) 

#PERmonthly= plyr::rename(PERmonthly, c("ADM2_PCODE" = "ADM"))

setwd("..")
write.csv(PERmonthly, "PER_cleaned.csv")


#####ADMIN 3 ####
setwd("~/Documents/projects/cases per degree/per_adm_ign_20200714_shp")
PERmunic <- st_read("per_admbnda_adm3_ign_20200714.shp") #ADM1_EN
PERmunic$ADM3_PCODE = as.factor(PERmunic$ADM3_PCODE)

setwd("~/Documents/projects/cases per degree/Peru")
PERdata <- read.csv("dengue_cases_by_district_week_2010_2020.csv") #had to open and save as UTF-8 #Distrito
#add PE and leading 0
PERdata$ADM3_PCODE = paste("PE",sprintf("%06d",PERdata$Cd.Dist), sep="")
PERdata = PERdata[which(PERdata$ADM3_PCODE != "PE0000NA" ),] #remove NA
# PE120604 Mazamari and PE120606 Pangoa = PE120699 Mazamari - Pangoa
PERdata$ADM3_PCODE[which(PERdata$ADM3_PCODE == "PE120604" |PERdata$ADM3_PCODE == "PE120606" )] <- "PE120699"
#eventually need to add the rows together
PERdata$ADM3_PCODE = as.factor(PERdata$ADM3_PCODE)
PERdata$ADM3_PCODE = droplevels(PERdata)$ADM3_PCODE
setdiff(levels(PERdata$ADM3_PCODE),levels(PERmunic$ADM3_PCODE))

