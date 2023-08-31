#dengue in El Salvador SLV
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#El Salvador
setwd("~/Documents/projects/cases per degree")
setwd("slv_adm_gadm_2021_shp")
SLVmunic <- st_read("slv_admbnda_adm1_gadm_20210204.shp") #ADM1_ES
SLVmunic$ADM1_ES = as.factor(tolower(stri_trans_general(str = SLVmunic$ADM1_ES, id = "Latin-ASCII")))

setwd("..")
SLVdata <- read.csv("tycho_elsalvador.csv") #Admin1Name
SLVdata = plyr::rename(SLVdata, c("Admin1Name" = "ADM1_ES"))
SLVdata$ADM1_ES = as.factor(tolower(stri_trans_general(str = SLVdata$ADM1_ES, id = "Latin-ASCII")))
setdiff(levels(SLVdata$ADM1_ES),levels(SLVmunic$ADM1_ES))

#export new data
SLVdata = SLVdata[,c("ADM1_ES","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(SLVdata$PeriodStartDate), month = month(SLVdata$PeriodStartDate))
#add together the 2 reports for each adm and timepoint. Alternatively, I could delete rows marked "fatalities"
SLVdata = aggregate(CountValue ~ ADM1_ES + month + Year, data = SLVdata, FUN = sum)

#fill in zeros
SLVdata <- SLVdata %>%
  spread(ADM1_ES, CountValue, fill = 0) %>%
  gather(ADM1_ES, CountValue, -c(month,Year))
write.csv(SLVdata, "SLV_cleaned.csv")



#join with new gee data

#join era 5 files if more than 1 
setwd("from_gee")
era5SLV = plyr::ldply(list.files(pattern = "SLV_temp"), read.csv, header=TRUE)

#get era5 data into wide format where each row is year_month, each column is temp variable
era5SLVjoin = era5SLV %>% 
  spread(property,mean) %>%
  mutate(ADM1_ES = tolower(stri_trans_general(str = ADM1_ES, id = "Latin-ASCII"))) #***

#change date format to year_month to match era5 data then join to era5 and case data by year_month and ADM1_EN
SLVjoin = SLVdata[,c("ADM1_ES","month","Year","CountValue")] %>% 
  mutate(year_month = paste0(SLVdata$Year,sprintf("%02d",SLVdata$month))) %>% 
  merge(era5SLVjoin,by=c("ADM1_ES", "year_month")) 

#include temp a few months before 200310,200311,200312
#era5SLVjoin[which(era5SLVjoin$year_month == c(200310,200311,200312) ),]
casestart = min(paste0(SLVdata$Year,sprintf("%02d",SLVdata$month)))
SLVjoin2 = era5SLVjoin %>% 
  subset(year_month >= "199901" & year_month < casestart) %>%  
  #subset(year_month == c(199910,199911,199912)) %>% 
  plyr::rbind.fill(SLVjoin)
setwd("..")
write.csv(SLVjoin2, "SLV_joined.csv")
