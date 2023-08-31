#dengue in Thailand THA
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Thailand
setwd("~/Documents/projects/cases per degree")
setwd("tha_adm_rtsd_itos_20210121_shp")
THAmunic <- st_read("tha_admbnda_adm1_rtsd_20220121.shp") #ADM1_EN
THAmunic$ADM1_EN = as.factor(tolower(THAmunic$ADM1_EN))

setwd("..")
THAdata <- read.csv("tycho_thailand.csv") #Admin1Name
THAdata = plyr::rename(THAdata, c("Admin1Name" = "ADM1_EN"))
THAdata$ADM1_EN = tolower(THAdata$ADM1_EN)
#krung thep maha nakhon = bangkok
THAdata$ADM1_EN[which(THAdata$ADM1_EN == "krung thep maha nakhon")] <- "bangkok"
THAdata$ADM1_EN = as.factor(THAdata$ADM1_EN)
setdiff(levels(THAdata$ADM1_EN),levels(THAmunic$ADM1_EN))

#export data
THAdata = THAdata[,c("ADM1_EN","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(THAdata$PeriodStartDate), month = month(THAdata$PeriodStartDate))
#write.csv(THAdata, "THA_cleaned_2002.csv")

#transform 2011-2019 from wide to long format
setwd("Thailand/Thailand 11-19 csv")

THAdata_wide <-ldply(list.files(), read.csv, header=TRUE) #case data 2011-2019
colnames(THAdata_wide)[11] = "Sep"
THAdata_long <- gather(THAdata_wide[,1:14], month, CountValue, Jan:Dec, factor_key=TRUE)
#convert months to numbers
THAdata_long$month = match(THAdata_long$month, month.abb)
#THAdataMOH = plyr::rename(THAdata_long, c("ADM1_EN" = "ADM"))
#merge with tycho data
# #tycho has start and end date, while MoH data has month abbreviation
# THAdata$Year = substr(THAdata$PeriodStartDate,0,4)
# THAdata$Month = substr(THAdata$PeriodStartDate,6,7)
# THAdata_long$Month = c(rep(1,617),rep(2,617), rep(3,617),rep(4,617),rep(5,617),rep(6,617),rep(7,617),rep(8,617),rep(9,617),rep(10,617),rep(11,617),rep(12,617))
# 
# #export new data
# THAdataAll = rbind(THAdata[,c("ADM1_EN","Year","Month","CountValue")],THAdata_long[,c("ADM1_EN","Year","Month","CountValue")])

setwd("./../..")
#write.csv(THAdata_long, "THA_cleaned_2015.csv")
