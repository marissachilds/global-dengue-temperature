#dengue in Vietnam VNM
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)
#Vietnam
setwd("~/Documents/projects/cases per degree")
setwd("vnm_adm_gov_20201027_shp")
VNMmunic <- st_read("vnm_admbnda_adm1_gov_20201027.shp") #ADM1_EN
VNMmunic$ADM1_EN = as.factor(tolower(VNMmunic$ADM1_EN))

setwd("..")
VNMdata <- read.csv("tycho_vietnam.csv") #Admin1Name
VNMdata = plyr::rename(VNMdata, c("Admin1Name" = "ADM1_EN"))
VNMdata$ADM1_EN = tolower(VNMdata$ADM1_EN)
# "ba ria-vung tau"= ba ria - vung tau, can tho = can tho city, da nang = da nang city, hai phong = hai phong city, ho chi minh = ho chi minh city, thua thien-hue = thua thien hue
VNMdata$ADM1_EN[which(VNMdata$ADM1_EN == "ba ria-vung tau")] <- "ba ria - vung tau"
VNMdata$ADM1_EN[which(VNMdata$ADM1_EN == "can tho")] <- "can tho city"
VNMdata$ADM1_EN[which(VNMdata$ADM1_EN == "da nang")] <- "da nang city"
VNMdata$ADM1_EN[which(VNMdata$ADM1_EN == "hai phong")] <- "hai phong city"
VNMdata$ADM1_EN[which(VNMdata$ADM1_EN == "ho chi minh")] <- "ho chi minh city"
VNMdata$ADM1_EN[which(VNMdata$ADM1_EN == "thua thien-hue")] <- "thua thien hue"
# ha tay is a former province, now part of ha noi
VNMdata$ADM1_EN[which(VNMdata$ADM1_EN == "ha tay")] <- "ha noi"
VNMdata$ADM1_EN = as.factor(VNMdata$ADM1_EN)
setdiff(levels(VNMdata$ADM1_EN),levels(VNMmunic$ADM1_EN))

VNMdata = VNMdata[,c("ADM1_EN","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(VNMdata$PeriodStartDate), month = month(VNMdata$PeriodStartDate))

VNMdata = aggregate(CountValue ~ ADM1_EN + month + Year, data = VNMdata, FUN = sum)

#fill in zeros
VNMdata <- VNMdata %>%
  spread(ADM1_EN, CountValue, fill = 0) %>%
  gather(ADM1_EN, CountValue, -c(month,Year))
write.csv(VNMdata, "VNM_cleaned.csv")

#xtabs(~ ADM1_EN, data = VNMdata)



