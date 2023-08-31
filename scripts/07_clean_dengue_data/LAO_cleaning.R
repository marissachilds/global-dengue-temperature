#dengue in Laos LAO
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)
#Laos
setwd("~/Documents/projects/cases per degree")
setwd("lao_adm_ngd_20191112_SHP")
LAOmunic <- st_read("lao_admbnda_adm1_ngd_20191112.shp") #ADM1_EN
LAOmunic$ADM1_EN = as.factor(tolower(LAOmunic$ADM1_EN))

setwd("..")
LAOdata <- read.csv("tycho_laos.csv") #Admin1Name
LAOdata = plyr::rename(LAOdata, c("Admin1Name" = "ADM1_EN"))
LAOdata$ADM1_EN = tolower(stri_trans_general(str = LAOdata$ADM1_EN, id = "Latin-ASCII"))
#10 of the 17 have small differences in spelling
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "attapu")] <- "attapeu"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "champasak")] <- "champasack"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "louang namtha")] <- "louangnamtha"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "phongsali")] <- "phongsaly"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "viangchan [prefecture]")] <- "vientiane"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "viangchan")] <- "vientiane capital"
#vientiane capital is not a subset of vientiane the province (although very correlated)
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "xaignabouli")] <- "xaignabouly"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "xaisomboun")] <- "xaisomboon"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "xekong")] <- "sekong"
LAOdata$ADM1_EN[which(LAOdata$ADM1_EN == "xiangkhoang")] <- "xiengkhouang"
LAOdata$ADM1_EN = as.factor(LAOdata$ADM1_EN)
setdiff(levels(LAOdata$ADM1_EN),levels(LAOmunic$ADM1_EN))

#export new data
LAOdata = LAOdata[,c("ADM1_EN","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(LAOdata$PeriodStartDate), month = month(LAOdata$PeriodStartDate))

#fill in zeros
LAOdata <- LAOdata %>%
  spread(ADM1_EN, CountValue, fill = 0) %>%
  gather(ADM1_EN, CountValue, -c(month,Year))

write.csv(LAOdata, "LAO_cleaned_2004.csv")

#xtabs(~ ADM1_EN, data = LAOdata)
