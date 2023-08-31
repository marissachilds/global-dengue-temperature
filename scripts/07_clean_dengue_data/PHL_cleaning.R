#dengue in Philippines PHL
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)
library(lubridate)

#Philippines
setwd("~/Documents/projects/cases per degree")
setwd("phl_adminboundaries_candidate_exclude_adm3")
PHLmunic <- st_read("phl_admbnda_adm2_psa_namria_20200529.shp") #municipality
PHLmunic$ADM2_EN = as.factor(tolower(PHLmunic$ADM2_EN))

setwd("../Philippines")
PHLdata <- ldply(list.files(), read.csv, header=TRUE) #case data, Admin2Name 
PHLdata = plyr::rename(PHLdata, c("Admin2Name" = "ADM2_EN"))
PHLdata$ADM2_EN = as.factor(tolower(PHLdata$ADM2_EN))
#remove "province of" text from PHLdata
PHLdata$ADM2_EN = as.factor(gsub("province of ","",as.character(PHLdata$ADM2_EN)))
setdiff(levels(PHLdata$ADM2_EN),levels(PHLmunic$ADM2_EN)) #check they match


#export new data
setwd("..")
PHLdata = PHLdata[,c("ADM2_EN","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(PHLdata$PeriodStartDate), month = month(PHLdata$PeriodStartDate))  %>%
  complete(ADM2_EN, Year, month, fill = list(CountValue = 0))

#write.csv(PHLdata, "PHL_cleaned_2002.csv")
