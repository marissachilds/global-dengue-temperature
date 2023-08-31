#dengue in Indonesia IDN
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)
library(googledrive)

#Indonesia
setwd("~/Documents/projects/cases per degree")
setwd("idn_adm_bps_20200401_shp")
IDNmunic <- st_read("idn_admbnda_adm1_bps_20200401.shp") #ADM1_EN
IDNmunic$ADM1_EN = as.factor(tolower(IDNmunic$ADM1_EN))

setwd("..")
IDNdata <- read.csv("tycho_indonesia.csv") #Admin1Name
IDNdata = plyr::rename(IDNdata, c("Admin1Name" = "ADM1_EN"))
IDNdata$ADM1_EN = tolower(IDNdata$ADM1_EN)
#jakarta raya = dki jakarta, yogyakarta = daerah istimewa yogyakarta
IDNdata$ADM1_EN[which(IDNdata$ADM1_EN == "jakarta raya")] <- "dki jakarta"
IDNdata$ADM1_EN[which(IDNdata$ADM1_EN == "yogyakarta")] <- "daerah istimewa yogyakarta"
IDNdata$ADM1_EN = as.factor(IDNdata$ADM1_EN)
setdiff(levels(IDNdata$ADM1_EN),levels(IDNmunic$ADM1_EN))


#export new data
IDNdata = IDNdata[,c("ADM1_EN","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(IDNdata$PeriodStartDate), month = month(IDNdata$PeriodStartDate))
#add together the 2 reports for each adm and timepoint. Alternatively, I could delete rows marked "fatalities"
IDNdata = aggregate(CountValue ~ ADM1_EN + month + Year, data = IDNdata, FUN = sum)

#fill in zeros
IDNdata <- IDNdata %>%
  spread(ADM1_EN, CountValue, fill = 0) %>%
  gather(ADM1_EN, CountValue, -c(month,Year))
write.csv(IDNdata, "IDN_cleaned_2005.csv")


