#dengue in Malaysia MYS
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Malaysia
setwd("~/Documents/projects/cases per degree")
setwd("mys_admb_unhcr_20210211_shp")
MYSmunic <- st_read("mys_admbnda_adm1_unhcr_20210211.shp") #ADM1_EN
MYSmunic$ADM1_EN = as.factor(tolower(MYSmunic$ADM1_EN))

setwd("..")
MYSdata <- read.csv("tycho_malaysia.csv") #Admin1Name
MYSdata = plyr::rename(MYSdata, c("Admin1Name" = "ADM1_EN"))
MYSdata$ADM1_EN = tolower(stri_trans_general(str = MYSdata$ADM1_EN, id = "Latin-ASCII"))
#plot(st_simplify(MYSmunic[3], dTolerance = 5e3))
MYSdata$ADM1_EN[which(MYSdata$ADM1_EN == "negeri sembilan")] <- "negeri"
MYSdata$ADM1_EN = as.factor(MYSdata$ADM1_EN)
MYSdata$ADM1_EN = droplevels(MYSdata)$ADM1_EN
setdiff(levels(MYSdata$ADM1_EN),levels(MYSmunic$ADM1_EN))

#export new data
MYSdata = MYSdata[,c("ADM1_EN","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(MYSdata$PeriodStartDate), month = month(MYSdata$PeriodStartDate))
#add together the 2 reports for each adm and timepoint. Alternatively, I could delete rows marked "fatalities"
MYSdata = aggregate(CountValue ~ ADM1_EN + month + Year, data = MYSdata, FUN = sum)

#fill in zeros
MYSdata <- MYSdata %>%
  spread(ADM1_EN, CountValue, fill = 0) %>%
  gather(ADM1_EN, CountValue, -c(month,Year))
write.csv(MYSdata, "MYS_cleaned_2002.csv")

#xtabs(~ ADM1_EN, data = MYSdata)
