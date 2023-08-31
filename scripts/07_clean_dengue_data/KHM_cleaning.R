#dengue in Cambodia KHM
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Cambodia
setwd("~/Documents/projects/cases per degree")
setwd("khm_admbnda_adm1_gov_20181004")
KHMmunic <- st_read("khm_admbnda_adm1_gov_20181004.shp") #ADM1_EN
KHMmunic$ADM1_EN = as.factor(tolower(KHMmunic$ADM1_EN))

setwd("..")
KHMdata <- read.csv("tycho_cambodia.csv") #Admin1Name
KHMdata = plyr::rename(KHMdata, c("Admin1Name" = "ADM1_EN"))
KHMdata$ADM1_EN = tolower(stri_trans_general(str = KHMdata$ADM1_EN, id = "Latin-ASCII"))
#16 of the 25 have small differences in spelling
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "batdambang")] <- "battambang"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "kampong spoe")] <- "kampong speu"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "kampong thum")] <- "kampong thom"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "kaoh kong")] <- "koh kong"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "kracheh")] <- "kratie"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "krong keb")] <- "kep"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "krong pailin")] <- "pailin"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "krong preah sihanouk")] <- "preah sihanouk"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "mondol kiri")] <- "mondul kiri"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "otdar meanchey")] <- "oddar meanchey"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "phnum penh")] <- "phnom penh"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "pouthisat")] <- "pursat"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "rotanokiri")] <- "ratanak kiri"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "siemreab")] <- "siemreap"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "stoeng treng")] <- "stung treng"
KHMdata$ADM1_EN[which(KHMdata$ADM1_EN == "takev")] <- "takeo"
KHMdata$ADM1_EN = as.factor(KHMdata$ADM1_EN)
setdiff(levels(KHMdata$ADM1_EN),levels(KHMmunic$ADM1_EN))

#export new data
KHMdata = KHMdata[,c("ADM1_EN","PeriodStartDate","CountValue")] %>% 
  mutate(Year = year(KHMdata$PeriodStartDate), month = month(KHMdata$PeriodStartDate))
#add together the 2 reports for each adm and timepoint. Alternatively, I could delete rows marked "fatalities"
KHMdata = aggregate(CountValue ~ ADM1_EN + month + Year, data = KHMdata, FUN = sum)

#fill in zeros
KHMdata <- KHMdata %>%
  spread(ADM1_EN, CountValue, fill = 0) %>%
  gather(ADM1_EN, CountValue, -c(month,Year))

write.csv(KHMdata, "KHM_cleaned_2004.csv")

#xtabs(~ ADM1_EN, data = KHMdata)