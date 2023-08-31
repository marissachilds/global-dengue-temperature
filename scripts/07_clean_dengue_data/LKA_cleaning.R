#dengue in Sri Lanka LKA
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Sri Lanka
setwd("~/Documents/projects/cases per degree/")
setwd("lka_adm_slsd_20200305_shp")
LKAmunic <- st_read("lka_admbnda_adm2_slsd_20200305.shp")
LKAmunic$ADM2_EN = as.factor(tolower(LKAmunic$ADM2_EN))

setwd("../Sri Lanka")
LKAdata <- ldply(list.files(), read.csv, header=TRUE) #case data, RDHS 
LKAdata = plyr::rename(LKAdata, c("RDHS" = "ADM2_EN"))
LKAdata$ADM2_EN = tolower(LKAdata$ADM2_EN)
#"apura" = Anuradhapura, "moneragala"= monaragala, "mulativu" = mullaitivu
LKAdata$ADM2_EN[which(LKAdata$ADM2_EN == "apura")] <- "anuradhapura"
LKAdata$ADM2_EN[which(LKAdata$ADM2_EN == "moneragala")] <- "monaragala"
LKAdata$ADM2_EN[which(LKAdata$ADM2_EN == "mulativu")] <- "mullaitivu"
LKAdata = LKAdata[which(LKAdata$ADM2_EN != "kalmunai"),] #"kalmunai" is a city in ampara district
LKAdata$ADM2_EN = as.factor(LKAdata$ADM2_EN)
setdiff(levels(LKAdata$ADM2_EN),levels(LKAmunic$ADM2_EN)) #check they match

#convert to long format with columns c("ADM1_EN","month","Year","CountValue")
LKAdata = gather(LKAdata, month, CountValue, January:December, factor_key=TRUE)
LKAdata$month = recode(LKAdata2$month, "January"=1, "February"=2, "March"=3, "April"=4, "May"=5, "June"=6, "July"=7, "August"=8, "September"=9, "October"=10, "November"=11, "December"=12)
LKAdata = na.omit(LKAdata)   

#export new data
setwd("..")
write.csv(LKAdata, "LKA_cleaned.csv")

#Tycho dataset admin unit 1
setwd("lka_adm_slsd_20200305_shp")
LKA2munic <- st_read("lka_admbnda_adm1_slsd_20200305.shp")
LKA2munic$ADM1_EN = as.factor(tolower(LKA2munic$ADM1_EN))

setwd("..")
LKA2data <- read.csv("tycho_srilanka.csv") #Admin1Name
LKA2data = plyr::rename(LKA2data, c("Admin1Name" = "ADM1_EN"))
LKA2data$ADM1_EN = tolower(LKA2data$ADM1_EN)
LKA2data$ADM1_EN = as.factor(gsub(" province","",as.character(LKA2data$ADM1_EN)))
setdiff(levels(as.factor(LKA2data$ADM1_EN)),levels(LKA2munic$ADM1_EN))

#subset ADM1_EN and CountValue, create month and Year columns
library(lubridate)
LKA2data$Year = year(LKA2data$PeriodStartDate)
LKA2data$month = month(LKA2data$PeriodStartDate)

LKA2data = LKA2data[,c("ADM1_EN","month","Year","CountValue")]
LKA2data = aggregate(CountValue ~ ADM1_EN + month + Year, data = LKA2data, FUN = sum)

#fill in zeros
LKA2data <- LKA2data %>%
  spread(ADM1_EN, CountValue, fill = 0) %>%
  gather(ADM1_EN, CountValue, -c(month,Year))
xtabs(~ ADM1_EN, data = LKA2data)

#export new data
write.csv(LKA2data, "LKA1_cleaned_2000.csv")
