#DOM

#dengue in Dominican Republic DOM
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)
library(stringr)

#Dominican Republic
setwd("~/Documents/projects/cases per degree/")
setwd("dom_admbnda_one_20210629_shp")
DOMmunic <- st_read("dom_admbnda_adm2_one_20210629.shp")
DOMmunic$ADM2_ES = tolower(stri_trans_general(str = DOMmunic$ADM2_ES, id = "Latin-ASCII"))
#DOMmunic$ADM2_ES = as.factor(gsub("provincia ", "", DOMmunic$ADM2_ES))

setwd("../Dominican Republic/DOM_data")

DOMdata2007 = read.csv("DR_denguemalaria07wk.csv", header=FALSE,strip.white=TRUE)
# DOMdata7 = as.data.frame(t(as.matrix(DOMdata2007)))
# colnames(DOMdata7) = gsub(" ", "_", DOMdata7[1,])
# DOMdata7 = DOMdata7[-1, ] 
# DOMdata7long = gather(DOMdata7, ADM2_ES, CountValue, Distrito_Nacional:Santo_Domingo)
# DOMdata7long$Week = gsub(" ", "", DOMdata7long$Week)
# DOMdata7long$CountValue = str_trim(DOMdata7long$CountValue)
# DOMdata7long = DOMdata7long %>%  
#     mutate_at(c('Week', 'Year',"CountValue"), as.numeric)
# DOMdata7long$ADM2_ES = gsub("_", " ", DOMdata7long$ADM2_ES)
# DOMdata7long$ADM2_ES = tolower(stri_trans_general(str = DOMdata7long$ADM2_ES, id = "Latin-ASCII"))

DOMdata2008 <- read.csv("DR_denguemalaria08wk.csv", header=FALSE)
DOMdata2009 <- read.csv("DR_denguemalaria09wk.csv", header=FALSE)
DOMdata2010 <- read.csv("DR_denguemalaria10wk.csv", header=FALSE)
DOMdata2011 <- read.csv("DR_denguemalaria11wk.csv", header=FALSE)
DOMdata2012 <- read.csv("DR_denguemalaria12wk.csv", header=FALSE)
DOMdata2013 <- read.csv("DR_denguemalaria13wk.csv", header=FALSE)
DOMdata2014 <- read.csv("DR_denguemalaria14wk.csv", header=FALSE)
DOMdata2015 <- read.csv("DR_denguemalaria15wk.csv", header=FALSE)
DOMdata2016 <- read.csv("DR_denguemalaria16wk.csv", header=FALSE)
DOMdata2017 <- read.csv("DR_denguemalaria17wk.csv", header=FALSE)
DOMdata2018 <- read.csv("DR_denguemalaria18wk.csv", header=FALSE)
DOMdata2019 <- read.csv("DR_denguemalaria19wk.csv", header=FALSE)
DOMdata2020 <- read.csv("DR_denguemalaria20wk.csv", header=FALSE)

#for loop
yr = sprintf("%02d",seq(07,20))
datayr = paste("DOMdata20",yr,sep="")
DOMlist = list()
for (i in 1:12){
DOMdata = as.data.frame(t(as.matrix(eval(parse(text = datayr[i])))))
colnames(DOMdata) = gsub(" ", "_", DOMdata[1,])
DOMdata = DOMdata[-1, ] 
DOMdatalong = gather(DOMdata, ADM2_ES, CountValue, Distrito_Nacional:Santo_Domingo)
DOMdatalong$Week = gsub(" ", "", DOMdatalong$Week)
DOMdatalong$CountValue = str_trim(DOMdatalong$CountValue)
DOMdatalong = DOMdatalong %>%  
    mutate_at(c('Week', 'Year',"CountValue"), as.numeric)
DOMdatalong$ADM2_ES = gsub("_", " ", DOMdatalong$ADM2_ES)
DOMdatalong$ADM2_ES = tolower(stri_trans_general(str = DOMdatalong$ADM2_ES, id = "Latin-ASCII"))
DOMlist[[i]] <- DOMdatalong
}
DOM_all <- dplyr::bind_rows(DOMlist)
#remove NAs
DOM_all = DOM_all[complete.cases(DOM_all), ]

#any mismatches?
DOM_all$ADM2_ES[which(DOM_all$ADM2_ES == "bahoruco")] <- "baoruco"
DOM_all$ADM2_ES[which(DOM_all$ADM2_ES == "el seybo")] <- "el seibo"
DOM_all$ADM2_ES[which(DOM_all$ADM2_ES == "san juan de la maguana")] <- "san juan"
DOM_all$ADM2_ES[which(DOM_all$ADM2_ES == "montecristi")] <- "monte cristi"
DOM_all$ADM2_ES[which(DOM_all$ADM2_ES == "salcedo")] <- "hermanas mirabal"
DOM_all$ADM2_ES[which(DOM_all$ADM2_ES == "santiago ")] <- "santiago"
DOM_all$ADM2_ES = paste("provincia",DOM_all$ADM2_ES)
DOM_all$ADM2_ES[which(DOM_all$ADM2_ES == "provincia distrito nacional")] <- "distrito nacional"


setdiff(as.factor(levels(DOM_all$ADM2_ES)),levels(as.factor(DOMmunic$ADM2_ES)))


#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", DOM_all$Week)
DOM_all$startdate = ISOweek2date(paste(DOM_all$Year,"-",week,"-",1, sep="")) 
DOM_all$enddate = DOM_all$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- DOM_all %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

DOMmonthly_long = tbl_split_long %>% 
  group_by(ADM2_ES, Week, Year = year(.start), month = month(.start)) 
DOMmonthly_long$duration = difftime(DOMmonthly_long$.end, DOMmonthly_long$.start, units = "days")
DOMmonthly_long$duration = as.numeric(DOMmonthly_long$duration, units = "days")
DOMmonthly_long$CountValue = as.numeric(DOMmonthly_long$CountValue)
#multiply CountValue by fraction days in each month
DOMmonthly_long$CountValue = DOMmonthly_long$CountValue*(DOMmonthly_long$duration/7)

#aggregate CountValue by adm/year/month
DOMmonthly = aggregate(CountValue ~ ADM2_ES + month + Year, data = DOMmonthly_long, FUN = sum)

#fill in zeros
DOMmonthly <- DOMmonthly %>%
  spread(ADM2_ES, CountValue, fill = 0) %>%
  gather(ADM2_ES, CountValue, -c(month,Year)) %>%
  mutate(ADM2_ES = tolower(ADM2_ES)) 

#export new data
setwd("..")
#write.csv(DOMmonthly, "DOM_cleaned_2014.csv")
