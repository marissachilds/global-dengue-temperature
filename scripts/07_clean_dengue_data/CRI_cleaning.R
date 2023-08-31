#dengue in Costa Rica CRI
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#weekly by region 2012-Sep 2020
#shapefile in different format
setwd("~/Documents/projects/cases per degree")
setwd("Costa Rica shp")
  CRImunic <- st_read("costa_rica.shp") #ADM1_ES
  levels(as.factor(CRImunic$sccn_rg))
  CRImunic$sccn_rg = as.factor(tolower(CRImunic$sccn_rg))

  #
setwd("..")

#aggregate regions to match shapefile
CRIdata = read.csv("CostaRica2012-2020.csv") 
CRIdata = CRIdata[-1, ]
CRIdata = gather(CRIdata, sccn_rg, CountValue, Pacifico.Central:H.Norte)
CRIdata$sccn_rg = tolower(CRIdata$sccn_rg)
CRIdata$sccn_rg[which(CRIdata$sccn_rg == "pacifico.central")] <- "central pacific"
CRIdata$sccn_rg[which(CRIdata$sccn_rg == "h.norte")] <- "h. norte"
CRIdata$sccn_rg[which(CRIdata$sccn_rg == "h..caribe")] <- "h. atlantica"
CRIdata$sccn_rg[which(CRIdata$sccn_rg == "central.sur"|CRIdata$sccn_rg == "central.norte"|CRIdata$sccn_rg == "occidente"|CRIdata$sccn_rg == "c.este")] <- "central"
CRIdata$sccn_rg = as.factor(CRIdata$sccn_rg)
setdiff(levels(CRIdata$sccn_rg),levels(CRImunic$sccn_rg))



#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
CRIdata$Semana = as.numeric(CRIdata$Semana)
CRIdata = na.omit(CRIdata)
week = sprintf("W%02d", CRIdata$Semana)
CRIdata$startdate = ISOweek2date(paste(CRIdata$Year,"-",week,"-",1, sep="")) 
CRIdata$enddate = CRIdata$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- CRIdata %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

CRIdata = tbl_split_long %>% 
  group_by(sccn_rg, Semana, Year = year(.start), month = month(.start)) 
CRIdata$duration = difftime(CRIdata$.end, CRIdata$.start, units = "days")
CRIdata$duration = as.numeric(CRIdata$duration, units = "days")
CRIdata$CountValue = as.numeric(CRIdata$CountValue)
#multiply CountValue by fraction days in each month
CRIdata$CountValue = CRIdata$CountValue*(CRIdata$duration/7)

#aggregate CountValue by adm/year/month
CRIdata = aggregate(CountValue ~ sccn_rg + month + Year, data = CRIdata, FUN = sum)

#fill in zeros
CRIdata <- CRIdata %>%
  spread(sccn_rg, CountValue, fill = 0) %>%
  gather(sccn_rg, CountValue, -c(month,Year)) %>%
  mutate(sccn_rg = tolower(sccn_rg)) 

#export new data
write.csv(CRIdata, "CRI_cleaned_2016.csv")
