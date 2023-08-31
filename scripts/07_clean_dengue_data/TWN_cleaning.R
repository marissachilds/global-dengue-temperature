#dengue in Taiwan TWN
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Taiwan
setwd("~/Documents/projects/cases per degree/")
setwd("Taiwanshape")
TWNmunic <- st_read("TWN_adm2.shp")

setwd("../Taiwan")
TWNdata <- read.csv("Taiwan data.csv",header=F)
#need to change from wide to long format
TWNdata2 <- as.data.frame(t(as.matrix(TWNdata)))
colnames(TWNdata2) <- TWNdata2[1,]
TWNdata2 <- TWNdata2[-1, ] 
TWNdata <- gather(TWNdata2, province, cases, Taipei:Taitung)
#Lienchiang = Lienkiang (Matsu Islands), Yunlin = Yulin
TWNdata$province[which(TWNdata$province == "Lienchiang")] <- "Lienkiang (Matsu Islands)"
TWNdata$province[which(TWNdata$province == "Yunlin")] <- "Yulin"
TWNdata$province = as.factor(TWNdata$province)
setdiff(levels(TWNdata$province),levels(as.factor(TWNmunic$NAME_2)))
TWNdata$Week = as.numeric(TWNdata$Week)
#aggregate weekly to monthly
library(lubridate)
library(ISOweek)
week = sprintf("W%02d", TWNdata$Week)
TWNdata$startdate = ISOweek2date(paste(TWNdata$Year,"-",week,"-",1, sep="")) 
TWNdata$enddate = TWNdata$startdate + days(7)

#how many days in each week fall into each month
#answered here https://stackoverflow.com/questions/58772863/how-to-split-event-intervals-by-month-with-dplyr
split_interval <- function(start, end, unit) {
  breaks <- seq(floor_date(start, unit), ceiling_date(end, unit), by = unit)
  timeline <- c(start, breaks[breaks > start & breaks < end], end)
  tibble(.start = head(timeline, -1), .end = tail(timeline, -1))
}

library(purrr)
tbl_split <- TWNdata %>% 
  mutate(periods = map2(startdate, enddate,
                        split_interval, unit = "months"))

tbl_split_long <- tbl_split %>% unnest(periods)

TWNmonthly_long = tbl_split_long %>% 
  group_by(province, Week, Year = year(.start), month = month(.start)) 
TWNmonthly_long$duration = difftime(TWNmonthly_long$.end, TWNmonthly_long$.start, units = "days")
TWNmonthly_long$duration = as.numeric(TWNmonthly_long$duration, units = "days")
TWNmonthly_long$cases = as.numeric(TWNmonthly_long$cases)
#multiply cases by fraction days in each month
TWNmonthly_long$CountValue = TWNmonthly_long$cases*(TWNmonthly_long$duration/7)

#aggregate cases by adm/year/month
TWNmonthly = aggregate(CountValue ~ province + month + Year, data = TWNmonthly_long, FUN = sum)

#fill in zeros
TWNmonthly <- TWNmonthly %>%
  spread(province, CountValue, fill = 0) %>%
  gather(province, CountValue, -c(month,Year)) %>%
  mutate(province = tolower(province)) 

TWNmonthly= plyr::rename(TWNmonthly, c("province" = "NAME_2"))

#export new data
setwd("..")
write.csv(TWNmonthly, "TWN_cleaned.csv")
