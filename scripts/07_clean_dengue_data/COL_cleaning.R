#dengue in Colombia COL
library(sf)
library(plyr)
library(dplyr)
library(tidyr)
library(stringi)

#Colombia
setwd("~/Documents/projects/cases per degree/")
setwd("col-administrative-divisions-shapefiles")
COLmunic <- st_read("col_admbnda_adm2_mgn_20200416.shp")
#COLmunic$ADM2_PCODE = gsub("0", "O", COLmunic$ADM2_PCODE)  #many use 0 instead of O
COLmunic$ADM2_PCODE = as.factor(COLmunic$ADM2_PCODE)

setwd("../Colombia")
COLdata <- ldply(list.files(), read.csv, header=TRUE) #case data
#COLdata <- read.csv("Datos_210_2020.csv")
COLdata = COLdata[which(COLdata$COD_MUN_O != 0 & COLdata$COD_MUN_O != 999 & COLdata$COD_PAIS_O == 170),]# COD_MUN_O!= 0 unknown department
#"C0" or "C00" + COD_DPTO_O + "0" or "00" + COD_MUN_O = ADM2_PCODE
COLdata$ADM2_PCODE = as.factor(paste("CO",sprintf("%02d",COLdata$COD_DPTO_O), sprintf("%03d",COLdata$COD_MUN_O), sep=""))
#Belén de Bajirá is involved in a territorial dispute between Antioquia and Chocó C027086 -> C005480 Mutata 
COLdata$ADM2_PCODE[which(COLdata$ADM2_PCODE=="CO27086")] <- "CO05480"
COLdata$ADM2_PCODE = droplevels(COLdata)$ADM2_PCODE
setdiff(levels(COLdata$ADM2_PCODE),levels(COLmunic$ADM2_PCODE))

#assign rows to month and year
COLdata_sub = COLdata[,c("INI_SIN","SEMANA","ANO","ADM2_PCODE")]
#INI_SIN is either in format MO/DAY/YEAR or MO/DAY/YR or DAY/MO/YEAR or YEAR-MO-DAY
# remove cases before the start of 2007
COLdata_sub = COLdata_sub[which(COLdata_sub$ANO >= 2007),]
# there are 122 cases without an INI_SIN date nrow(COLdata_sub[which(COLdata_sub$INI_SIN ==""),])
#2007-MO-DA
#2008-MO-DA
#2009-MO-DA
#2010 mix of DA/MO/YR and DA/MO/YEAR 
#2011-MO-DA
#2012-MO-DA
#2013-MO-DA
#2014-MO-DA
#2015-MO-DA
#2016-MO-DA
#2017-MO-DA
#2018 is DA/MO/YR 
#2019 is DA/MO/YEAR 
#2020 is DA/MO/YEAR 

# for 2010, 2018,19,20 make into regular date format
#stri_pad_left(COLdata_sub[which(COLdata_sub$ANO == 2018 |COLdata_sub$ANO == 2019 | COLdata_sub$ANO == 2020 | COLdata_sub$ANO == 2010),"INI_SIN"], 8, pad = "0")
DAMO = COLdata_sub[which(COLdata_sub$ANO == 2018 | COLdata_sub$ANO == 2019 | COLdata_sub$ANO == 2020 | COLdata_sub$ANO == 2010),]
DAMO$INI_SIN = format(as.Date(DAMO$INI_SIN,format="%d/%m/%Y"), "20%y-%m-%d")
COLdata_sub = COLdata_sub[which(COLdata_sub$ANO != 2018 & COLdata_sub$ANO != 2019 & COLdata_sub$ANO != 2020 & COLdata_sub$ANO != 2010),]
COLdata_sub = rbind(COLdata_sub,DAMO)
#add columns for Year and month, fill by splitting up digits 1:4 and 6:7
COLdata_sub$INIYear = as.numeric(substr(COLdata_sub$INI_SIN,1,4))
COLdata_sub$Month = as.numeric(substr(COLdata_sub$INI_SIN,6,7))

#count rows by adm/month/year
#COLdata_ag = aggregate(cbind(count = VALUE) ~ Month + INIYear + ADM2_PCODE, 
#          data = COLdata_sub, 
#          FUN = function(x){NROW(x)})
COLdata_monthly = COLdata_sub %>% dplyr::count(INIYear, Month, ADM2_PCODE)
COLdata_monthly = COLdata_monthly[complete.cases(COLdata_monthly), ]

#subset to 2006 to 2020
COLdata_monthly = COLdata_monthly[which(COLdata_monthly$INIYear >= 2007 & COLdata_monthly$INIYear < 2021),]
#fill in zeros
COLmonthly <- COLdata_monthly %>%
  spread(ADM2_PCODE, n, fill = 0) %>%
  gather(ADM2_PCODE, n, -c(Month,INIYear)) %>%
  mutate(ADM2_PCODE = tolower(ADM2_PCODE)) 

COLmonthly= plyr::rename(COLmonthly, c("INIYear" = "Year", "Month" = "month", "n"="CountValue"))

setwd("..")
write.csv(COLmonthly, "COL_cleaned.csv")
