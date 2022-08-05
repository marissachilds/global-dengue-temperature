library(readr)
library(dplyr)
library(sf)

#shapefile downloaded from here: https://data.humdata.org/dataset/cod-ab-cri/resource/bfa7a7dd-0515-405d-a515-20853c27a9b2
#modified two typos in place names
cr_map <- st_read(".", "cri_admbnda_adm3_2021_WGS_84")

#manually created to map cantons to socioeconomic regions based on this: https://second.wiki/wiki/regiones_socioeconc3b3micas_de_costa_rica#Las_regiones_socioecon%C3%B3micas_de_Costa_Rica
to_canton <- read_csv("canton_converter.csv", locale = readr::locale(encoding = "latin1"))

setdiff(to_canton$ADM2_ES, cr_map$ADM2_ES)
  
#add a socioeconomic region column to df
cr_map <- left_join(cr_map,to_canton, by="ADM2_ES")

#handle cantons that are subdivided 
cr_map$socioecon_region<-ifelse(cr_map$ADM3_ES=="Sarapiquí", 
                            "H. Norte",
                            cr_map$socioecon_region)

cr_map$socioecon_region<- ifelse(cr_map$ADM3_ES=="Peñas Blancas", 
                             "H. Norte",
                             cr_map$socioecon_region)

#note: it is unclear which region the districts Llanuras del Gaspar and Cureña should go to
cr_map$socioecon_region<- ifelse(cr_map$ADM3_ES=="Horquetas", 
                             "H. Atlantica",
                             cr_map$socioecon_region)

cr_map$socioecon_region<- ifelse(cr_map$ADM3_ES=="Rio Cuarto", 
                                 "H. Norte",
                                 cr_map$socioecon_region)

#spatial join
sf::sf_use_s2(FALSE)

cr_map <- cr_map %>%
            group_by(socioecon_region) %>%
            summarise()

cr_map$geometry <- cr_map$geometry %>%
  s2::s2_rebuild() %>%
  sf::st_as_sfc()

plot(cr_map)

#save a shapefile
st_write(cr_map, "costa_rica.shp", delete_layer=TRUE)
