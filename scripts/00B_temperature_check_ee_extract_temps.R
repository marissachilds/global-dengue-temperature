library(tidyverse)
library(magrittr)
library(sf)
library(rgee)

# download data matching the time period for each of those stations from earth engine 
ee_Initialize(user = "mlc1132") 

station_ll <- readRDS("./data/ghcn_station_data/study_station_inventory.rds")

download_loc <- "./data/ghcn_comparison_data/"
ee_era5_clim_loc <- "users/marissachilds/era5_monthly_climatology"
ee_worldclim_loc <- "users/lyberger/worldclim"

if (!dir.exists(download_loc)){
  dir.create(download_loc)
}

worldclim_clim <- purrr::map(1:12, 
                             function(m){
                               ee$Image(paste0(ee_worldclim_loc, 
                                               "/", 
                                               month.name[m])) %>% 
                                 ee$Image$set("month", m)
                             }) %>% ee$List()

era5_clim <- purrr::map(1:12, 
                        function(m){
                          ee$Image(paste0(ee_era5_clim_loc, 
                                          "/era5_climatology_1970-2000_", 
                                          formatC(m, width=2, flag="0"))) %>% 
                            ee$Image$set("month", m)
                        }) %>% ee$List()

era5 <- ee$ImageCollection("ECMWF/ERA5/DAILY")
era5_proj <- era5$first()$projection()
era5_scale <- era5_proj$nominalScale()$getInfo()

# extract both raw and debiased era5 data over the relevant time period
years_months <- expand_grid(y = (era5 %>% 
                                   ee$ImageCollection$filterDate(paste0(station_ll$start %>% min, "-01-01"), 
                                                                 paste0(station_ll$end %>% max %>% add(1), "-01-01")) %>% 
                                   ee$ImageCollection$aggregate_array("year"))$getInfo() %>% unique, 
                            m = 1:12) %>% 
  split(seq(nrow(.))) %>%
  unname %>% 
  purrr::map(function(x){x[1,] %>% as.numeric}) %>%
  ee$List()

temp_ims <- years_months$map(ee_utils_pyfunc(function(y_m){
  y = ee$List(y_m)$get(0)
  m = ee$List(y_m)$get(1)
  era5$filter(ee$Filter$eq("month", m)) %>%
    ee$ImageCollection$filter(ee$Filter$eq("year", y)) %>%
    ee$ImageCollection$map(function(day_im){
      day_im$select("mean_2m_air_temperature") %>% 
        ee$Image$rename("temp_raw") %>% 
        ee$Image$addBands(day_im$select("mean_2m_air_temperature") %>% 
                            ee$Image$subtract(era5_clim$get(ee$Number(m)$subtract(1))) %>%
                            ee$Image$add(worldclim_clim$get(ee$Number(m)$subtract(1))) %>% 
                            ee$Image$rename("temp_debias")) %>%
        ee$Image$copyProperties(day_im, list("day", "month", "year", "system:time_start")) %>% 
        return()
    }) %>% 
    return()
  })) %>% ee$ImageCollection$flatten()
  
temp_ims$size()$getInfo()
temp_ims$first()$propertyNames()$getInfo()


station_ee <- sf_as_ee(station_ll %>% select(id))
# Map$addLayer(station_ee)
# now extract temperature data for each station
station_daily = temp_ims$map(function(day_im){
  ee$Image(day_im)$reduceRegions(collection = station_ee, 
                       reducer = ee$Reducer$first()) %>% 
    ee$FeatureCollection$map(function(f){
            f$set("date", day_im$get("system:index")) %>%  # set the date of the feature
              return}) %>% 
    return
}) %>% ee$FeatureCollection$flatten()

# ee$String(temp_ims$first()$get("system:id"))$replace(".*/", "")$getInfo()
# temp_ims$first()$get("system:index")$getInfo()

ee_print(ee$Feature(station_daily$first()))
ee$Feature(station_daily$first())$propertyNames()$getInfo()
export_task <- ee_table_to_drive(
  collection = station_daily,
  description = "ghcn_station_temps",
  fileFormat = "CSV",
  selectors = list("id", "date", "temp_raw", "temp_debias")
)
export_task$start()
export_task$status()$state

ee_drive_to_local(export_task, 
                  paste0(download_loc, "ghcn_station_temps", ".csv"))          
