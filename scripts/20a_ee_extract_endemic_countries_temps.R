library(rgee)
library(tidyverse)
library(magrittr)

source("./scripts/00_utilities/setup.R")
ee_Initialize(user = gee_user) 

gbd_est <- readxl::read_excel("./data/Global Burden Disease dataset.xls")  %>% 
  filter(measure == "Incidence") %>% 
  filter(val > 0) %>% 
  mutate(gee_name = case_match(location, 
                               "American Samoa" ~ "American Samoa (US)", 
                               "Antigua and Barbuda" ~ "Antigua & Barbuda", 
                               "Cape Verde" ~ "Cabo Verde", 
                               "Central African Republic" ~ "Central African Rep",
                               "Congo" ~ "Congo, Rep of the",
                               "Democratic Republic of the Congo" ~ "Congo, Dem Rep of the",
                               "Federated States of Micronesia" ~ "Micronesia, Fed States of", 
                               "Marshall Islands" ~ "Marshall Is",
                               "Myanmar" ~ "Burma",
                               "Northern Mariana Islands" ~ "Northern Mariana Is (US)",
                               "Puerto Rico" ~ "Puerto Rico (US)",
                               "Saint Lucia" ~ "St Lucia",
                               "Saint Vincent and the Grenadines" ~ "St Vincent & the Grenadines", 
                               "Solomon Islands" ~ "Solomon Is", 
                               "Taiwan (Province of China)" ~ "Taiwan", 
                               "The Bahamas" ~ "Bahamas, The",
                               "The Gambia" ~ "Gambia, The",
                               "Trinidad and Tobago" ~ "Trinidad & Tobago", 
                               "Virgin Islands, U.S." ~ "US Virgin Is (US)", 
                               .default = location))


# country borders on gee 
(ee$FeatureCollection("USDOS/LSIB/2017") %>% 
    ee$FeatureCollection$filter(ee$Filter$inList('COUNTRY_NA', gbd_est$gee_name))) -> countries 

# climatologies to debias with 
ee_era5_clim_loc <- "users/marissachilds/era5_monthly_climatology"
ee_worldclim_loc <- "users/lyberger/worldclim"

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


# want to calculate the pop-weighted average temperature from 1995 - 2014 in each of these places
# to make things easier, we'll use ERA5 global, monthly 
# we still should debias with WorldClim as before, 
# so lets calculate temperature averages for each calendar month from 1995 - 2014, 
# debias the average, and then average over all calendar months to get year round 

# grab all era5 images from the relevant time period
era5_all <- ee$ImageCollection("ECMWF/ERA5/MONTHLY") %>% 
  ee$ImageCollection$filterDate("1995-01-01", "2015-01-01") %>% 
  ee$ImageCollection$select("mean_2m_air_temperature")

era5_monthly <- ee$List(1:12)$map(ee_utils_pyfunc(function(m){
  # grab everything in the relevant month
  era5_all$filter(ee$Filter$eq("month", m)) %>% 
    # average over all years, subtract era5 climatology, add worldclim climatology for relevant month
    ee$ImageCollection$mean() %>% 
    ee$Image$subtract(era5_clim$get(ee$Number(m)$subtract(1))) %>%
    ee$Image$add(worldclim_clim$get(ee$Number(m)$subtract(1))) %>% 
    return
})) %>% ee$ImageCollection$fromImages()

era5_annual <- era5_monthly$mean()

# now extract pop-weighted estimates for all countries 
pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_density <- pop_orig %>% 
  ee$ImageCollection$filter(ee$Filter$calendarRange(2015, 2015, "year")) %>%
  ee$ImageCollection$reduce(ee$Reducer$mean()) %>% 
  ee$Image$divide(ee$Image$pixelArea())

era5_proj <- era5_all$first()$projection()
era5_scale <- era5_proj$nominalScale()$getInfo()

era5_annual$addBands(pop_density) %>% 
  ee$Image$reduceRegions(collection = countries,
                         reducer = ee$Reducer$mean()$splitWeights(),
                         crs = era5_proj,
                         scale = 5000) -> country_temps # era5 scale ~27km is too big for some of the island locations

export_task <- ee_table_to_drive(
  collection = country_temps,
  description = "endemic_dengue_country_temperatures",
  fileFormat = "CSV",
  selectors = list("COUNTRY_NA", "mean")
)

export_task$start()
export_task$status()$state
ee_drive_to_local(export_task, paste0("./data/", export_task$status()$description, ".csv")) 
