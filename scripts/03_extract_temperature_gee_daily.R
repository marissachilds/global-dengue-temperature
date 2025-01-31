# potentially redo chunking of shapefiles to match the way that the cmip6 extractions do it
library(magrittr)
library(tidyr)
library(reticulate)
library(rgee)
library(dplyr)

source("./scripts/00_setup.R")
ee_Initialize(user = gee_user) 

download_loc <- "./data/from_gee/"
ee_era5_clim_loc <- "users/marissachilds/era5_monthly_climatology"
ee_worldclim_loc <- "users/lyberger/worldclim"

# check if the download location exists, otherwise, make it 
if (!dir.exists(download_loc)){
  dir.create(download_loc)
}

# make lists of the worldclim and ERA5 monthly climatologies
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

country_tasks = read.csv("./ref_tables/country_tasks.csv") %>% 
  # PHL is the only one in a different location on earth engine
  mutate(ee_loc = ifelse(country_shapefile == "PHL", "users/marissachilds/", "users/lyberger/dengue/")) 

# reformat dates from month-day-year to year-month-day
# also add 1 day to end date to avoid excluding the last day
country_tasks %<>% 
  mutate(start_date = as.Date(start_date, format = "%m/%d/%y") %>% 
           format("%Y-%m-%d"), 
         # end_date_inclusive = end_date, 
         end_date = as.Date(end_date, format = "%m/%d/%y") %>% 
           add(as.difftime(1, units = "days")) %>%
           format("%Y-%m-%d"))

# COL and PHL have trouble running, has out of memory errors, so split it into 4 time periods 
country_tasks %<>% 
  mutate(count = case_when(country_shapefile == "COL" ~ 4, 
                           country_shapefile == "PHL" ~ 3, 
                           T ~ 1)) %>% 
  uncount(count, .id = "id") %>% 
  mutate(start_date = case_when(id == 2 & country_shapefile == "COL" ~ "2013-01-01",
                                id == 3 & country_shapefile == "COL" ~ "2015-01-01",
                                id == 4 & country_shapefile == "COL" ~ "2017-01-01", 
                                id == 2 & country_shapefile == "PHL" ~ "1999-01-01",
                                id == 3 & country_shapefile == "PHL" ~ "2005-01-01",
                                T ~ start_date), 
         end_date = case_when(id == 1 & country_shapefile == "COL" ~ "2013-01-01", 
                              id == 2 & country_shapefile == "COL" ~ "2015-01-01",
                              id == 3 & country_shapefile == "COL" ~ "2017-01-01", 
                              id == 1 & country_shapefile == "PHL" ~ "1999-01-01",
                              id == 2 & country_shapefile == "PHL" ~ "2005-01-01",
                              T ~ end_date)) %>% 
  select(-id)
  
# set up ERA5 and WorldPop data which are stored on earth engine
era5 <- ee$ImageCollection("ECMWF/ERA5/DAILY")
era5_proj <- era5$first()$projection()
era5_scale <- era5_proj$nominalScale()$getInfo()

pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()

# calculating era5 monthly averages on earth engine: 
# observed daily era5 values - era5 month climatology + worldclim month climatology. 
# to get the nonlinear temp functions right, then we need to square, cube (and to be safe, ^4, ^5, ^6) 
# temperature, then average each of those to the month then do the spatial averaging.
country_exports <- country_tasks %>% 
  rename(mid_year = mid_date) %>%
  purrr::pmap(function(country_shapefile, 
                       identifier_col, 
                       start_date, 
                       end_date, 
                       split_shape,
                       mid_year, 
                       ee_loc){
    print(paste0("starting extractions for shapefile: ", country_shapefile))
    
    shape <- ee$FeatureCollection(paste0(ee_loc, 
                                         ifelse(country_shapefile == "PHL", 
                                                "PHL_palawan_simp", 
                                                country_shapefile)))
    
    export_properties <- list(identifier_col,"year", "month", "mean", "property") 
    
    # based on the time range, identify the set of years and months we'll map over
    year_month_list = seq.Date(as.Date(start_date), as.Date(end_date), by = "month") %>% 
      head(-1) %>% # drop last excluded end date
      as.character() %>% 
      substr(1, 7) %>% # keep only the year and month info from the date
      as.list() %>% 
      purrr::map(function(x){
        strsplit(x, "-") %>% 
          unlist %>% 
          as.numeric() %>% 
          return}) %>% 
      ee$List()
    
    
    # if the shapefile is large, we'll split it in half
    if(split_shape){
      tasks <- data.frame(i = 1:2,  
                          size = cut(1:shape$size()$getInfo(), 2) %>% table %>% as.numeric,
                          order = 1:0 %>% as.logical())
    }else{
      tasks <- data.frame(i = 1, 
                          size = shape$size()$getInfo(), 
                          order = 0 %>% as.logical())
    }
    
    # again, COL has trouble running so split it unevenly
    if(country_shapefile == "COL"){
      tasks$size = c(650, 472)
    }
    print(tasks)
    
    tasks %>% 
      purrr::pmap(function(i, size, order){
        # if necessary, subset to part of the shapefile
        muni_col <- shape$limit(maximum = size, 
                                opt_property = identifier_col, 
                                opt_ascending = order)
        
        # print out which part of the shapefile is being operated on
        print(paste0("units with codes/names from ", 
                     muni_col$aggregate_min(identifier_col)$getInfo(), 
                     " to ", 
                     muni_col$aggregate_max(identifier_col)$getInfo()))
        
        pop_density <- pop_orig %>% 
          ee$ImageCollection$filter(ee$Filter$calendarRange(mid_year, mid_year, "year")) %>%
          ee$ImageCollection$reduce(ee$Reducer$mean()) %>% 
          ee$Image$divide(ee$Image$pixelArea())
        
        # go by year and month and calculate temporal averages
        unit_avgs = year_month_list$map(ee_utils_pyfunc(function(y_m){
          y = ee$List(y_m)$get(0)
          m = ee$List(y_m)$get(1)
          
          # loop over the daily images and debias 
          month_avg <- era5$filter(ee$Filter$eq("month", m)) %>%
            ee$ImageCollection$filter(ee$Filter$eq("year", y)) %>%
            ee$ImageCollection$map(function(day_im){
              
              # debias with the right offset depending on the month
              debias_day = day_im$select("mean_2m_air_temperature") %>% 
                ee$Image$subtract(era5_clim$get(ee$Number(m)$subtract(1))) %>%
                ee$Image$add(worldclim_clim$get(ee$Number(m)$subtract(1))) %>%
                # calculate ^2, ^3, ^4, ^5, ^6 powers
                ee$Image$pow(ee$Image$constant(as.list(1:6))) %>% 
                ee$Image$rename(paste0("mean_2m_air_temperare_degree", 1:6)) %>% 
                # also add DTR and precip average bands
                ee$Image$addBands(day_im$select("maximum_2m_air_temperature") %>% 
                                    ee$Image$subtract(day_im$select("minimum_2m_air_temperature")) %>%
                                    ee$Image$rename("DTR_2m_air_temperature")) %>%
                ee$Image$addBands(day_im$select(list("total_precipitation"))) %>%
                ee$Image$copyProperties(day_im, list("day", "month", "year", "system:time_start")) %>% 
                return()
            }) %>% 
            # then temporal average for each band
            ee$ImageCollection$mean()
          
          # spatial averaging
          # for each band, calculate population-weighted average 
          unit_month_avgs = month_avg$bandNames()$map(ee_utils_pyfunc(function(band_name){
            month_avg$select(list(band_name))$addBands(pop_density) %>% 
              ee$Image$reduceRegions(collection = muni_col,
                                     reducer = ee$Reducer$mean()$splitWeights(),
                                     crs = era5_proj,
                                     scale = 1000) %>%
              ee$FeatureCollection$map(function(f){
                f$set("year", y, 
                      "month", m,
                      "property", ee$String(band_name)) %>%  # set the date of the feature
                  return})
          })) %>% ee$FeatureCollection() %>%
            ee$FeatureCollection$flatten() %>% 
            return
          
        })) %>%
          ee$FeatureCollection() %>%
          ee$FeatureCollection$flatten()
        
        # export the temperature feature collection to drive, 
        # only saving the relevant properties to speed things up 
        export_temp_task <- ee_table_to_drive(
          collection = unit_avgs,
          description = paste0(country_shapefile,
                               "_temp_precip",
                               "_",i,
                               "_", gsub("-", "", start_date), 
                               "_", gsub("-", "", end_date)),
          fileFormat = "CSV",
          selectors = export_properties
        )
        
        # calculate populations in each administrative unit 
        pop <- pop_orig %>% 
          ee$ImageCollection$filter(ee$Filter$calendarRange(mid_year, mid_year, "year")) %>%
          ee$ImageCollection$reduce(ee$Reducer$mean())
        
        muni_pop <- pop %>% ee$Image$reduceRegions(collection = muni_col,
                                                   reducer = ee$Reducer$sum(),
                                                   crs = pop_proj,
                                                   scale = pop_scale)
        # export population data to drive 
        export_pop_task <- ee_table_to_drive(
          collection = muni_pop,
          description = paste0(country_shapefile,
                               "_pop",
                               "_",i,
                               "_y", mid_year),
          fileFormat = "CSV",
          selectors = list(identifier_col, "sum")
        )
        
        # start export tasks and return the export task information 
        export_temp_task$start()
        export_pop_task$start()
        return(list(export_temp_task,
                    export_pop_task))
        
      }) -> muni_temps_exports 
    return(muni_temps_exports)
    
  })

# check the status
# each individual task takes ~ 1-10 minutes, except CHN, BRA, MEX which take ~15-30 min 
# but total may take longer if the tasks run sequentially rather than simultaneously
retry::wait_until(
  expr = all(purrr::map_chr(unlist(country_exports), function(x) x$status()$state) == "COMPLETED"), 
  interval = 120
)

# once all are completed, download to local
unlist(country_exports) %>% 
  purrr::map(function(x){
    temp = x$status()
    name =  temp$description
    if(x$status()$state == "COMPLETED"){
      ee_drive_to_local(x, paste0(download_loc, name, ".csv"))          
    } else{
      print(paste0(name, " has status ", temp$state))
    }
  })
