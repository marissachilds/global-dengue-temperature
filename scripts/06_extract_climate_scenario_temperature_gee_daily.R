# COL 2 failed, and still need to do PHL once it uploads
# why is COL failing when BRA succeeds despite 
# what happens for dates outside the era5 range? --> just doesn't return anything
library(magrittr)
library(tidyr)
library(reticulate)
library(rgee)
# library(ggplot2)
library(dplyr)

proj_start =  "2013-01-01" # "1995-01-01"
# actually need to adjust to be a few (lets just do 6) months before so we can get lags 
proj_end = "2020-01-01" 

scenarios_range = c(77)

source("00_setup.R")
ee_Initialize(user = gee_user)
download_loc <- "./data/from_gee_cmip6"
ee_era5_clim_loc <- "users/marissachilds/era5_monthly_climatology"
ee_worldclim_loc <- "users/lyberger/worldclim"
ee_cmip6_dT_loc <- "users/marissachilds/cmip6_scenarios_dT_monthly"

scenario_list = read.csv("./data/GCM_variant_scenarios_to_include.csv") %>% 
  filter(experiment_id != "historical") %>% 
  mutate(asset_name = paste0(source_id, "_", member_id, "_", experiment_id)) %>% 
  select(source_id, member_id, experiment_id, asset_name) %>% 
  rbind(data.frame(source_id = "era5", 
                   member_id = "0", 
                   experiment_id = "current", 
                   asset_name = NA))

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

country_tasks = read.csv("country_tasks.csv") %>% View
  # PHL is the only one in my assets
  mutate(ee_loc = ifelse(country_shapefile == "PHL", "users/marissachilds/", "users/lyberger/dengue/"))

# replace start and end dates with 5 year intervals for projections
country_tasks %<>% 
  select(-c(start_date, end_date)) %>% 
  cross_join(data.frame(start_date = seq.Date(as.Date(proj_start, format = "%Y-%m-%d"),
                                              as.Date(proj_end, format = "%Y-%m-%d"),
                                              # by = "5 years"
                                              length.out = 2)) %>% # might want to redo this somehow so that the 6 months at the start work nicely....
               mutate(end_date = lead(start_date, 1)) %>% 
               filter(!is.na(end_date)))

# add the number of splits for each country 
country_tasks %<>% mutate(n_split = case_when(country_shapefile %in% c("PHL", "BRA", "COL") ~ 10, 
                                              T ~ 2))

# drop countries that are in the data set multiple times, keeping the instance with the later midyear
country_tasks %<>% 
  filter(mid_date == max(mid_date), 
         .by = country_shapefile)

# country_tasks$country_shapefile %>% n_distinct

era5 <- ee$ImageCollection("ECMWF/ERA5/DAILY")
era5_proj <- era5$first()$projection()
era5_scale <- era5_proj$nominalScale()$getInfo()

pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()

# calculating projected era5 monthly averages on earth engine: 
# observed daily era5 values - era5 month climatology + worldclim month climatology + dT from cmip6
# to get the nonlinear temp functions right, then we need to square, cube (and to be safe, ^4, ^5, ^6) 
# temperature, then average each of those to the month then do the spatial averaging.
country_exports <- country_tasks[1:2,] %>% 
  rename(mid_year = mid_date) %>% 
  cross_join(scenario_list %>% 
               magrittr::extract(scenarios_range,)) %>% 
  purrr::pmap(function(country_shapefile, 
                       identifier_col, 
                       start_date, 
                       end_date, 
                       split_shape,
                       mid_year, 
                       ee_loc, 
                       n_split, 
                       source_id, 
                       member_id, 
                       experiment_id, 
                       asset_name){
    print(paste0("starting extractions for shapefile: ", country_shapefile, " for ", asset_name))
    
    shape <- ee$FeatureCollection(paste0(ee_loc, 
                                         ifelse(country_shapefile == "PHL", 
                                                "PHL_palawan_simp", 
                                                country_shapefile))) 
    n_feat <- shape$size()$getInfo()
    
    shape <- shape$limit(n_feat, identifier_col)
    
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
    
    # get the cmip6 dT image 
    if(is.na(asset_name)){
      # this is a little dumb, but so it works later, make a band with zero for each month 
      scenario_dT_im = ee$Image$constant(0) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0)) %>% 
        ee$Image$addBands(ee$Image$constant(0))
    } else{ 
      scenario_dT_im = ee$Image(paste0(ee_cmip6_dT_loc, "/", asset_name))
    }
    
    tasks <- data.frame(i = 1:n_split, 
                        size = if(n_split > 1){cut(1:n_feat, n_split) %>% table %>% as.numeric} else{n_feat}) %>% 
      mutate(n_prev = cumsum(lag(size, n = 1, default = 0)))
    print(tasks)
    
    tasks %>% 
      purrr::pmap(function(i, size, n_prev){
        # if necessary, subset to part of the shapefile
        muni_col <- ee$FeatureCollection(shape$toList(size, n_prev))
        # Map$addLayer(muni_col)
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
              
              # print(ee$String("b")$cat(format(ee$Number(m)))$getInfo())
              # print(ee$Number(m)$getInfo())
              # debias with the right offset depending on the month
              debias_day = day_im$select("mean_2m_air_temperature") %>% 
                ee$Image$subtract(era5_clim$get(ee$Number(m)$subtract(1))) %>%
                ee$Image$add(worldclim_clim$get(ee$Number(m)$subtract(1))) %>%
                ee$Image$add(scenario_dT_im$select(ee$Number(m)$subtract(1))) %>% 
                # calculate ^2, ^3, ^4, ^5, ^6
                ee$Image$pow(ee$Image$constant(as.list(1:6))) %>% 
                ee$Image$rename(paste0("mean_2m_air_temperature_degree", 1:6)) %>% 
                # also add DTR and precip average bands
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
                                     scale = era5_scale) %>%
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
        
        export_temp_task <- ee_table_to_drive(
          collection = unit_avgs,
          description = paste0(country_shapefile,
                               "_temp_cmpi6_", 
                               source_id, 
                               "_", member_id, 
                               "_", experiment_id, 
                               "_", i,
                               "_", gsub("-", "", start_date),
                               "_", gsub("-", "", end_date + as.difftime(-1, units = "days"))),
          fileFormat = "CSV",
          selectors = export_properties
        )
        
        
        export_temp_task$start()
        return(list(export_temp_task))
        
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
beepr::beepr()

# once all are completed, download to local
unlist(country_exports) %>% 
  purrr::map(function(x){
    temp = x$status()
    name =  temp$description
    if(x$status()$state == "COMPLETED"){
      ee_drive_to_local(x, paste0(download_loc, "/", name, ".csv"))          
    } else{
      print(paste0(name, " has status ", temp$state))
    }
  })
