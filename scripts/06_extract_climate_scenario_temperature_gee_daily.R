library(tidyr)
library(dplyr)
library(magrittr)
library(reticulate)
library(rgee)

source("00_setup.R")
ee_Initialize(user = gee_user)

proj_start =  "1994-07-01" # we want 1995, but start 6 months before so we can get lags 
proj_end = "2015-01-01" 

# how many time chunks to cut up into 
n_date_breaks = 2

# how many spatial chunks to break the space into (i think default to smaller, unless the pieces don't run)
n_spat_xl = 24 # PHL, do it in 16 chunks + Sulu separately, since that seemed to make things fail before
n_spat_l = 8 # COL IDN MEX BRA 
n_spat_m = 4 # THA VEN VNM PER LKA NIC 
n_spat_s = 2 # MYS PAN SLV BOL DOM CRI HND
n_spat_xs = 1 # TWN KHM LAO 

scenarios_range = c(1:74) # 1 - 74
country_set = c() # if you want to limit to a set of countries. use c() to run all countries

# for countries where some sub-country units are especially slow, run the slow ones separately and the rest as larger chunks
separate_units = list(PHL = c("Sulu"))

download_loc <- "./data/from_gee_cmip6"
ee_era5_clim_loc <- "users/marissachilds/era5_monthly_climatology"
ee_worldclim_loc <- "users/lyberger/worldclim"
ee_cmip6_dT_loc <- "users/marissachilds/cmip6_scenarios_dT_monthly"

country_tasks = read.csv("./data/country_tasks.csv") %>% 
  filter(country_shapefile != "CHN" & 
           country_shapefile != "LKA1") %>% 
  # PHL is the only one in my assets
  mutate(ee_loc = ifelse(country_shapefile == "PHL", "users/marissachilds/", "users/lyberger/dengue/")) %>% 
  # drop countries that are in the data set multiple times, keeping the instance with the later midyear
  filter(mid_date == max(mid_date), 
         .by = country_shapefile) 

if(length(country_set)>0){
  country_tasks %<>% filter(country_shapefile %in% country_set)
}

scenario_list = read.csv("./data/GCM_variant_scenarios_to_include.csv") %>% 
  filter(experiment_id != "historical") %>% 
  mutate(asset_name = paste0(source_id, "_", member_id, "_", experiment_id)) %>% 
  select(source_id, member_id, experiment_id, asset_name) %>% 
  rbind(data.frame(source_id = "era5", 
                   member_id = "0", 
                   experiment_id = "current", 
                   asset_name = NA)) %>% 
  filter(source_id != "MCM-UA-1-0")
# MCM-UA-1-0 has weird stuff spatially, so lets not run for now

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
if(n_date_breaks == 1){
  proj_date_seq <- c(as.Date(proj_start, format = "%Y-%m-%d"), 
                     as.Date(proj_end, format = "%Y-%m-%d"))
} else{
  proj_date_seq <- seq.Date(as.Date(proj_start, format = "%Y-%m-%d"),
                            as.Date(proj_end, format = "%Y-%m-%d"),
                            by = "1 months") %>% 
    {.[c(1, cut(1:length(.), n_date_breaks) %>% table %>% as.numeric %>% cumsum)]}
}

country_exports <- country_tasks %>%
  rename(mid_year = mid_date) %>% 
  # add the number of splits for each country  
  mutate(n_split = case_when(country_shapefile %in% c("PHL") ~ n_spat_xl, 
                             country_shapefile %in% c("COL", "IDN", "BRA", "MEX") ~ n_spat_l, 
                             country_shapefile %in% c("PER", "LKA", "VNM", 
                                                      "NIC", "THA", "VEN") ~ n_spat_m, 
                             country_shapefile %in% c("TWN", "KHM", "LAO", "HND") ~ n_spat_xs,
                             T ~ n_spat_s)) %>% 
  # replace start and end dates with intervals set above
  select(-c(start_date, end_date)) %>% 
  cross_join(data.frame(start_date = proj_date_seq) %>% 
               mutate(end_date = lead(start_date, 1)) %>% 
               filter(!is.na(end_date))) %>% 
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
    print(paste0("starting extractions for ", country_shapefile,
                 " for ", asset_name, 
                 ", from ", format(start_date, "%Y-%m-%d"), 
                 " to ", format(end_date, "%Y-%m-%d")))
    
    shape <- ee$FeatureCollection(paste0(ee_loc, 
                                         ifelse(country_shapefile == "PHL", 
                                                "PHL_palawan_simp", 
                                                country_shapefile))) 
    
    shape <- shape$sort(prop = identifier_col)
    
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
    
    # for some countries, there are a couple units that really slow things down or cause exports to fail
    # we can run them separately so the rest can be in larger chunks
    if(country_shapefile %in% names(separate_units)){
      # make a filter to grab the separate units
      unit_filter = ee$Filter$inList(identifier_col,
                                     as.list(separate_units[[country_shapefile]]))
      # how many remain without those units? 
      shape_sub2 <- shape$filter(unit_filter$Not())
      n_feat <- shape_sub2$size()$getInfo()
      
      shape = shape$filter(unit_filter)$merge(shape_sub2)
      
      tasks_sub1 <- data.frame(i = separate_units[[country_shapefile]], 
                               size = rep(1, length(separate_units[[country_shapefile]]))) 
      tasks_sub2 <- data.frame(i = 1:n_split, 
                               size = if(n_split > 1){cut(1:n_feat, n_split) %>% table %>% as.numeric} else{n_feat}) 
      
      
      tasks <- rbind(tasks_sub1, 
            tasks_sub2) %>% 
        mutate(n_prev = cumsum(lag(size, n = 1, default = 0)))
      
    } else{
      n_feat <- shape$size()$getInfo()
      tasks <- data.frame(i = 1:n_split, 
                          size = if(n_split > 1){cut(1:n_feat, n_split) %>% table %>% as.numeric} else{n_feat}) %>% 
        mutate(n_prev = cumsum(lag(size, n = 1, default = 0)))
    }

    tasks %>% 
      purrr::pmap(function(i, size, n_prev){
        # subset shapefile
        muni_col <- ee$FeatureCollection(shape$toList(size, n_prev))
        
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
                ee$Image$add(scenario_dT_im$select(ee$Number(m)$subtract(1))) %>% 
                # calculate ^2, ^3, ^4, ^5
                ee$Image$pow(ee$Image$constant(as.list(1:5))) %>% 
                ee$Image$rename(paste0("mean_2m_air_temperature_degree", 1:5)) %>% 
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
        
        export_temp_task <- ee_table_to_drive(
          collection = unit_avgs,
          folder = paste0("gee_temps_", asset_name),
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

