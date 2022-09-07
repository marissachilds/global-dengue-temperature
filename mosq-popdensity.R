library(magrittr)
library(tidyr)
library(reticulate)
library(rgee)
library(ggplot2)
library(dplyr)
library(stars)

shapefile_ee_loc <- "users/lyberger/dengue/" # "users/marissac/" 
download_loc <- "./data/from_gee/"
pct_aegypti<-raster("pct_aegypti.tif")
country_tasks = read.csv("country_tasks.csv")

ee_Initialize(gcs = TRUE)
ee_utils_sak_copy(sakfile = "gee_key.json",
                  users="malljes13")
ee_utils_sak_validate()


pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()

pct_aegypti_ee<-ee$Image("users/malljes13/pct_aegypti")
aeg_proj <- pct_aegypti_ee$projection()

# reformat dates from month-day-year to year-month-day
# also add 1 day to end date to avoid excluding the last day
country_tasks %<>% 
  mutate(start_date = as.Date(start_date, format = "%m/%d/%y") %>% 
           format("%Y-%m-%d"), 
         end_date = as.Date(end_date, format = "%m/%d/%y") %>% 
           add(as.difftime(1, units = "days")) %>%
           format("%Y-%m-%d"))


country_exports <- country_tasks %>% 
   dplyr::filter(!country_shapefile %in% c("TWN", "LKA", "PHL")) %>%
  # magrittr::extract(c(23), ) %>%
  # mutate(country_shapefile = ifelse(country_shapefile == "PHL1", "PHL", country_shapefile)) %>%
  rename(mid_year = mid_date) %>%
  purrr::pmap(function(country_shapefile, 
                       identifier_col, 
                       start_date, 
                       end_date, 
                       split_shape,
                       mid_year){
    print(paste0("starting extractions for shapefile: ", country_shapefile))
    
    shape <- ee$FeatureCollection(paste0(shapefile_ee_loc, country_shapefile))
    
    export_properties <- list(identifier_col,"year_month", "mean", "property") 
    
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
          ee$ImageCollection$reduce(ee$Reducer$mean()) 
      
        
        # pct_aegypti_ee<-ee$Image()raster_as_ee(pct_aegypti, bucket='dengue_perdegree',
        #                              assetId="/users/malljes13/pct_aegypti")    
            # for  the pct_aegypti band, calculate population-weighted average 
         pct_aegypti_coll <-  pct_aegypti_ee$bandNames()$map(ee_utils_pyfunc(function(band_name){
              pct_aegypti_ee$select(list(band_name))$addBands(pop_density) %>% 
                ee$Image$reduceRegions(collection = muni_col,
                                       reducer = ee$Reducer$mean()$splitWeights(),
                                       crs = aeg_proj,
                                       scale = 1000) %>%
           ee$FeatureCollection$map(function(f){
             f$set("year_month", pct_aegypti_ee$get("system:index"),
                   "property", ee$String(band_name)) %>%  # set the date of the feature
               return})
          })) %>%
          ee$FeatureCollection() %>%
          ee$FeatureCollection$flatten()
        
        export_pct_aeg_task <- ee_table_to_drive(
          collection = pct_aegypti_coll,
          description = paste0(country_shapefile,
                               "_pct_aeg",
                               "_",i),
          fileFormat = "CSV",
          selectors = export_properties
        )
        
        
        export_pct_aeg_task$start()
        #export_pop_task$start()
        return(list(export_pct_aeg_task))
        
      }) -> muni_aeg_exports 
    return(muni_aeg_exports)
})  
# 
# # check the status
# retry::wait_until(
#   expr = all(purrr::map_chr(unlist(country_exports), function(x) x$status()$state) == "COMPLETED"), 
#   interval = 120
# )
# beepr::beepr()
# 
# once all are completed, download to local

unlist(country_exports) %>%
  purrr::map(function(x){
    ee_drive_to_local(x, paste0(download_loc, x$status()$description, ".csv"))
  })

# # read all the population exports in 
# pop_df <- unlist(country_exports) %>% 
#   purrr::map_chr(function(x) x$status()$description) %>% 
#   grep(pattern = "pop", value = TRUE) %>% 
#   purrr::map_dfr(function(x){
#     read.csv(paste0(download_loc, x, ".csv")) %>% 
#       mutate(file = x) %>% 
#       return
#   }) 
# 
# pop_df %>% 
#   filter(is.na(sum))
# 
aeg_df <- unlist(country_exports) %>%
  purrr::map_chr(function(x) x$status()$description) %>%
  grep(pattern = "pct_aeg", value = TRUE) %>%
  purrr::map_dfr(function(x){
    read.csv(paste0(download_loc, x, ".csv")) %>%
      mutate(file = x) %>%
      return
  })
# 
# clim_df %>% 
#   filter(is.na(mean)) 
# 
# # open a test one
# test <- read.csv(paste0(download_loc, unlist(country_exports)[[2]]$status()$description, ".csv"))
# test %>% filter(is.na(mean)) %>% View