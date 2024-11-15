library(magrittr)
library(tidyr)
library(reticulate)
library(rgee)
library(dplyr)
library(purrr)

source("../scripts/00_setup.R")
ee_Initialize(user = gee_user) 

download_loc <- "./data/pop2015/"

pop_year = 2015 
# check if the download location exists, otherwise, make it 
if (!dir.exists(download_loc)){
  dir.create(download_loc)
}

country_tasks = read.csv("../ref_tables/country_tasks.csv") %>% 
  filter(country_shapefile != "CHN" & 
           country_shapefile != "LKA1") %>% 
  # PHL is the only one in my assets
  mutate(ee_loc = ifelse(country_shapefile == "PHL", "users/marissachilds/", "users/lyberger/dengue/")) %>% 
  select(country_shapefile, identifier_col, ee_loc) %>% 
  distinct

pop_orig <- ee$ImageCollection("WorldPop/GP/100m/pop")
pop_proj <- pop_orig$first()$projection()
pop_scale <- pop_proj$nominalScale()$getInfo()

separate_units = list(PHL = c("Sulu", "Palawan"))

all_exports3 <- country_tasks %>%
  # add the number of splits for each country  
  mutate(n_split = case_when(country_shapefile %in% c("PHL", "COL") ~ 8,
                             country_shapefile %in% c("IDN") ~ 4, 
                             country_shapefile %in% c("COL", "IDN", "BRA", 
                                                      "MEX", "THA") ~ 2, 
                             T ~ 1)) %>% 
  filter(country_shapefile %in% c("PHL", "COL")) %>%
  purrr::pmap(function(country_shapefile, 
                       identifier_col, 
                       ee_loc, 
                       n_split){
    print(paste0("starting extractions for shapefile: ", country_shapefile))
    
    shape <- ee$FeatureCollection(paste0(ee_loc, 
                                         ifelse(country_shapefile == "PHL", 
                                                "PHL_palawan_simp", 
                                                country_shapefile))) 
    
    shape <- shape$sort(prop = identifier_col)
    
    
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
      
      tasks_sub1 <- data.frame(i = sort(separate_units[[country_shapefile]]), 
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
        pop <- pop_orig %>% 
          ee$ImageCollection$filter(ee$Filter$calendarRange(pop_year, pop_year, "year")) %>%
          ee$ImageCollection$reduce(ee$Reducer$mean())
        
        muni_pop <- pop %>% ee$Image$reduceRegions(collection = muni_col,
                                                   reducer = ee$Reducer$sum(),
                                                   crs = pop_proj,
                                                   scale = pop_scale)
        
        export_pop_task <- ee_table_to_drive(
          collection = muni_pop,
          description = paste0(country_shapefile,
                               "_pop",
                               "_",i,
                               "_y", pop_year),
          fileFormat = "CSV",
          selectors = list(identifier_col, "sum")
        )
        
        export_pop_task$start()
        return(export_pop_task)
      }) %>% 
      return
  })

# when they've all finished, download the csvs to local
unlist(all_exports3) %>% 
  purrr::map(function(x){
    temp = x$status()
    name =  temp$description
    if(x$status()$state == "COMPLETED"){
      ee_drive_to_local(x, paste0(download_loc, name, ".csv"))          
    } else{
      print(paste0(name, " has status ", temp$state))
    }
  })

# then read them in and combine them into a single rds 
list.files(download_loc, full.names = T) %>% 
  purrr::map(function(x){
    read.csv(x) %>% 
      rename_with(function(x){"id"}, any_of(country_tasks$identifier_col)) %>% 
      mutate(country = gsub(".*/", "", x) %>% 
               gsub("_.*", "", .)) %>% 
      return
  }) %>% 
  list_rbind %>% 
  saveRDS(paste0("./data/", "all_pop2015.rds"))


