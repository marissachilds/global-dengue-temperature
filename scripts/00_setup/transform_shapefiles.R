library(magrittr)
library(tidyverse)
library(sf)
library(rworldmap)

gee_tasks <- read.csv("../ref_tables/country_tasks.csv") %>% 
  filter(country_shapefile %in% c("LKA1", "CHN") == "FALSE")

# read in shapefiles and transform crs so that 180 (pacific ocean) is in the middle, not 360
target_crs <- st_crs("+proj=longlat +datum=WGS84 +no_defs +lon_0=180")

data(countriesCoarseLessIslands) 
countries <- st_as_sf(countriesCoarseLessIslands)

continents <- st_read("./data/World_Continents")

continents_plot <- continents %>% 
  filter(CONTINENT %in% c("Africa", "Europe", "Antarctica") == FALSE) %>% 
  st_break_antimeridian(lon_0 = 180) %>% 
  st_transform(target_crs)
countries_plot <- countries %>% 
  filter(SOV_A3 %in% c(unique(gee_tasks$country_shapefile))) %>% 
  st_break_antimeridian(lon_0 = 180) %>% 
  st_transform(target_crs)

all_shapes <- st_read("./data/shapefiles/all_shapes") %>% 
  filter(country %in% c("CHN") == FALSE) %>% 
  # set crs to 4326
  st_set_crs(4326) %>% 
  st_break_antimeridian(lon_0 = 180) %>% 
  st_transform(target_crs) %>% 
  st_simplify(dTolerance = 0.05) 

list(continents = continents_plot, 
     countries = countries_plot, 
     admin = all_shapes) %>% 
  saveRDS("./data/shapefiles_plotting.rds", 
          compress = T)
