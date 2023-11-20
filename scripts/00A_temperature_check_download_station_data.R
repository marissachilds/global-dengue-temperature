library(tidyverse)
library(magrittr)
library(rgee)
library(sf)
library(maptools)

study_countries = c("Bolivia", "Peru", "Mexico", "Colombia",  "Costa Rica",
                    "Taiwan", "Brazil", "Honduras", "El Salvador", 
                    "Lao People's Democratic Republic", "Nicaragua", 
                    "Venezuela", "Viet Nam", "Dominican Republic", "Indonesia", 
                    "Panama", "Sri Lanka", "Philippines", "Thailand", 
                    "Cambodia", "Malaysia")
stations <- read_fwf("./data/ghcn_station_data/ghcnd-inventory.txt")
stations %<>% set_colnames(c("id", "lat", "lon", "var", "start", "end"))
stations %<>% st_as_sf(coords = c("lon","lat"), remove = FALSE)

# check the stations look reasonable 
data(wrld_simpl)
wrld_simpl %<>% st_as_sf()

study_stations <- stations %>% 
  st_set_crs(4326) %>%
  filter(var == "TAVG") %>% 
  st_filter(wrld_simpl %>% 
              filter(NAME %in% study_countries))

ggplot() + 
  geom_sf(data = wrld_simpl %>% 
            st_transform(crs = 4326)) + 
  geom_sf(data = stations %>% 
            st_set_crs(4326), 
          color = "blue", size = 0.2) + 
  geom_sf(data = study_stations, 
          color = "red") + 
  theme_void()

# for all of those stations, download the GHCN data 
ghcn_files <- study_stations %>% 
  pull(id) %>% 
  unique %>% 
  purrr::map(function(x){
    download.file(paste0("https://www.ncei.noaa.gov/data/global-historical-climatology-network-daily/access/", x, ".csv"),
                  paste0("./data/ghcn_station_data/", x, ".csv"))
    return(paste0("./data/ghcn_station_data/", x, ".csv"))
  })

saveRDS(study_stations, 
        "./data/ghcn_station_data/study_station_inventory.rds")
