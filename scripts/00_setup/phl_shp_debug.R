library(tidyverse)
library(magrittr)
library(sf)

phl <- st_read("./data/phl_adminboundaries_candidate_exclude_adm3", 
               layer = "phl_admbnda_adm2_psa_namria_20200529")

# for rows whose geometry has over 1e6 (earth engine max) vertices, simplify it
phl %<>% rowwise() %>% 
  mutate(geometry = ifelse(mapview::npts(geometry) > 1e6, 
                           st_simplify(geometry, 
                                       preserveTopology = T, 
                                       dTolerance = 0.000005), 
                           geometry)) %>% 
  ungroup 

st_write(phl, "./data/PHL_palawan_simp", driver = "ESRI Shapefile")
