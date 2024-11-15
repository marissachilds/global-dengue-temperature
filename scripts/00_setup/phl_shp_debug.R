# library(tidyverse)
# library(magrittr)
# library(sf)
# 
# phl <- st_read("./data/phl_adminboundaries_candidate_exclude_adm3", 
#                layer = "phl_admbnda_adm2_psa_namria_20200529")
# 
# data.frame(npts = mapview::npts(phl, by_feature = TRUE), ind = 1:87) %>% arrange(npts)
# 
# st_write(phl[65,], "./data/phl_palawan", driver = "ESRI Shapefile")
# st_write(phl[-65,], "./data/phl_no_palawan", driver = "ESRI Shapefile")
# 
# # upload both to earth engine and see what happens
# # palawan (row 65) seems to be the source of the problem, can we just simpify it? 
# 
# palawan_simple <- st_simplify(phl[65,], 
#                               preserveTopology = T, 
#                               dTolerance = 0.000005)
# mapview::npts(palawan_simple)
# mapview::npts(phl[65,])
# 
# st_write(palawan_simple, "./data/phl_palawan_simp", driver = "ESRI Shapefile")

# to simplify palawan and save the whole shapefile for upload to earth engine
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
