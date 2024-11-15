# fit models with heterogeneity 
library(tidyverse)
library(magrittr)
library(fixest)
source("./scripts/00_utilities/functions.R")

unit_covar <- readRDS("./data/unit_covariates.rds") 
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

dengue_temp %<>% left_join(unit_covar %>% select(country, id, mid_year, ends_with("tercile")))

# add lags, calculate dengue incidence, make some FEs from others
dengue_temp %<>% 
  prep_dengue_data %>% 
  filter(!is.na(dengue_inc))

# other units are missing subcountry dengue if they have all NAs for dengue, but they get dropped in line 22 above
het_ests <- dengue_temp %>% 
  select(ends_with("tercile")) %>% 
  colnames %>% 
  purrr::map(function(x){
    fml <- paste0("dengue_inc ~ ", 
                  x, ":mean_2m_air_temp_degree1_lag1 + ", 
                  x, ":mean_2m_air_temp_degree2_lag1 + ", 
                  x, ":mean_2m_air_temp_degree3_lag1 + ", 
                  x, ":mean_2m_air_temp_degree1_lag2 + ", 
                  x, ":mean_2m_air_temp_degree2_lag2 + ", 
                  x, ":mean_2m_air_temp_degree3_lag2 + ", 
                  x, ":mean_2m_air_temp_degree1_lag3 + ", 
                  x, ":mean_2m_air_temp_degree2_lag3 + ", 
                  x, ":mean_2m_air_temp_degree3_lag3 + ", 
                  "total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | countryFE^id + countryFE^year + countryFE^month")
    print(fml)
    
    fixest::fepois(as.formula(fml),
                   weights =~pop, # population weight
                   data = dengue_temp) %>% 
      return
  })

saveRDS(het_ests, 
        "./output/mod_ests/het_models.rds")

het_ests %>% 
  purrr::map(function(x){
    list(coef = coef(x), 
         vcov = vcov(x, "cluster"))
  }) %>% 
  saveRDS("./output/mod_ests/het_models_coef_vcv.rds")


