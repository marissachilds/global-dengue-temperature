# fit models with heterogeneity 
library(tidyverse)
library(magrittr)
library(fixest)

unit_covar <- readRDS("./data/unit_covariates.rds") 
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

dengue_temp %<>% left_join(unit_covar %>% select(country, id, mid_year, ends_with("tercile")))

# add lags, calculate dengue incidence, make some FEs from others
dengue_temp %<>% arrange(country, mid_year, id, date) %>% 
  mutate(across(union(contains("temp"), contains("precipitation")), 
                list(lag1 =~ lag(.x, 1),
                     lag2 =~ lag(.x, 2),
                     lag3 =~ lag(.x, 3),
                     lag4 =~ lag(.x, 4))), 
         .by = c(country, mid_year, id)) %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(country, "_", id)) %>% 
  filter(!is.na(dengue_inc))

# double check for missing covariates (we expect TWN to be missing some)
dengue_temp %>% 
  mutate(any_missing_covar = across(ends_with("tercile")) %>% is.na %>% rowSums) %>% 
  filter(is.na(any_missing_covar) | any_missing_covar > 0) %>% 
  pull(country) %>% 
  unique

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


