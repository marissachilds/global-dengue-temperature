# load packages
library(tidyverse)
library(magrittr)
library(fixest)

dengue_temp <- readRDS("./data/dengue_temp_full.rds")

# add lags of temperature
dengue_temp %<>% 
  arrange(country, mid_year, id, date) %>% 
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

# fit specs with analytic CIs ----
robust_check <- list(
  main = fixest::fepois(dengue_inc ~ 
                          mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                          mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                          mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                          total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                          countryFE^id + countryFE^year + countryFE^month, 
                        weights =~pop, # population weight 
                        data = dengue_temp),
  no_pop_weight = fixest::fepois(dengue_inc ~ 
                                   mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                                   mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                                   mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                                   total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                   countryFE^id + countryFE^year + countryFE^month, 
                                 data = dengue_temp),
  no_precip = fixest::fepois(dengue_inc ~ 
                               mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                               mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                               mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 | 
                               countryFE^id + countryFE^year + countryFE^month,
                             weights =~pop, # population weight
                             data = dengue_temp),
  precip_sq = fixest::fepois(dengue_inc ~ 
                               mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                               mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                               mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                               total_precipitation_lag1 + total_precipitation_lag1^2 + 
                               total_precipitation_lag2 + total_precipitation_lag2^2 + 
                               total_precipitation_lag3 + total_precipitation_lag3^2 | 
                               countryFE^id + countryFE^year + countryFE^month,
                             weights =~pop, # population weight
                             data = dengue_temp),
  no_brazil = fixest::fepois(dengue_inc ~ 
                               mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                               mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                               mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                               total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                               countryFE^id + countryFE^year + countryFE^month, 
                             weights =~pop, # population weight
                             data = dengue_temp %>% 
                               filter(country != "BRA")),
  poly2 = fixest::fepois(dengue_inc ~ 
                           mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + 
                           mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + 
                           mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + 
                           total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                           countryFE^id + countryFE^year + countryFE^month, 
                         weights =~pop, # population weight 
                         data = dengue_temp),
  poly4 = fixest::fepois(dengue_inc ~ 
                           mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + mean_2m_air_temp_degree4_lag1 + 
                           mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + mean_2m_air_temp_degree4_lag1 + 
                           mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + mean_2m_air_temp_degree4_lag1 + 
                           total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                           countryFE^id + countryFE^year + countryFE^month, 
                         weights =~pop, # population weight 
                         data = dengue_temp),
  poly5 = fixest::fepois(dengue_inc ~ 
                           mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + mean_2m_air_temp_degree4_lag1 + mean_2m_air_temp_degree5_lag1 + 
                           mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + mean_2m_air_temp_degree4_lag2 + mean_2m_air_temp_degree5_lag2 + 
                           mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + mean_2m_air_temp_degree4_lag3 + mean_2m_air_temp_degree5_lag3 + 
                           total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                           countryFE^id + countryFE^year + countryFE^month, 
                         weights =~pop, # population weight 
                         data = dengue_temp),
  lag2 = fixest::fepois(dengue_inc ~ 
                          mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                          mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                          total_precipitation_lag1 + total_precipitation_lag2 | 
                          countryFE^id + countryFE^year + countryFE^month, 
                        weights =~pop, # population weight 
                        data = dengue_temp),
  lag4 = fixest::fepois(dengue_inc ~ 
                          mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                          mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                          mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                          mean_2m_air_temp_degree1_lag4 + mean_2m_air_temp_degree2_lag4 + mean_2m_air_temp_degree3_lag4 + 
                          total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 + total_precipitation_lag4 | 
                          countryFE^id + countryFE^year + countryFE^month, 
                        weights =~pop, # population weight 
                        data = dengue_temp),
  lag3_w_lag0 = fixest::fepois(dengue_inc ~ 
                                 mean_2m_air_temp_degree1 + mean_2m_air_temp_degree2 + mean_2m_air_temp_degree3 + 
                                 mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                                 mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                                 mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                                 total_precipitation + total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                 countryFE^id + countryFE^year + countryFE^month, 
                               weights =~pop, # population weight 
                               data = dengue_temp),
  country_mos_FE = fixest::fepois(dengue_inc ~ 
                                    mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                                    mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                                    mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                                    total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                    countryFE^id + countryFE^year^month, 
                                  weights =~pop, # population weight 
                                  data = dengue_temp), 
  unit_season = fixest::fepois(dengue_inc ~ 
                                 mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                                 mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                                 mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                                 total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                 countryFE^id + countryFE^year + countryFE^id^month,
                               weights =~pop, # population weight
                               data = dengue_temp),
  country_trend = fixest::fepois(dengue_inc ~
                                   mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                                   mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                                   mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                                   total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                   countryFE^id + countryFE^month + countryFE[year] + countryFE[year^2], 
                                 weights =~pop, # population weight 
                                 data = dengue_temp %>% 
                                   mutate(year = year - min(year), 
                                          .by = countryFE)), 
  pop_offset = fixest::fepois(dengue_cases ~
                                mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                                mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                                mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                                total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                countryFE^id + countryFE^year + countryFE^month, 
                              offset =~ log(pop),
                              data = dengue_temp))

saveRDS(robust_check, 
        "./output/all_models.rds")

robust_check %>% 
  purrr::map(function(x){
    list(coef = coef(x), 
         vcov = vcov(x, "cluster"))
  }) %>% saveRDS("./output/robust_check_coef_vcv.rds")


   