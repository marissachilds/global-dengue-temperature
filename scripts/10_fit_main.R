# load packages
library(tidyverse)
library(magrittr)
library(fixest)
source("./scripts/00_utilities/functions.R")

dengue_temp <- readRDS("./data/dengue_temp_full.rds")

# add lags of temperature
dengue_temp %<>% 
  prep_dengue_data %>% 
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
                               data = dengue_temp))

saveRDS(robust_check, 
        "./output/all_models.rds")

robust_check %>% 
  purrr::map(function(x){
    list(coef = coef(x), 
         vcov = vcov(x, "cluster"))
  }) %>% saveRDS("./output/robust_check_coef_vcv.rds")

   
comp_cor2 <- list(
  main = r2(fixest::fepois(dengue_inc ~ 
                             total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                             countryFE^id + countryFE^year + countryFE^month, 
                           weights =~pop, # population weight 
                           data = dengue_temp), 
            "cor2"),
  no_pop_weight = r2(fixest::fepois(dengue_inc ~ 
                                      total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                      countryFE^id + countryFE^year + countryFE^month, 
                                    data = dengue_temp),
                     "cor2"),
  no_precip = r2(fixest::fepois(dengue_inc ~ 1 | countryFE^id + countryFE^year + countryFE^month,
                                weights =~pop, # population weight
                                data = dengue_temp),
                 "cor2"),
  precip_sq = r2(fixest::fepois(dengue_inc ~ 
                                  total_precipitation_lag1 + total_precipitation_lag1^2 + 
                                  total_precipitation_lag2 + total_precipitation_lag2^2 + 
                                  total_precipitation_lag3 + total_precipitation_lag3^2 | 
                                  countryFE^id + countryFE^year + countryFE^month,
                                weights =~pop, # population weight
                                data = dengue_temp),
                 "cor2"),
  no_brazil = r2(fixest::fepois(dengue_inc ~ 
                                  total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                  countryFE^id + countryFE^year + countryFE^month, 
                                weights =~pop, # population weight
                                data = dengue_temp %>% 
                                  filter(country != "BRA")),
                 "cor2"),
  poly2 = r2(fixest::fepois(dengue_inc ~ 
                              total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                              countryFE^id + countryFE^year + countryFE^month, 
                            weights =~pop, # population weight 
                            data = dengue_temp),
             "cor2"),
  poly4 = r2(fixest::fepois(dengue_inc ~ 
                              total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                              countryFE^id + countryFE^year + countryFE^month, 
                            weights =~pop, # population weight 
                            data = dengue_temp),
             "cor2"),
  poly5 = r2(fixest::fepois(dengue_inc ~ 
                              total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                              countryFE^id + countryFE^year + countryFE^month, 
                            weights =~pop, # population weight 
                            data = dengue_temp),
             "cor2"),
  lag2 = r2(fixest::fepois(dengue_inc ~ 
                             total_precipitation_lag1 + total_precipitation_lag2 | 
                             countryFE^id + countryFE^year + countryFE^month, 
                           weights =~pop, # population weight 
                           data = dengue_temp),
            "cor2"),
  lag4 = r2(fixest::fepois(dengue_inc ~ 
                             total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 + total_precipitation_lag4 | 
                             countryFE^id + countryFE^year + countryFE^month, 
                           weights =~pop, # population weight 
                           data = dengue_temp),
            "cor2"),
  lag3_w_lag0 = r2(fixest::fepois(dengue_inc ~ 
                                    total_precipitation + total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                    countryFE^id + countryFE^year + countryFE^month, 
                                  weights =~pop, # population weight 
                                  data = dengue_temp),
                   "cor2"),
  country_mos_FE = r2(fixest::fepois(dengue_inc ~ 
                                       total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                       countryFE^id + countryFE^year^month, 
                                     weights =~pop, # population weight 
                                     data = dengue_temp), 
                      "cor2"),
  unit_season = r2(fixest::fepois(dengue_inc ~ 
                                    total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                                    countryFE^id + countryFE^year + countryFE^id^month,
                                  weights =~pop, # population weight
                                  data = dengue_temp),
                   "cor2"))

map2_dbl(robust_check, 
         comp_cor2, 
         function(x, y){
           r2(x, "cor2") - y
         }) %>% 
  set_names(names(robust_check)) %>% 
  saveRDS("./output/all_models_change_cor2.rds")
