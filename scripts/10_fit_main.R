# load packages
library(tidyverse)
library(magrittr)
library(fixest)
source("./scripts/00_utilities/functions.R")

dengue_temp <- readRDS("./data/dengue_temp_full.rds")

# add lags of temperature
dengue_temp %<>% 
  prep_dengue_data %>% 
  filter(!is.na(dengue_inc)) %>% 
  mutate(dengue_inc_lag = lag(dengue_inc, 1), 
         dengue_inc_lag7_18 = lag(dengue_inc, 7) + lag(dengue_inc, 8) + lag(dengue_inc, 9) + lag(dengue_inc, 10) + lag(dengue_inc, 11) + lag(dengue_inc, 12) + 
           lag(dengue_inc, 13) + lag(dengue_inc, 14) + lag(dengue_inc, 15) + lag(dengue_inc, 16) + lag(dengue_inc, 17) + lag(dengue_inc, 18), 
         .by = c(countryFE, id))

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
  lagged_dengue = fixest::fepois(dengue_inc ~ mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                                   mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                                   mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                                   poly(total_precipitation_lag1,3) + poly(total_precipitation_lag2,3) + poly(total_precipitation_lag3,3) + 
                                   poly(dengue_inc_lag, 2)*poly(dengue_inc_lag7_18, 2) | 
                                   countryFE^id[year] + countryFE^year + countryFE^month, 
                                 weights =~pop, # population weight 
                                 data = dengue_temp %>% 
                                   filter(!is.na(dengue_inc_lag7_18) & !is.na(dengue_inc_lag))))

saveRDS(robust_check, 
        "./output/mod_ests/all_models.rds")

purrr::map(robust_check, function(x){
  list(vcov = vcov_cluster(x, "countryFE"), 
       coef = coef(x)) %>% 
    return
}) %>% 
  set_names(names(robust_check)) %>% 
  saveRDS("./output/mod_ests/all_models_vcv_coef.rds")


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
                   "cor2"), 
  lagged_dengue = r2(fixest::fepois(dengue_inc ~ 
                                      poly(total_precipitation_lag1,3) + poly(total_precipitation_lag2,3) + poly(total_precipitation_lag3,3) + 
                                      poly(dengue_inc_lag, 2)*poly(dengue_inc_lag7_18, 2) | 
                                      countryFE^id[year] + countryFE^year + countryFE^month, 
                                    weights =~pop, # population weight 
                                    data = dengue_temp  %>% 
                                      filter(!is.na(dengue_inc_lag7_18) & !is.na(dengue_inc_lag))),
                     "cor2"))

map2_dbl(robust_check, 
         comp_cor2, 
         function(x, y){
           r2(x, "cor2") - y
         }) %>% 
  set_names(names(robust_check)) %>% 
  saveRDS("./output/mod_ests/all_models_change_cor2.rds")
