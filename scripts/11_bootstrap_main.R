# load packages
library(dplyr)
library(magrittr)
library(fixest)
library(future.apply)
library(foreach)
library(doParallel)

# set options 
n_boot <- 1000 # try 10 for testing, 1000 for full boostrap

# this might not be right... but lets try having fixest use 1 thread per model, 
# so each worker/thread works on a different bootstrap


if(Sys.getenv('SLURM_JOB_ID') != ""){
  registerDoParallel(cores = Sys.getenv("SLURM_NTASKS_PER_NODE"))
  
}else{
  registerDoParallel(cores = 1)
}

# stratified bootstrap function ----
boot_strat_newID <- function(df_ids, # dataset with IDs and state of every station
                             df_full, # full dataset
                             id_var,
                             strat_var, # name of variable you're stratifying on
                             seed = 1234){ # need seed argument so you can set inside bootstrap loop to fix draws that happen in other functions/code
  set.seed(seed)
  ids <- df_ids %>% 
    slice_sample(prop = 1, replace = T, by = all_of(strat_var)) %>% 
    select(-any_of(strat_var)) %>% 
    mutate(boot_id = 1:n()) 
  
  df_out <- df_full %>%
    left_join(ids,
              by = id_var,
              relationship = "many-to-many") %>% 
    filter(!is.na(boot_id))  
  return(df_out)
}

# load data ----
dengue_temp <- readRDS("dengue_temp_full.rds") 

# add lags of temperature ----
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

print("data loaded")
# fitting function ----
boot_fit_main_mod <- function(df_ids, # dataset with IDs and state of every station
                              df_full, # full dataset
                              id_var,
                              strat_var, # name of variable you're stratifying on
                              seed = 1234){
  fixest::fepois(dengue_inc ~ 
                   mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                   mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                   mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                   total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                   countryFE^boot_id + countryFE^year + countryFE^month, 
                 weights =~pop, # population weight 
                 data = boot_strat_newID(df_ids, df_full, id_var, strat_var, seed)) %>% 
    coef() %>% 
    return
}

df_unit <- dengue_temp %>% 
  summarise(.by = c(country_id, country)) 

boot_args <- list(df_ids = df_unit, 
                  df_full = dengue_temp, 
                  id_var = "country_id",
                  strat_var = "country")

print("starting bootstraps")

list_boot<-foreach(i=1:n_boot) %dopar% {
    print(i)
    boot_fit_main_mod(df_ids = df_unit, df_full = dengue_temp, id_var = "country_id", strat_var = "country", seed=i)
}

print("finished bootstrapping")
saveRDS(do.call("bind_rows", list_boot), 
        paste0("./output/main_coef_boot", n_boot, ".rds"))
