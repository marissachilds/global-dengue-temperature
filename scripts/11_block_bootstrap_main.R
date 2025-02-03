# load packages
library(dplyr)
library(magrittr)
library(fixest)
library(foreach)
library(doParallel)

source("./scripts/functions.R")
# set options 
n_boot <- 1000 

if(Sys.getenv('SLURM_JOB_ID') != ""){
  # registerDoParallel(cores = Sys.getenv("SLURM_NTASKS_PER_NODE"))
  # print(paste0("working with ", Sys.getenv("SLURM_NTASKS_PER_NODE"), " cores"))
  print(paste0("working with ", as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE")), " total cores"))
  print(paste0("running as ", as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))/2, " separate tasks"))
  registerDoParallel(cores = as.integer(as.numeric(Sys.getenv("SLURM_NTASKS_PER_NODE"))/2))
}else{
  registerDoParallel(cores = 1)
}


# stratified bootstrap function ----
boot_strat_newID <- function(df_ids, # dataset with IDs and state of every station
                             df_full, # full dataset
                             country_id_var,
                             seed = 1234){ # need seed argument so you can set inside bootstrap loop to fix draws that happen in other functions/code
  set.seed(seed)
  ids <- df_ids %>% 
    slice_sample(prop = 1, replace = T) %>% 
    mutate(country_boot_id = 1:n()) 
  df_out <- df_full %>%
    left_join(ids,
              by = country_id_var,
              relationship = "many-to-many") %>%
    filter(!is.na(country_boot_id))
  return(df_out)
}

# load data ----
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

# add lags of temperature ----
dengue_temp %<>% 
  prep_dengue_data_noby %>% 
  filter(!is.na(dengue_inc))

print("data loaded")
# fitting function ----
boot_fit_main_mod <- function(df_ids, # dataset with IDs and state of every station
                              df_full, # full dataset
                              country_id_var,
                              seed = 1234){
  fixest::fepois(dengue_inc ~ 
                   mean_2m_air_temp_degree1_lag1 + mean_2m_air_temp_degree2_lag1 + mean_2m_air_temp_degree3_lag1 + 
                   mean_2m_air_temp_degree1_lag2 + mean_2m_air_temp_degree2_lag2 + mean_2m_air_temp_degree3_lag2 + 
                   mean_2m_air_temp_degree1_lag3 + mean_2m_air_temp_degree2_lag3 + mean_2m_air_temp_degree3_lag3 + 
                   total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | 
                   country_boot_id^id + country_boot_id^year + country_boot_id^month, 
                 weights =~pop, # population weight 
                 data = boot_strat_newID(df_ids, df_full, country_id_var, seed),
                 nthreads = 2) %>% 
    coef() %>% 
    return
}

df_country = dengue_temp %>% select(countryFE) %>% unique

print("starting bootstraps")

list_boot<-foreach(i=1:n_boot) %dopar% {
    print(i)
    boot_fit_main_mod(df_ids = df_country, df_full = dengue_temp, 
                      country_id_var = "countryFE", seed=i)
}

print("finished bootstrapping")
saveRDS(do.call("bind_rows", list_boot), 
        paste0("./output/mod_ests/main_coef_blockboot", n_boot, ".rds"))
