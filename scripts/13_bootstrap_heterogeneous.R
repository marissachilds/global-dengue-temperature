# load packages
library(dplyr)
library(magrittr)
library(fixest)
library(foreach)
library(doParallel)

# set options 
source("./scripts/00_utilities/functions.R")
n_boot <- 1000 # try 10 for testing, 1000 for full boostrap

if(Sys.getenv('SLURM_JOB_ID') != ""){
  args = commandArgs(trailingOnly = T)
  het_tercile_colname = args[1]
  print(paste0("bootstrapping hetergeneity estimates for ", het_tercile_colname))
  print(paste0("working with ", as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")), " total cores"))
  print(paste0("running as ", as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))/2, " separate tasks"))
  registerDoParallel(cores = as.integer(as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))/2))
}else{
  het_tercile_colname <- "continent_tercile"
  print(paste0("bootstrapping hetergeneity estimates for ", het_tercile_colname))
  print("working with 1 core")
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

# fitting function ----
boot_fit_het_mod <- function(df_ids, # dataset with IDs and state of every station
                              df_full, # full dataset
                              country_id_var,
                              seed = 1234){
  fml <- paste0("dengue_inc ~ ", 
                het_tercile_colname, ":mean_2m_air_temp_degree1_lag1 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree2_lag1 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree3_lag1 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree1_lag2 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree2_lag2 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree3_lag2 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree1_lag3 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree2_lag3 + ", 
                het_tercile_colname, ":mean_2m_air_temp_degree3_lag3 + ", 
                "total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | country_boot_id^id + country_boot_id^year + country_boot_id^month")
  fixest::fepois(as.formula(fml),
                 weights =~pop, # population weight
                 data = boot_strat_newID(df_ids, df_full, country_id_var, seed),
                 nthreads = 2) %>%
    coef() %>% 
    return
}

# load data ----
unit_covar <- readRDS("./data/unit_covariates.rds") 
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

dengue_temp %<>% left_join(unit_covar %>% select(country, id, mid_year, ends_with("tercile")))

# add lags of temperature ----
dengue_temp %<>% 
  prep_dengue_data_noby() %>% 
  filter(!is.na(dengue_inc))

print("data loaded")

df_country = dengue_temp %>% select(countryFE) %>% unique

print("starting bootstraps")
print(Sys.time())

list_boot<-foreach(i=1:n_boot) %dopar% {
  print(i)
  boot_fit_het_mod(df_ids = df_country, df_full = dengue_temp, 
                   country_id_var = "countryFE", seed=i)
}

print("finished bootstrapping")
saveRDS(do.call("bind_rows", list_boot), 
        paste0("./output/mod_ests/het_", het_tercile_colname, "_coef_blockboot", n_boot, ".rds"))
