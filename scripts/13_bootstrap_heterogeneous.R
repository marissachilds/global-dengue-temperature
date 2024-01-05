# load packages
library(dplyr)
library(magrittr)
library(fixest)
library(foreach)
library(doParallel)

# set options 
n_boot <- 1000 # try 10 for testing, 1000 for full boostrap
het_tercile_colname <- "continent_tercile"

# max_cores <- 1
# print(paste0("working with ", min(parallel::detectCores(), max_cores), " cores"))
# registerDoParallel(cores = min(parallel::detectCores(), max_cores))
# SLURM_CPUS_PER_TASK
# SLURM_NTASKS_PER_NODE
# print(Sys.getenv("SLURM_CPUS_PER_TASK"))
# print(class(Sys.getenv("SLURM_CPUS_PER_TASK")))
# print(str(Sys.getenv("SLURM_CPUS_PER_TASK")))
if(Sys.getenv('SLURM_JOB_ID') != ""){
  print(paste0("working with ", as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK")), " total cores"))
  print(paste0("running as ", as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))/2, " separate tasks"))
  registerDoParallel(cores = as.integer(as.numeric(Sys.getenv("SLURM_CPUS_PER_TASK"))/2))
}else{
  print("working with 1 core")
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
    group_by(by = all_of(strat_var)) %>% 
    slice_sample(prop = 1, replace = T) %>% # , by = all_of(strat_var)
    ungroup %>%
    select(-any_of(strat_var)) %>% 
    mutate(boot_id = 1:n()) 
  
  df_out <- df_full %>%
    left_join(ids,
              by = id_var#,
              #multiple = "all", 
              ) %>% 
    filter(!is.na(boot_id))  
  return(df_out)
}

# fitting function ----
boot_fit_het_mod <- function(df_ids, # dataset with IDs and state of every station
                              df_full, # full dataset
                              id_var,
                              strat_var, # name of variable you're stratifying on
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
                "total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | countryFE^boot_id + countryFE^year + countryFE^month")
  fixest::fepois(as.formula(fml),
                 weights =~pop, # population weight
                 data = boot_strat_newID(df_ids, df_full, id_var, strat_var, seed),
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
  arrange(country, mid_year, id, date) %>% 
  group_by(country, mid_year, id) %>% 
  mutate(across(union(contains("temp"), contains("precipitation")), 
                list(lag1 =~ lag(.x, 1),
                     lag2 =~ lag(.x, 2),
                     lag3 =~ lag(.x, 3),
                     lag4 =~ lag(.x, 4)))) %>% 
  ungroup %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(country, "_", id)) %>% 
  filter(!is.na(dengue_inc))

print("data loaded")

df_unit <- dengue_temp %>% 
  select(country_id, country) %>% 
  unique 
  # summarise(.by = c(country_id, country)) 

# boot_args <- list(df_ids = df_unit, 
#                   df_full = dengue_temp, 
#                   id_var = "country_id",
#                   strat_var = "country")

print("starting bootstraps")
print(Sys.time())

list_boot<-foreach(i=1:n_boot) %dopar% {
  print(i)
  boot_fit_het_mod(df_ids = df_unit, df_full = dengue_temp, id_var = "country_id", strat_var = "country", seed=i)
}

print("finished bootstrapping")
saveRDS(do.call("bind_rows", list_boot), 
        paste0("./output/mod_ests/het_", het_tercile_colname, "_coef_boot", n_boot, ".rds"))
