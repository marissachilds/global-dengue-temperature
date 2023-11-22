# Estimate percent change in dengue between different climate scenarios 
# and save aggregates of the median estimates across model bootstrapped 
# coefficients and GCMs to use in subsequent analysis/figures.
# packages for general data manipulation 
library(tidyverse)
library(magrittr)
library(data.table)
# ...for faster matrix multiplication 
library(Rcpp)
# ...for quantiles across values in rows
library(matrixStats)
# ...for running in parallel
library(foreach)
library(doParallel)

if(Sys.getenv('SLURM_JOB_ID') != ""){
  print(paste0("Running with ", Sys.getenv("SLURM_NTASKS_PER_NODE"), " cores"))
  registerDoParallel(cores = Sys.getenv("SLURM_NTASKS_PER_NODE"))
}else{
  registerDoParallel(cores = 1)
}

sourceCpp("./scripts/mat_mult.cpp")

# set some parameters ----
# cur_country = "BRA"
all_scenarios = c("ssp126", "ssp245", "ssp370", "plusone", "hist-nat")
# "base" is the one scenario we're calculating the % change from
compare_scenarios = data.frame(base = c("ssp126"), 
                               change = c("ssp370")) 
which_boot_ind <- 1:25 # 25 bootstraps took ~75 GB (call it 100 to be safe) of memory, but 100 bootstraps failed with 360 GB
quant_vals <- c(0, 0.01, 0.025, 0.05, 0.25, 0.5, 0.75, 0.95, 0.975, 0.99, 1)

# load bootstrap coefficients 
boot_coef <- readRDS("./output/mod_ests/main_coef_boot1000.rds") %>% 
  magrittr::extract(which_boot_ind,)
model_lags <- grep("temp", colnames(boot_coef), value = T) %>% 
  gsub(".*_lag", "", .) %>% unique %>% as.numeric
temp_inds <- which(grepl("temp", colnames(boot_coef)))

# load dengue data to figure out which units have non-zero dengue during the study period 
dengue_units <- readRDS("./data/dengue_temp_full.rds") %>% 
  group_by(country, mid_year, id) %>% 
  summarise(total_dengue = sum(dengue_cases, na.rm = T)) %>% 
  ungroup %>% 
  # drop LKA1, filter to the max mid_year for each country 
  filter(country != "LKA1") %>% 
  group_by(country) %>% 
  filter(mid_year == max(mid_year)) %>% 
  ungroup %>% 
  select(-mid_year)

# load population data
pop <- readRDS("./data/all_pop2015.rds") 

# join dengue incidence to population data to be used in the country averages
pop %<>% 
  mutate(id_join = case_when(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX") ~ stringi::stri_trans_general(id, id = "Latin-ASCII"), 
                             country == "BRA" ~ str_sub(id, 1, 8), 
                             country == "IDN" ~ case_when(id == "Dki Jakarta" ~ "Jakarta", 
                                                          id == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta", 
                                                          # kalimantan utara gets assigned the dengue cases from timur, which is okay since we only care about whether dengue is > 0
                                                          id == "Kalimantan Utara" ~ "Kalimantan Timur", 
                                                          T ~ id), 
                             T ~ id) %>% tolower) %>% 
  filter(id_join != "br430000" & id != "[unknown]") %>% 
  full_join(dengue_units %>% rename(id_join = id), 
            multiple = "warning")  %>% 
  mutate(nonzero_dengue = 1*(total_dengue > 0))

# load observed data 
clim_obs <- readRDS("./data/dT_combined/scenarios_era_current_current.gz") %>% 
  rename_with(function(x){gsub("temperature", "temp", x)}, contains("temperature")) %>%
  select(-ends_with("degree5"), -ends_with("degree4")) %>% 
  as.data.table()
# clim_obs = clim_obs[country == cur_country,]
# print(nrow(clim_obs))
clim_obs = clim_obs[order(country, id, year, month),]  
# add lags 1 - 3
temp_cols <- grep("temp", colnames(clim_obs), value=TRUE)
for ( i in model_lags ) { # update for the number of shifts
  lag_names <- paste(temp_cols, "_lag", i, sep = "")
  clim_obs[,(lag_names) := lapply(.SD, shift, i, type = "lag"),
           by=c("country", "id"), 
           .SDcols=temp_cols] 
}
clim_obs = clim_obs[year >= 1995,]

# calculate "observed" dengue 
all_temp_cols = (colnames(boot_coef)[temp_inds])
dengue_obs <- eigenMapMatMult(as.matrix(clim_obs[,..all_temp_cols]), 
                              t(as.matrix(boot_coef[,temp_inds])))

# now loop through the listed scenarios
all_scenario_dengue <- foreach(scenario = all_scenarios) %dopar% {
  print(paste0("starting on scenario ", scenario, "----------"))
  dengue_est <- list.files("./data/dT_combined", 
                            pattern = scenario, 
                            full.names = T) %>% 
    purrr::map(function(scenario_fname){
      print(scenario_fname)
      # load climate scenario data and add temperature lags
      clim_scenario <- readRDS(scenario_fname) %>% 
        rename_with(function(x){gsub("temperature", "temp", x)}, contains("temperature")) %>%
        select(-ends_with("degree5"), -ends_with("degree4")) %>% 
        as.data.table() 
      # clim_scenario = clim_scenario[country == cur_country,]
      # sometime theres multiple obs for a country-id-year-month that differ by machine precision amount, so take the first
      clim_scenario = clim_scenario[, head(.SD, 1), by = c("country", "id", "year", "month")]
      clim_scenario = clim_scenario[order(country, id, year, month),]  
      # add lags 1 - 3
      temp_cols <- grep("temp", colnames(clim_scenario), value=TRUE)
      for ( i in model_lags ) { 
        lag_names <- paste(temp_cols, "_lag", i, sep = "")
        clim_scenario[,(lag_names) := lapply(.SD, shift, i, type = "lag"),
                      by=c("country", "id"), 
                      .SDcols=temp_cols] 
      }
      clim_scenario = clim_scenario[year >= 1995,]
      
      # return scenario dengue and temperature
      return(list(dengue_est = eigenMapMatMult(as.matrix(clim_scenario[,..all_temp_cols]),
                                               t(as.matrix(boot_coef[,temp_inds]))), 
                  T_est = clim_scenario$mean_2m_air_temp_degree1))})
  
  # calculate differences from "observed"
  print(paste0("calculating % change in dengue for scenario ", scenario, "----------"))
  dengue_diff <- map(dengue_est, ~subtract(.x[["dengue_est"]], dengue_obs)) %>% 
    reduce(cbind) %>% 
    # convert to % change in dengue
    exp %>% 
    subtract(1) 
  # print(dim(dengue_diff))
  T_diff <- map(dengue_est, ~.x[["T_est"]] - clim_obs$mean_2m_air_temp_degree1) %>% 
    reduce(cbind)
  # print(dim(T_diff))
  
  # calculate summary stats ----
  print(paste0("calculating summary statistics for scenario ", scenario, "----------"))
  
  # 1) for each time point, quantiles of coef*scenario_temp - coef*obs temp
  # calculate quantiles for each row (which is a unit-month)
  # unit_month_dengue %<>% apply(1, function(x) quantile(x, quant_vals))
  # unit_month_dengue <- dengue_diff %>% rowQuantiles(probs=quant_vals) %>%
  #   set_colnames(paste0("q_", quant_vals)) %>%
  #   as.data.table
  # 
  # # similarly, loop through and grab T_diff, cbind together, calculate quantiles for each row (although with only 21 scenario, there will be fewer quantiles )
  # if(scenario != "plusone"){
  #   unit_month_T <- T_diff %>%
  #     rowQuantiles(probs=quant_vals) %>%
  #     set_colnames(paste0("q_", quant_vals)) %>%
  #     as.data.table
  # } else{ unit_month_T <- T_diff %>%
  #   as.matrix(ncol = 1) %>%
  #   set_colnames("q_0.5") %>%
  #   as.data.table}
  # 
  # # cbind the dengue quantiles and T quantiles with unit-year-month identifiers
  # cbind(clim_obs %>%
  #         select(country, id, year, month),
  #       unit_month_T %>% rename_with(function(x) paste0("dTemp_", x)),
  #       unit_month_dengue %>% rename_with(function(x) paste0("pct_change_dengue_", x))) %>%
  #   saveRDS(paste0("./output/projection_ests/unit_month_changes_maxBoot", max(which_boot_ind),
  #                  "_scenario_", scenario, ".rds"),
  #           compress = T)
  # print("unit-month quantiles saved")
  # rm(unit_month_T, unit_month_dengue)
  
  # 2) for each unit, average over time to get that place's pct change in dengue
  unit_dengue = dengue_diff %>% 
    as.data.frame() %>% 
    cbind(clim_obs %>%
            select(country, id)) %>% 
    as.data.table() %>% 
    melt(id.vars = c("id", "country")) 
  # print(str(unit_dengue))
  
  # calculate the average within bootstrap
  unit_dengue = unit_dengue[,.(mean=mean(value)),.(country, id, variable)]
  # print(str(unit_dengue))
  
  print("calculated unit average dengue")
  # also calculate mean temperature change 
  unit_T <- T_diff %>%
    as.data.frame() %>%
    set_colnames(paste0("V", 1:ncol(.))) %>%
    cbind(clim_obs %>%
            select(country, id)) %>%
    as.data.table() %>%
    melt(id.vars = c("id", "country"))
  
  # average over all months for a unit and GCM
  unit_T = unit_T[, .(mean=mean(value)), .(country, id, variable)] # variable is GCM x bootstrap
  print("calculated unit average temperature")
  
  unit_T_quant <- unit_T[,.(quant = quantile(mean, quant_vals, na.rm = T)), .(country, id)] %>% 
    cbind(name = paste0("dTemp_q_", quant_vals)) %>% 
    dcast(country + id ~ name, 
          value.var = "quant") 
  unit_dengue_quant <- unit_dengue[,.(quant = quantile(mean, quant_vals, na.rm = T)), .(country, id)] %>% 
    cbind(name = paste0("pct_change_dengue_q_", quant_vals)) %>% 
    dcast(country + id ~ name, 
          value.var = "quant")
  
  # then get quantiles across each bootstrap
  unit_dengue_quant[unit_T_quant, on = c("country", "id")] %>% 
    saveRDS(paste0("./output/projection_ests/unit_changes_maxBoot", max(which_boot_ind),
                   "_scenario_", scenario, ".rds"), 
            compress = T)
  
  print("unit quantiles saved")
  rm(unit_T_quant, unit_dengue_quant)
  
  # 3) for each country, use average over time for each unit from above
  # then do pop-weighted average to get country estimate for each bootstrap
  # join the population data
  country_dengue = unit_dengue[pop %>% 
                                 as.data.table, 
                               on = c("country", "id")] 
  
  # calculate pop-weighted avg pct change in each country for each bootstrap
  country_dengue = country_dengue[, .(mean = weighted.mean(mean, sum*nonzero_dengue)), by = .(country, variable)]
  
  country_T = unit_T[pop %>% 
                       as.data.table, 
                     on = c("country", "id")]
  # calculate pop-weighted avg dT in each country for each bootstrap
  country_T = country_T[, .(mean = weighted.mean(mean, sum*nonzero_dengue)), by = .(country, variable)]
  
  full_join(country_dengue[,.(quant = quantile(mean, quant_vals, na.rm = T)), .(country)] %>% 
              cbind(name = paste0("pct_change_dengue_q_", quant_vals)) %>% 
              dcast(country ~ name, 
                    value.var = "quant"), 
            country_T[,.(quant = quantile(mean, quant_vals, na.rm = T)), .(country)] %>% 
              cbind(name = paste0("dTemp_q_", quant_vals)) %>% 
              dcast(country ~ name, 
                    value.var = "quant")) %>% 
    saveRDS(paste0("./output/projection_ests/country_changes_maxBoot", max(which_boot_ind),
                   "_scenario_", scenario, ".rds"), 
            compress = T)
  
  print("country quantiles saved")
  
  if(scenario %in% compare_scenarios$base | scenario %in% compare_scenarios$change){
    out <- list(dengue_est = map(dengue_est, ~.x[["dengue_est"]]) %>% reduce(cbind), 
                T_est = map(dengue_est, ~.x[["T_est"]]) %>% reduce(cbind))
  } else{ out <- NA}
  print(paste0("finished with scenario ", scenario, "----------"))
  return(out)
}  
rm(dengue_obs)
all_scenario_dengue %<>% set_names(all_scenarios)

print("now moving on to between-scenario comparisons ----------")
# for each other comparison we want to make, loop through and make the country calculations
purrr::pmap(compare_scenarios,
            function(base, change){
              dengue_diff <- subtract(all_scenario_dengue[[change]]$dengue_est,
                                      all_scenario_dengue[[base]]$dengue_est) %>%
                exp %>%
                subtract(1)

              T_diff <- subtract(all_scenario_dengue[[change]]$T_est,
                                 all_scenario_dengue[[base]]$T_est)
              unit_dengue = dengue_diff %>%
                as.data.frame() %>%
                cbind(clim_obs %>%
                        select(country, id)) %>%
                as.data.table() %>%
                melt(id.vars = c("id", "country"))
              unit_dengue = unit_dengue[,.(mean=mean(value)),.(country, id, variable)]

              # also calculate mean temperature change
              unit_T <- T_diff %>%
                as.data.frame() %>%
                set_colnames(paste0("V", 1:ncol(.))) %>%
                cbind(clim_obs %>%
                        select(country, id)) %>%
                as.data.table() %>%
                melt(id.vars = c("id", "country"))
              # average over all months for a unit and GCM
              unit_T = unit_T[,.(mean=mean(value)),.(country, id, variable)]

              # for each country, use average over time for each unit from above
              # then do pop-weighted average to get country estimate for each bootstrap
              # join the population data
              country_dengue = unit_dengue[pop %>%
                                             as.data.table,
                                           on = c("country", "id")]

              # calculate pop-weighted avg pct change in each country for each bootstrap
              country_dengue = country_dengue[, .(mean = weighted.mean(mean, sum*nonzero_dengue)), by = .(country, variable)]
              
              country_T = unit_T[pop %>% 
                                   as.data.table, 
                                 on = c("country", "id")]
              # calculate pop-weighted avg dT in each country for each bootstrap
              country_T = country_T[, .(mean = weighted.mean(mean, sum*nonzero_dengue)), by = .(country, variable)]
              
              # country_dengue[,.(quant = quantile(mean, quant_vals, na.rm = T)), .(country)] %>%
              #   cbind(name = paste0("q_", quant_vals)) %>%
              #   dcast(country ~ name,
              #         value.var = "quant") %>%
              full_join(country_dengue[,.(quant = quantile(mean, quant_vals, na.rm = T)), .(country)] %>% 
                          cbind(name = paste0("pct_change_dengue_q_", quant_vals)) %>% 
                          dcast(country ~ name, 
                                value.var = "quant"), 
                        country_T[,.(quant = quantile(mean, quant_vals, na.rm = T)), .(country)] %>% 
                          cbind(name = paste0("dTemp_q_", quant_vals)) %>% 
                          dcast(country ~ name, 
                                value.var = "quant")) %>% 
                saveRDS(paste0("./output/projection_ests/country_changes_maxBoot", max(which_boot_ind),
                               "_scenario_", change, "_minus_", base, ".rds"),
                        compress = T)
              print(paste0("country quantiles saved for comparison of ", base, " and ", change))
            })
  


# SCRATCH -----------------  
  # clim_scenario %>% 
  #   select(country, id, year, month) %>% 
  #   mutate(T_diff = clim_obs$mean_2m_air_temp_degree1 - clim_scenario$mean_2m_air_temp_degree1)
  
  # exp(dengue_1 - dengue_2) - 1
# 2 is baseline/observed temps
# 1 is scenario temps
# saveRDS(dengue_est, 
#         "./output/scenario_est_intermediate/dengue_ACCESS-CM2_hist-nat.rds")

# OLD CODE 
# clim_scenario <- readRDS(scenario_fname) %>% 
#   rename_with(function(x){gsub("temperature", "temp", x)}, contains("temperature")) %>%
#   select(-ends_with("degree5"), -ends_with("degree4")) %>% 
#   mutate(date = as.Date(paste0(year, "-", month, "-01"), format = "%Y-%m-%d")) %>% 
#   group_by(country, id) %>%
#   arrange(country, id, date) %>% 
#   mutate(across(contains("temp"), 
#                 list(lag1 =~ lag(.x, 1),
#                      lag2 =~ lag(.x, 2),
#                      lag3 =~ lag(.x, 3)))) %>% 
#   ungroup %>% 
#   filter(year >= 1995)# %>% 
# filter(country == x)




# clim_obs[order(country, id, year, month), 
#          c("lag1", "lag2", "lag3"):=shift(.SD, 1:3), 
#          by=c("country", "id"), 
#          .SDcols=grep("temp", colnames(clim_obs), value=TRUE)] 
# 
# clim_obs %>% 
#   mutate(across(contains("temp"), 
#                 list(lag1 =~ lag(.x, 1),
#                      lag2 =~ lag(.x, 2),
#                      lag3 =~ lag(.x, 3))), 
#          .by = c(country, id)) %>% 
#   str
#   filter(year >= 1995)
# scenarios_CESM2_ssp126.gz" is the wrong dimensions, doesn't match clim_obs
# same with "./data/dT_combined/scenarios_CNRM-CM6-1_ssp126.gz"
# full_join(clim_scenario %>% 
#             distinct %>% 
#             summarise(n_scenario = n(), 
#                       .by = c(country, id)), 
#           clim_obs %>% 
#             summarise(n_obs = n(), 
#                       .by = c(country, id))) %>% 
#   mutate(diff = n_scenario - n_obs) %>% 
#   View

# check_dims <- list.files("./data/dT_combined",
#                          full.names = T) %>% #
#   purrr::map(function(scenario_fname){
#     print(scenario_fname)
#     # load climate scenario data and add temperature lags
#     clim_scenario <- readRDS(scenario_fname) %>%
#       rename_with(function(x){gsub("temperature", "temp", x)}, contains("temperature")) %>%
#       select(-ends_with("degree5"), -ends_with("degree4")) %>%
#       as.data.table()
#     full_join(clim_scenario %>%
#                 distinct %>%
#                 summarise(n_scenario = n(),
#                           n_ym = n_distinct(paste0(year, "-", month)),
#                           .by = c(country, id)),
#               clim_obs %>%
#                 summarise(n_expect = n() + 6,
#                           .by = c(country, id))) %>%
#       mutate(diff = n_scenario - n_expect,
#              scenario = gsub(".*/|\\.gz", "", scenario_fname)) %>%
#       filter(diff != 0) %>%
#       return})
# # 
# check_dims %>%
#   list_rbind() %>% View
#   write.csv("./data/scenario_extra_obs.csv", 
#             row.names = F)

# scenarios_ACCESS-CM2_hist-nat
# BR1100320
# test <- readRDS("./data/dT_combined/scenarios_ACCESS-CM2_hist-nat.gz")
# test %>%
#   filter(id == "BR1100320") %>% 
#   slice_head(n = 1, by = c(year, month)) %>% 
#   nrow
#   arrange(year, month) %>% 
#   View
#   pull(mean_2m_air_temperature_degree1)
#   
  
# loop through and grab dengue_diff, cbind together
# all_dengue <- map(dengue_diff, ~.x[["dengue_diff"]]) %>% 
#   reduce(cbind) %>% 
#   # convert to % change in dengue
#   exp %>% 
#   subtract(1) 
# dengue_diff = subtract(
#   eigenMapMatMult(as.matrix(clim_scenario[,..all_temp_cols]),
#                   t(as.matrix(boot_coef[,temp_inds]))), 
#   dengue_obs) 
# T_diff = clim_scenario$mean_2m_air_temp_degree1 - clim_obs$mean_2m_air_temp_degree1
# all_T <- map(dengue_diff, ~.x[["T_diff"]] ) %>% 
#   reduce(cbind)
# cbind(clim_obs %>%
#         select(country, id, year, month), 
#       all_dengue) %>% 
#   saveRDS("./output/all_dengue_ssp126.rds") # ~850 MB for 3 bootstrap coefs and 21 GCMs --> lets not save them for all bootstrap
