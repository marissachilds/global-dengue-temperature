library(dplyr)
library(tidyr)
library(magrittr)

scenarios = c("historical", 'ssp126', "ssp245", 'ssp370', "hist-nat")

tcr_range = c(1.4, 2.2)

catalog = read.csv("~/Downloads/pangeo-cmip6.csv")

gcm_tcr = read.csv("./data/GCM TCR.csv")

cat_subset <- catalog %>% 
  # filter to monthly, average temp, for the scenarios we care about
  filter(table_id == "Amon" & 
           variable_id == "tas" &  
           experiment_id %in% c(scenarios)) %>%
  # filter to models that have all the non hist-nat scenarios we want 
  filter(setdiff(c("historical", 'ssp126', "ssp245", 'ssp370'), 
                 unique(experiment_id)) %>% 
           length() %>% 
           equals(0), 
         .by = source_id) %>% 
  # filter to models with TCR in the expected range (8 of the source_id at this point aren't in the TCR list, but lets ignore them)
  filter(source_id %in% (gcm_tcr %>% filter(TCR  >= tcr_range[1] & TCR <= tcr_range[2]) %>% pull(Model))) #%>% 
  # pull(source_id) %>% n_distinct # this leaves us with 22 models
  # lets drop some variable that aren't needed to uniquely identify our dataset anymoore
  # unique %>% nrow # 921
  # select(-zstore, -table_id, -variable_id, -grid_label, -dcpp_init_year, 
  #        -version, -activity_id, -institution_id) 
  # unique %>% nrow # still 921

# lets define an ordering for the member_id/variants to so we can default to taking the same r1i1p1f1
variant_order = cat_subset %>% 
  select(member_id) %>% 
  unique %>% 
  separate(member_id, 
           into = c(NA, "r", "i", "p", "f"), 
           sep = c("r|i|p|f")) %>% 
  mutate(across(everything(), as.numeric)) %>% 
  as.list %>% 
  lapply(function(x) range(x)[1]:range(x)[2]) %>% 
  expand.grid() %>% 
  mutate(member_id = paste0("r", r, 
                            "i", i, 
                            "p", p,
                            "f", f)) %>% 
  pull(member_id) %>% 
  # limit to variants actually in the data set
  intersect(cat_subset$member_id)

# now, for each model and future/hist-nat scenario, we want a historical scenario with the same variant
cat_subset %<>% mutate(member_id = factor(member_id, levels = variant_order, ordered = T)) %>% 
  mutate(n_scenario = n_distinct(experiment_id), 
         .by = c(source_id)) %>% 
  mutate(all_future = setdiff(c("ssp126", "ssp245", "ssp370"), 
                              unique(experiment_id)) %>% 
           length %>% 
           equals(0), 
         has_hist_nat = "hist-nat" %in% unique(experiment_id), 
         has_hist = "historical" %in% unique(experiment_id), 
         n_scenario_variant = n_distinct(experiment_id),
         all_model_scenarios = n_scenario_variant == n_scenario,
         hist_nat_possible = has_hist_nat & has_hist, 
         all_future_possible = all_future & has_hist,
         .by = c(source_id, member_id)) %>% 
  arrange(source_id, member_id, experiment_id) 



# cat_subset %>% 
#   mutate(all = member_id == min(member_id[all_model_scenarios]), 
#          .by = source_id) %>% 
#   View
#   
# now for each source_id, if there's any member_ids that work for all scenarios, use the first one (order determined by variant order above)
cat_use <- cat_subset %>% 
  mutate(use = case_when(member_id == min(member_id[all_model_scenarios], na.rm = T) ~ "all", 
                         # if there's none that works for all scenarios, try to get one that works for all future and one that works for hist-nat
                         all(!all_model_scenarios) &
                           member_id == min(member_id[all_future_possible]) ~ "future", 
                         all(!all_model_scenarios) &
                           member_id == min(member_id[hist_nat_possible]) ~ "historical", 
                         T ~ NA),
         .by = c(source_id)) %>% 
  filter(!is.na(use)) %>% 
  # when we have separate variants for future and historical, make sure you only keep the relevant experiments for a given variant 
  filter(use == "all" | 
           (use == "future" & experiment_id != "hist-nat") | 
           (use == "historical" & !(experiment_id %in% c("ssp126", "ssp245", "ssp370")))) %>% 
  # only keep the relevant variables
  select(all_of(colnames(catalog)), use)

# double check this will join nicely with the full catalog
catalog %>% 
  left_join(cat_use) %>% 
  filter(!is.na(use)) %>% 
  View
# looks good, so lets save that catalog with the GCMs to use

write.csv(cat_use, 
          "./data/GCM_variant_scenarios_to_include.csv", 
          row.names = FALSE)

# SCRATCH 



# models and variants  
# 1) ACCESS-CM2 -- r1i1p1f1
# 2) ACCESS-ESM1-5  -- r33i1p1f1 for future, r1i1p1f1 for hist-nat 
# 3) AWI-CM-1-1-MR -- r1i1p1f1
# 4) BCC-CSM2-MR -- r1i1p1f1
# 5) CAMS-CSM1-0 -- r1i1p1f1
# 6) CESM2 -- r1i1p1f1 for hist-nat, r4i1p1f1 for future
# 7) CESM2-WACCM -- r1i1p1f1
# 8) CNRM-CM6-1 -- r1i1p1f2
# 9) CNRM-ESM2-1 -- r1i1p1f2
# 10) FGOALS-f3-L -- r1i1p1f1
# 11) GFDL-ESM4 -- r1i1p1f1
# 12) GISS-E2-1-G -- r1i1p3f1 for future, 


  
# what uniquely identifies a row in the catalog? 
# catalog %>%
#   select(activity_id, source_id, experiment_id, member_id, table_id, member_id) %>% 
#   unique %>% nrow
# ones previously missing TCR, when not considering 1pctC02 and piControl needed
# CanESM5-CanOE"    "FGOALS-g3"        "NorESM2-MM"       "EC-Earth3-Veg-LR" "CMCC-CM2-SR5"     "TaiESM1"         
# [7] "CAS-ESM2-0"       "CMCC-ESM2"    

# current ones missing TCR when those scenarios are considered (i.e., lost EC-Earth3-Veg-LR)
# "CanESM5-CanOE" "FGOALS-g3"     "NorESM2-MM"    "TaiESM1"       "CMCC-CM2-SR5"  "CAS-ESM2-0"    "CMCC-ESM2" 



