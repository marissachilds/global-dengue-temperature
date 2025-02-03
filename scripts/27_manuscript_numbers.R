library(tidyverse)
library(magrittr)

# output file 
out_txt_file <- file.path(".", "output", "manuscript_numbers.txt")

# intro, admin units and time ranges ---- 
write("introduction, number of administrative units ---- ", 
      out_txt_file)
readRDS("./data/unit_covariates.rds") %>% 
  filter(mid_year == max(mid_year), .by = country) %>% 
  filter(country != "LKA1") %>% 
  filter(sub_country_dengue > 0) %>% 
  nrow %>% 
  write(out_txt_file, 
        append = T)

# results, thermal sensitivity of dengue, temperature of peak ----
write("\n\nresults, temperature of dengue peak in main model (also in abstract) ----------", 
      out_txt_file, append = T)
main_est <- coef(readRDS("./output/mod_ests/all_models.rds")$main)
boot_ests <- readRDS("./output/mod_ests/main_coef_blockboot1000.rds") %>% 
  rbind(main_est) %>%
  select(contains("temp"))
temp_seq <- seq(0, 40, 0.01)
temp_resp_mat <- colnames(boot_ests) %>%
  grep("temp", ., value = TRUE) %>% 
  gsub(".*degree|_lag.", "", .) %>% 
  as.numeric %>%
  purrr::map(function(deg){
    temp_seq^deg
  }) %>% 
  reduce(cbind) 
boot_resp <- as.matrix(boot_ests) %*% t(temp_resp_mat) %>% 
  set_colnames(paste0("temp_", temp_seq)) %>% 
  as.data.frame() %>% 
  mutate(boot_id = c(1:(n()-1), "main")) %>% 
  pivot_longer(!boot_id, names_prefix = "temp_", 
               names_transform = as.numeric,
               names_to = "x", values_to = "y")
boot_resp %>% 
  filter(x > 10 & x < 35) %>% 
  filter(y == max(y), 
         .by = boot_id) %>% 
  summarise(mid = x[boot_id == "main"], 
            lower = quantile(x[boot_id != "main"], 0.025), 
            upper = quantile(x[boot_id != "main"], 0.975)) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 
rm(boot_resp, temp_resp_mat, temp_seq, boot_ests)

# results, approx change from +1 C in 15-20 C range ----- 
write("\n\nresults, approx effect of +1 C at low temperatures ----------", 
      out_txt_file, 
      append = T)
mod_ests <- readRDS("./output/mod_ests/all_models.rds")
source("./scripts/00_utilities/functions.R")

response_est_se(mod_ests$main, "temp", 15:21, vcov_type = NA, debug = FALSE) %>% 
  select(-se) %>% 
  mutate(y2 = lead(y, 1), 
         y_diff = y2 - y, 
         pct_of_orig = exp(y2 - y)*100) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 
rm(mod_ests)

# results, % of units that are in Brazil ----
write("\n\nresults, % of spatial units in Brazil ----------", 
      out_txt_file, 
      append = T)
readRDS("./data/unit_covariates.rds") %>% 
  filter(sub_country_dengue > 0) %>% 
  mutate(in_brazil = country == "BRA") %>% 
  summarise(n = n(), 
            .by = in_brazil) %>% 
  mutate(pct = n/sum(n)) %>% 
  filter(in_brazil == TRUE) %>% 
  pull(pct) %>% 
  write(out_txt_file, 
        append = T)

# results, average temperature change due to historical climate change ----
write("\n\nresults, average temperature change from historical climate change ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/unit_changes_maxBoot100_mod_main_scenario_hist-nat.rds") %>% 
  mutate(id_join = case_when(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX") ~ stringi::stri_trans_general(id, id = "Latin-ASCII"), 
                             country == "BRA" ~ str_sub(id, 1, 8), 
                             country == "IDN" ~ case_when(id == "Dki Jakarta" ~ "Jakarta", 
                                                          id == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta", 
                                                          # kalimantan utara gets assigned the dengue cases from timur, which is okay since we only care about whether dengue is > 0
                                                          id == "Kalimantan Utara" ~ "Kalimantan Timur", 
                                                          T ~ id), 
                             T ~ id) %>% tolower) %>% 
  filter(id_join != "br430000" & id != "[unknown]") %>% 
  left_join(readRDS("./data/unit_covariates.rds") %>% filter(mid_year == max(mid_year), .by = country), 
            by = c("id_join" = "id", "country")) %>% 
  filter(empirical_dengue_incidence > 0) %>% 
  pull(dTemp_mean) %>% 
  mean() %>% 
  multiply_by(-1) %>%
  write(out_txt_file, 
        append = T)
  

# results, overall % of existing dengue burden due to climate change ---- 
write("\n\nresults, overall % of existing dengue burden due to climate change ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_hist-nat.rds") %>% 
  filter(country == "overall_pop_weight") %>% 
  select(pct_inc_dengue_change_q_0.025, pct_inc_dengue_change_mean, pct_inc_dengue_change_q_0.975) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 

# results, historical impacts in cases ---- 
write("\n\nresults, annual cases due to historical climate change ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_hist-nat.rds") %>% 
  filter(country == "overall_pop_weight") %>% 
  select(contains("case")) %>% 
  select(contains(c("mean", "q_0.025", "q_0.975"))) %>% 
  rename_with(~gsub("q_", "", .x)) %>% 
  pivot_longer(everything()) %>% 
  separate(name, sep="_(?=[^_]+$)", into = c("metric", "quant"))%>% 
  pivot_wider(names_from = quant) %>% 
  # divide to get in millions of cases
  mutate(across(!metric, ~.x/1e6)) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE)

# results, overall % change in dengue under SSP3-7.0 projections ---- 
write("\n\nresults, overall % change in dengue under SSP3-7.0 projections ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp370.rds") %>% 
  filter(country == "overall_pop_weight") %>% 
  select(pct_inc_dengue_change_q_0.025, pct_inc_dengue_change_mean, pct_inc_dengue_change_q_0.975) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 


# results, overall % change in dengue under SSP1-2.6 projections ---- 
write("\n\nresults, overall % change in dengue under SSP1-2.6 projections ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp126.rds") %>% 
  filter(country == "overall_pop_weight") %>% 
  select(pct_inc_dengue_change_q_0.025, pct_inc_dengue_change_mean, pct_inc_dengue_change_q_0.975) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 

# results, % change in dengue in Bolivia under SSP3-7.0 projections
write("\n\nresults, Bolivia % change in dengue under SSP3-7.0 projections ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp370.rds") %>% 
  filter(country == "BOL") %>% 
  select(pct_inc_dengue_change_q_0.025, pct_inc_dengue_change_mean, pct_inc_dengue_change_q_0.975) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 

# results, number of people in areas with doubling dengue ----
write("\n\nresults, number of people (in millions) currently living in places expected to see doubling dengue ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/unit_changes_maxBoot100_mod_main_scenario_ssp370.rds") %>% 
  mutate(id_join = case_when(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX") ~ stringi::stri_trans_general(id, id = "Latin-ASCII"), 
                             country == "BRA" ~ str_sub(id, 1, 8), 
                             country == "IDN" ~ case_when(id == "Dki Jakarta" ~ "Jakarta", 
                                                          id == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta", 
                                                          # kalimantan utara gets assigned the dengue cases from timur, which is okay since we only care about whether dengue is > 0
                                                          id == "Kalimantan Utara" ~ "Kalimantan Timur", 
                                                          T ~ id), 
                             T ~ id) %>% tolower) %>% 
  filter(id_join != "br430000" & id != "[unknown]") %>% 
  full_join(readRDS("./data/unit_covariates.rds") %>% 
              filter(mid_year == max(mid_year), 
                     .by = country),
            by = c("country", "id_join" = "id")) %>% 
  full_join(readRDS("./data/all_pop2015.rds") %>% rename(pop2015 = sum)) %>% 
  filter(empirical_dengue_incidence > 0) %>% 
  filter(pct_change_dengue_mean > 1) %>% 
  pull(pop2015) %>% 
  sum %>% 
  divide_by(1e6) %>% 
  write(out_txt_file, 
        append = T)

# results, which countries see increses/decreases under the 3 projections? ----
purrr::map(c("ssp126", "ssp245", "ssp370"), function(x){
  readRDS(paste0("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_", x, ".rds")) %>% 
    mutate(scenario = x)
}) %>% 
  list_rbind() %>% 
  transmute(country, scenario, mid = pct_inc_dengue_change_mean, 
            sig = sign(pct_inc_dengue_change_q_0.025) == sign(pct_inc_dengue_change_q_0.975)) %>% 
  pivot_wider(names_from = scenario, names_glue = "{scenario}_{.value}", values_from = c(mid, sig),
              names_vary = "fastest") %>% 
  filter(!grepl("overall", country)) -> country_proj_class

write("\n\nresults, countries with decreases in dengue across all scenarios ----------", 
      out_txt_file, 
      append = T)
country_proj_class %>% 
  filter(if_all(ends_with("mid"), ~. < 0) & 
           if_all(ends_with("sig"), ~.)) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 

write("\n\nresults, countries with increases in dengue across all scenarios ----------", 
      out_txt_file, 
      append = T)
country_proj_class %>% 
  filter(if_all(ends_with("mid"), ~. > 0) & 
           if_all(ends_with("sig"), ~.)) %>%
  mutate(n = 1:n()) %>% 
  select(n, everything()) %>%
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 

# write("\n\nresults, countries with not consistent/not sig dengue changes across scenarios ----------", 
#       out_txt_file, 
#       append = T)
# country_proj_class %>% 
#   filter(!(if_all(ends_with("mid"), ~. > 0) | if_all(ends_with("mid"), ~. < 0)) |
#            !if_all(ends_with("sig"), ~.)) %>% 
#   write.table(out_txt_file,
#               append = TRUE, row.names = FALSE) 
rm(county_proj_class)

# results, future in cases ---- 
write("\n\nresults, annual cases projected under future scenarios ----------", 
      out_txt_file, 
      append = T)
rbind(readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp126.rds") %>% 
        filter(country == "overall_pop_weight") %>% 
        mutate(scenario = "ssp126"),
      readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp370.rds") %>% 
        filter(country == "overall_pop_weight") %>% 
        mutate(scenario = "ssp370")) %>% 
  select(scenario, contains("case")) %>% 
  select(scenario, contains(c("mean", "q_0.025", "q_0.975"))) %>% 
  rename_with(~gsub("q_", "", .x)) %>% 
  pivot_longer(!scenario) %>% 
  separate(name, sep="_(?=[^_]+$)", into = c("metric", "quant"))%>% 
  pivot_wider(names_from = quant) %>% 
  # divide to get in millions of cases
  mutate(across(!c(metric, scenario), ~.x/1e6)) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE)

# results, overall additional increase from ssp1-2.6 to ssp3-7.0   
write("\n\nresults, overall change in % change dengue for SSP3-7.0 relative to SSP1-2.6 ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp370_minus_ssp126.rds") %>% 
  filter(country == "overall_pop_weight") %>% 
  select(abs_change_mean, abs_change_q_0.025, abs_change_q_0.975) %>% 
  write.table(out_txt_file,
              append = TRUE, row.names = FALSE) 

# results, largest increase from ssp1-2.6 to ssp3-7.0 
write("\n\nresults, largest % change in dengue for SSP3-7.0 relative to SSP1-2.6 ----------", 
      out_txt_file, 
      append = T)
readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp370_minus_ssp126.rds") %>% 
  pull(abs_change_mean) %>% 
  write(out_txt_file,
        append = TRUE) 
