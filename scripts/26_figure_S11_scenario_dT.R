library(magrittr)
library(tidyverse)

unit_covar <- readRDS("./data/unit_covariates.rds") 
list.files("./data/dT_combined",
           full.names = T,
           pattern = "current") %>%
  readRDS() %>% 
  filter(year >= 1995) %>% 
  summarise(avg_temp = mean(mean_2m_air_temperature_degree1), 
            .by = c(country, id)) -> unit_avg_temp

pop <- readRDS("./data/all_pop2015.rds") %>% 
  filter(country != "CHN")
country_temps <- unit_avg_temp %>% 
  left_join(pop) %>% 
  summarise(mean_temp = weighted.mean(avg_temp, sum, na.rm = T), 
            .by = country) %>% 
  arrange(mean_temp) %>% 
  mutate(country_factor = factor(country, ordered = T, 
                                 levels = unique(country)))

test <- purrr::map(c("hist-nat", "ssp126", "ssp245", "ssp370"), 
                   function(x){
                     readRDS(paste0("./output/projection_ests/unit_changes_maxBoot100_mod_main_scenario_", x, ".rds")) %>% 
                       mutate(scenario = x)}) %>% 
  list_rbind() %>% 
  full_join(unit_avg_temp) %>% 
  mutate(id_join = case_when(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX") ~ stringi::stri_trans_general(id, id = "Latin-ASCII"), 
                             country == "BRA" ~ str_sub(id, 1, 8), 
                             country == "IDN" ~ case_when(id == "Dki Jakarta" ~ "Jakarta", 
                                                          id == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta", 
                                                          # kalimantan utara gets assigned the dengue cases from timur, which is okay since we only care about whether dengue is > 0
                                                          id == "Kalimantan Utara" ~ "Kalimantan Timur", 
                                                          T ~ id), 
                             T ~ id) %>% tolower) %>% 
  filter(id_join != "br430000" & id != "[unknown]") %>% 
  left_join(unit_covar %>% filter(mid_year == max(mid_year), .by = country), 
            by = c("id_join" = "id", "country")) %>% 
  left_join(country_temps) %>% 
  filter(empirical_dengue_incidence > 0) %>% 
  mutate(scenario_mean = mean(dTemp_mean), .by = scenario) %>% 
  mutate(scenario = gsub("ssp(\\d)(\\d)(\\d)", "SSP\\1-\\2\\.\\3", scenario) %>% 
           gsub("hist", "historical", .) %>% 
           gsub("nat", "natural", .), 
         scenario = factor(scenario, levels = unique(scenario), ordered = T))

test %>% 
  {ggplot(., aes(x = avg_temp, 
                 y = dTemp_mean)) + 
      # hack way to get ylims same for future scenarios but different for post
      geom_point(data = summarise(.,
                                  ymin = min(dTemp_q_0.025),
                                  ymax = min(dTemp_q_0.975),
                                  .by = c(scenario, scenario_mean)) %>%
                   mutate(past_future = grepl("historical", scenario)) %>%
                   mutate(ymin = min(ymin),
                          ymax = max(ymax),
                          .by = past_future) %>%
                   pivot_longer(starts_with("y")),
                 aes(x = -Inf, y = value),
                 alpha = 0) +
      geom_point(alpha = 0.05, shape = 16) + 
      geom_linerange(aes(ymax = dTemp_q_0.975, ymin = dTemp_q_0.025), 
                     alpha = 0.025) + 
      geom_hline(data = select(., scenario, scenario_mean) %>% unique,
                 aes(yintercept = scenario_mean), 
                 color = "red", linetype = "dashed") +
      facet_wrap(~paste0(scenario, 
                         ifelse(grepl("hist", scenario), " (1995 - 2014)", " (2040-2059)"),
                         "\nΔT = ", round(scenario_mean, 2), "°C"), 
                 scales = "free") + 
      xlab("Observed average temperature (°C, 1995 - 2014)") + 
      ylab("ΔT (°C)") + 
      theme_classic() + 
      theme(strip.background = element_blank(), 
            strip.text = element_text(face = "bold"))} %>% 
  ggsave(filename = "./figures/figureSX_dT_scenario.png", 
         width = 7, height = 5)
