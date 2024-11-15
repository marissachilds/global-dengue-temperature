library(tidyverse)
library(magrittr)
library(cowplot)
library(fixest)

# dengue last year (rolling avg over previous 7-18 months?) x temperature ----

unit_covar <- readRDS("./data/unit_covariates.rds") %>% 
  mutate(GBD_rescale = sub_country_dengue/empirical_dengue_incidence)
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

dengue_temp %<>% left_join(unit_covar %>% select(country, id, mid_year, GBD_rescale))

# add lags, calculate dengue incidence, make some FEs from others
dengue_temp %<>% arrange(country, mid_year, id, date) %>% 
  mutate(across(union(contains("temp"), contains("precipitation")), 
                list(lag1 =~ lag(.x, 1),
                     lag2 =~ lag(.x, 2),
                     lag3 =~ lag(.x, 3),
                     lag4 =~ lag(.x, 4))), 
         .by = c(country, mid_year, id)) %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(country, "_", id)) %>% 
  mutate(dengue_lag = lag(dengue_inc, 7) + lag(dengue_inc, 8) + 
           lag(dengue_inc, 9) + lag(dengue_inc, 10) + 
           lag(dengue_inc, 11) + lag(dengue_inc, 12) + 
           lag(dengue_inc, 13) + lag(dengue_inc, 14) + 
           lag(dengue_inc, 15) + lag(dengue_inc, 16) + 
           lag(dengue_inc, 17) + lag(dengue_inc, 18), 
         dengue_lag = dengue_lag*GBD_rescale,
         .by = c(country, mid_year, id)) %>% 
  filter(!is.na(dengue_inc))

# where should the tercile breaks be? 
dengue_temp %>% 
  # filter(dengue_lag > 0) %>%
  pull(dengue_lag) %>% 
  quantile(c(0, 1/4, 2/4, 3/4, 1), na.rm = T) -> dengue_breaks

dengue_temp %<>% 
  mutate(dengue_lag_tercile = case_when(is.na(dengue_lag) ~ NA, 
                                        dengue_lag <= dengue_breaks[2] ~ "1", 
                                        dengue_lag < dengue_breaks[3] ~ "2", 
                                        dengue_lag < dengue_breaks[4] ~ "3",
                                        dengue_lag <= dengue_breaks[5] ~ "4"))

# other units are missing subcountry dengue if they have all NAs for dengue, but they get dropped in line 22 above
het_ests <- c("dengue_lag_tercile") %>% 
  purrr::map(function(x){
    fml <- paste0("dengue_inc ~ ", 
                  x, ":mean_2m_air_temp_degree1_lag1 + ", 
                  x, ":mean_2m_air_temp_degree2_lag1 + ", 
                  x, ":mean_2m_air_temp_degree3_lag1 + ", 
                  x, ":mean_2m_air_temp_degree1_lag2 + ", 
                  x, ":mean_2m_air_temp_degree2_lag2 + ", 
                  x, ":mean_2m_air_temp_degree3_lag2 + ", 
                  x, ":mean_2m_air_temp_degree1_lag3 + ", 
                  x, ":mean_2m_air_temp_degree2_lag3 + ", 
                  x, ":mean_2m_air_temp_degree3_lag3 + ", 
                  "total_precipitation_lag1 + total_precipitation_lag2 + total_precipitation_lag3 | countryFE^id + countryFE^year + countryFE^month")
    print(fml)
    
    fixest::fepois(as.formula(fml),
                   weights =~pop, # population weight
                   data = dengue_temp %>% 
                     filter(!is.na(dengue_lag))) %>% 
      return
  })
temp_seq = round(seq(0, 50, by = 0.001), 3)
het_marginals <- list(list(coef = coef(het_ests[[1]]), 
                           vcov = vcov(het_ests[[1]], se = "cluster"))) %>% 
  purrr::imap(function(x, name){
    terciles <- names(x$coef) %>% 
      str_split_i("\\:", i = 1) %>%
      unique %>% 
      grep(pattern = "tercile", value = T) 
    print(terciles)
    purrr::map(terciles, function(terc){
      marginal_est_se(coef_name_regex = terc, x_seq = temp_seq, 
                      debug = T, vcov_mat = x$vcov, coef_vec = x$coef,
                      degree_regex = "_degree")  %>% 
        mutate(tercile = terc) %>% 
        return
    }) %>% 
      list_rbind() %>%
      mutate(mod = name) %>% 
      return
  }) %>% 
  list_rbind() 

# library(data.table)
clim_obs <- readRDS("data/dT_combined/scenarios_era_current_current.gz") %>% 
  rename_with(function(x){gsub("temperature", "temp", x)}, contains("temperature")) %>%
  select(-ends_with("degree5"), -ends_with("degree4")) 
clim_obs %<>% 
  mutate(id_join = case_when(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX") ~ stringi::stri_trans_general(id, id = "Latin-ASCII"), 
                             country == "BRA" ~ str_sub(id, 1, 8), 
                             country == "IDN" ~ case_when(id == "Dki Jakarta" ~ "Jakarta", 
                                                          id == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta", 
                                                          # kalimantan utara gets assigned the dengue cases from timur, which is okay since we only care about whether dengue is > 0
                                                          id == "Kalimantan Utara" ~ "Kalimantan Timur", 
                                                          T ~ id), 
                             T ~ id) %>% tolower) %>% 
  filter(id_join != "br430000" & id != "[unknown]") %>% 
  left_join(unit_covar %>% 
              filter(mid_year == max(mid_year), 
                     .by = country) %>% 
              select(country, id, empirical_dengue_incidence), 
            by = c("country", "id_join" = "id")) %>% 
  filter(!is.na(empirical_dengue_incidence))  

immunity_colors = c("#EEB32F", "#00B7A7", "#4E9626", "#AD5585")

# mutate(tercile = case_match(tercile, 
#                             "tercile1" ~ "low incidence/\nhigh susceptibility", 
#                             "tercile4" ~ "high incidence/\nlow susceptibility")) %>%
#             theme_classic() + 
#             theme(legend.position = "inside", 
#                   legend.title = element_blank(),
#                   legend.position.inside = c(0.775, 0.8)), 

dengue_temp %>% 
  mutate(tercile = paste0("quantile ", as.character(dengue_lag_tercile)),
         tercile = case_when(grepl("1", tercile) ~ paste0(tercile, " (low)"), 
                             grepl("4", tercile) ~ paste0(tercile, " (high)"), 
                             T ~ tercile)) %>% 
  ggplot(aes(x = dengue_lag/12, fill = tercile)) + 
  geom_histogram(bins = 50) + 
  scale_x_continuous(trans = "pseudo_log", breaks = c(0, 1, 10, 100, 1000, 1e4, 1e5)) + 
  scale_fill_manual(name = "immunity proxy",
                    values = immunity_colors,
                    # MetBrewer::met.brewer("Austria", 14)[c(8, 11, 14)],
                    na.translate = F) + 
  theme_classic() + 
  xlab("average lagged dengue (7-18 months)") + 
  theme(legend.position = "inside", 
        legend.position.inside = c(0.65, 0.75)) -> immunity_hist

set.seed(1001)
dengue_temp %>% 
  # only consider units without missing obs 
  filter(!any(is.na(dengue_inc)) & 
           n() == length(seq.Date(min(date), max(date), by = "month")), 
         .by = c(countryFE, id)) %>% 
  select(countryFE, id, pop) %>% 
  unique %>% 
  slice_sample(n = 4, weight_by = pop) %>% 
  left_join(dengue_temp) %>% 
  mutate(panel_lab = paste0(str_to_title(id), ", ",
                            case_when(country == "BRA" ~ "Brazil", 
                                      country == "IDN" ~ "Indonesia", 
                                      country == "MYS" ~ "Malaysia",
                                      country == "MEX" ~ "Mexico", 
                                      country == "COL" ~ "Colombia",
                                      country == "PHL" ~ "Philippines",
                                      country == "THA" ~ "Thailand"))) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = dengue_lag/GBD_rescale*1/12, 
                color = dengue_lag_tercile, group = id),
            lwd = 1.2) + 
  geom_line(aes(y = dengue_inc)) + 
  facet_wrap(~panel_lab,
             # paste0(countryFE, ": ", id),
             scales = "free") + 
  scale_color_manual(name = "immunity proxy",
                     values = immunity_colors,
                     # MetBrewer::met.brewer("Austria", 14)[c(8, 11, 14)],
                     na.translate = F) + 
  theme_classic() + ylab("dengue incidence") + 
  theme(axis.title.x = element_blank(),
        legend.position = "inside", 
        legend.position.inside = c(0.65, 0.8)) -> immunity_ts

yoff = 0.74  
het_marginals %>% 
  filter(x > 11 & x < 31) %>% 
  mutate(tercile = gsub("tercile", "quantile ", gsub("dengue_lag_", "", tercile)), 
         tercile = case_when(grepl("1", tercile) ~ paste0(tercile, " (low)"), 
                             grepl("4", tercile) ~ paste0(tercile, " (high)"), 
                             T ~ tercile)) %>%
  # filter(grepl("1|4", tercile)) %>%
  # ggplot(aes(x = x, y = y, ymin = y - 1.96*se, ymax = y + 1.96*se, 
  #            group = tercile, color = tercile, fill = tercile)) + 
  ggplot(aes(x = x, y = y + yoff, ymin = pmax(y - 1.96*se, -0.76) + yoff, ymax = y + 1.96*se + yoff, 
             group = tercile, color = tercile, fill = tercile)) + 
  geom_hline(yintercept = 0 + yoff) + 
  geom_ribbon(alpha = 0.4, color = NA) + 
  geom_histogram(data = clim_obs %>% filter(empirical_dengue_incidence > 0), 
                 aes(x = mean_2m_air_temp_degree1, 
                     y = 4*after_stat(density)), 
                 alpha = 0.5, color = "white",
                 inherit.aes = FALSE) + 
  geom_line(lwd = 1.3) + 
  scale_fill_manual(name = "immunity proxy",
                    values = immunity_colors,
                    # MetBrewer::met.brewer("Austria", 14)[c(8, 11, 14)],
                    aesthetics = c("color", "fill"),
                    na.translate = F) + 
  xlab("temperature (C)") + ylab("d log(dengue)/d temp") + 
  scale_x_continuous(expand = expansion(mult = 0.02), 
                     limits = c(11, 31)) + 
  scale_y_continuous(breaks = seq(-0.5, 1.5, by = 0.5) + yoff, 
                     labels = function(x){x - yoff}, 
                     expand = expansion(mult = 0),
                     limits = c(-0.76 + yoff, 1.35 + yoff)) +
  theme_classic() + 
  theme(legend.position = "inside", 
        legend.position.inside = c(0.77, 0.85)) -> immunity_response
clim_obs %>% 
  filter(empirical_dengue_incidence > 0) %>% 
  select(id, country, year, month, x = mean_2m_air_temp_degree1) %>% 
  mutate(x = round(x, 3)) %>% 
  left_join(het_marginals %>% mutate(x = round(x, 3))) %>% 
  filter(grepl("1|4", tercile)) %>% 
  {ggplot(data = ., aes(y = y)) + 
      geom_histogram(aes(group = tercile, fill = tercile, 
                         x = after_stat(density)), 
                     color = "white",
                     position = "identity",
                     alpha = 0.5) + 
      geom_hline(yintercept = 0) + 
      geom_hline(data = summarise(., 
                                  mean_marginal = mean(y), 
                                  .by = tercile), 
                 aes(yintercept = mean_marginal, 
                     color = tercile), 
                 linewidth = 1.2) + 
      geom_text(data = summarise(., 
                                 mean_marginal = mean(y), 
                                 .by = tercile), 
                aes(x = 1.3, 
                    y = mean_marginal + ifelse(grepl("1", tercile), 0.05, -0.05),
                    label = round(mean_marginal, 2),
                    color = tercile)) + 
      scale_color_manual(values = immunity_colors[c(1,4)],
                         aesthetics = c("color", "fill")) + 
      theme_classic() + 
      scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) + 
      scale_y_continuous(limits = c(-0.76, 1.35), 
                         breaks = seq(-0.5, 1.5, by = 0.5),
                         expand = expansion(mult = 0)) + 
      ylab("d log(dengue)/d temp") +  
      theme(legend.position = "none")} -> immunity_marginal_dist

# clim_obs %>% 
#   filter(empirical_dengue_incidence > 0) %>% 
#   select(id, country, year, month, x = mean_2m_air_temp_degree1) %>% 
#   mutate(x = round(x, 3)) %>% 
#   left_join(het_marginals %>% mutate(x = round(x, 3))) %>% 
#   filter(grepl("1|4", tercile)) %>%
#   summarise(., 
#             mean_marginal = mean(y), 
#             .by = tercile)

library(cowplot)
plot_grid(immunity_hist + 
            theme(plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points")), 
          immunity_ts + 
            theme(legend.position = "none", strip.background = element_blank(), 
                  plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points")), 
          nrow = 2, rel_heights = c(0.6, 1), 
          labels = c("a) distribution of lagged dengue incidence", 
                     "b) varition in immunity proxy within locations"), 
          hjust = 0, label_x = 0.01, vjust = c(1.2, 1.5)) %>% 
  ggsave(filename = "figures/immunity_quantiles.png", 
         width = 6, height = 8)

plot_grid(immunity_response + 
            theme(plot.margin = unit(c(37.5, 5.5, 5.5, 5.5), "points"),
                  legend.key.size = unit(0.9, "lines"),
                  legend.background = element_blank()), 
          immunity_marginal_dist + 
            theme(plot.margin = unit(c(37.5, 5.5, 5.5, 5.5), "points")), 
          align = "v", axis = "tb",
          nrow = 1, 
          rel_widths = c(1, 0.7),
          labels = c("a) marginal effect by immunity quantiles", 
                     "b) distribution of marginal effects\n    over observed temperatures"), 
          label_size = 13, 
          vjust = c(1.5, 1.2),
          hjust = 0, label_x = 0.03) %>% 
  ggsave(filename = "figures/immunity_responses.png", 
         width = 8, height = 4)