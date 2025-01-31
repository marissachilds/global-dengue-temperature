library(tidyverse)
library(magrittr)
library(cowplot)
library(fixest)

source("./scripts/00_utilities/functions.R")

immunity_colors = c("#EEB32F", "#00B7A7", "#4E9626", "#AD5585")

unit_covar <- readRDS("./data/unit_covariates.rds") %>% 
  mutate(GBD_rescale = sub_country_dengue/empirical_dengue_incidence)
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

dengue_temp %<>% left_join(unit_covar %>% select(country, id, mid_year, empirical_dengue_incidence, GBD_rescale))

# add lags, calculate dengue incidence, make some FEs from others
dengue_temp %<>% prep_dengue_data() %>% 
  mutate(dengue_lag_7_18 = slider::slide(dengue_inc, mean, .before = 6 + 12, .after = -7) %>% unlist,
         dengue_lag_7_42 = slider::slide(dengue_inc, mean, .before = 6 + 36, .after = -7) %>% unlist,
         dengue_lag_1_12 = slider::slide(dengue_inc, mean, .before = 12, .after = -1) %>% unlist,
         dengue_lag_1_36 = slider::slide(dengue_inc, mean, .before = 36, .after = -1) %>% unlist,
         across(starts_with("dengue_lag"), ~.x*GBD_rescale),
         .by = c(country, mid_year, id)) %>% 
  filter(!is.na(dengue_inc))

# where should the tercile breaks be? 
# define breaks as 0, and then 1/3s after that 
dengue_temp %>% colnames %>% grep("^dengue_lag_",., value = T) %>% 
  {set_names(map(., function(x){
    dengue_temp %>%
      filter(empirical_dengue_incidence > 0) %>%
      rename(temp := x) %>% 
      filter(temp > 0) %>%
      pull(temp) %>%
      quantile(c(1/3, 2/3, 1), na.rm = T)
  }), .)} -> dengue_breaks

dengue_temp %<>% 
  mutate(dengue_lag_7_18_tercile = case_when(is.na(dengue_lag_7_18) ~ NA, 
                                        dengue_lag_7_18 <= 0 ~ "1", 
                                        dengue_lag_7_18 < dengue_breaks$dengue_lag_7_18[1] ~ "2", 
                                        dengue_lag_7_18 < dengue_breaks$dengue_lag_7_18[2] ~ "3",
                                        dengue_lag_7_18 <= dengue_breaks$dengue_lag_7_18[3] ~ "4"), 
         dengue_lag_7_42_tercile = case_when(is.na(dengue_lag_7_42) ~ NA, 
                                         dengue_lag_7_42 <= 0 ~ "1", 
                                         dengue_lag_7_42 < dengue_breaks$dengue_lag_7_42[1] ~ "2", 
                                         dengue_lag_7_42 < dengue_breaks$dengue_lag_7_42[2] ~ "3",
                                         dengue_lag_7_42 <= dengue_breaks$dengue_lag_7_42[3] ~ "4"), 
         dengue_lag_1_12_tercile = case_when(is.na(dengue_lag_1_12) ~ NA, 
                                         dengue_lag_1_12 <= 0 ~ "1", 
                                         dengue_lag_1_12 < dengue_breaks$dengue_lag_1_12[1] ~ "2", 
                                         dengue_lag_1_12 < dengue_breaks$dengue_lag_1_12[2] ~ "3",
                                         dengue_lag_1_12 <= dengue_breaks$dengue_lag_1_12[3] ~ "4"),
         dengue_lag_1_36_tercile = case_when(is.na(dengue_lag_1_36) ~ NA, 
                                             dengue_lag_1_36 <= 0 ~ "1", 
                                             dengue_lag_1_36 < dengue_breaks$dengue_lag_1_36[1] ~ "2", 
                                             dengue_lag_1_36 < dengue_breaks$dengue_lag_1_36[2] ~ "3",
                                             dengue_lag_1_36 <= dengue_breaks$dengue_lag_1_36[3] ~ "4"))

# other units are missing subcountry dengue if they have all NAs for dengue, but they get dropped in line 22 above
het_ests <- dengue_temp %>% colnames %>% grep("^dengue_lag_.*_tercile",., value = T) %>% 
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
                   data = dengue_temp) %>% 
      return
  })
temp_seq = round(seq(0, 50, by = 0.001), 3)
het_marginals <- purrr::imap(set_names(het_ests, dengue_temp %>% colnames %>% grep("^dengue_lag_.*_tercile",., value = T)), 
                             function(fixest_mod, mod_name){
  print(mod_name)
  mod_coef = coef(fixest_mod)
  mod_vcov = vcov(fixest_mod, se = "cluster")
  terciles <- names(mod_coef) %>% 
    str_split_i("\\:", i = 1) %>%
    unique %>% 
    grep(pattern = "tercile", value = T) 
  purrr::map(terciles, function(terc){
    print(terc)
    marginal_est_se(coef_name_regex = terc, x_seq = temp_seq, 
                    debug = F, vcov_mat = mod_vcov, coef_vec = mod_coef,
                    degree_regex = "_degree")  %>% 
      mutate(tercile = terc) %>% 
      return
  }) %>% 
    list_rbind() %>%
    mutate(mod = mod_name) %>% 
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

dengue_temp %>% 
  # filter(dengue_lag_7_18 > 0) %>% pull(dengue_lag_7_18) %>% min(na.rm = T)
  mutate(tercile = paste0("quantile ", as.character(dengue_lag_7_18_tercile)),
         tercile = case_when(grepl("1", tercile) ~ paste0(tercile, " (low)"), 
                             grepl("4", tercile) ~ paste0(tercile, " (high)"), 
                             T ~ tercile)) %>% 
  ggplot(aes(x = dengue_lag_7_18, fill = tercile)) + 
  geom_histogram(bins = 50, boundary = 0.0002117654, closed = "left") + 
  scale_x_continuous(trans = "pseudo_log", breaks = c(0, 1, 10, 100, 1000, 1e4, 1e5)) + 
  scale_fill_manual(name = "immunity proxy",
                    values = immunity_colors,
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
  mutate(dengue_lag_7_18_tercile = case_when(grepl("1", dengue_lag_7_18_tercile) ~ paste0(dengue_lag_7_18_tercile, " (low)"), 
                                             grepl("4", dengue_lag_7_18_tercile) ~ paste0(dengue_lag_7_18_tercile, " (high)"), 
                                             T ~ dengue_lag_7_18_tercile),
         dengue_lag_7_18_tercile = ifelse(is.na(dengue_lag_7_18_tercile), NA, 
                                          paste0("quantile ", dengue_lag_7_18_tercile))) %>% 
  mutate(panel_lab = paste0(str_to_title(id), ", ",
                            case_when(country == "BRA" ~ "Brazil", 
                                      country == "IDN" ~ "Indonesia", 
                                      country == "MYS" ~ "Malaysia",
                                      country == "MEX" ~ "Mexico", 
                                      country == "COL" ~ "Colombia",
                                      country == "PHL" ~ "Philippines",
                                      country == "THA" ~ "Thailand"))) %>% 
  ggplot(aes(x = date)) + 
  geom_line(aes(y = dengue_lag_7_18/GBD_rescale, 
                color = dengue_lag_7_18_tercile, group = id),
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

yoff = 1
plot_grid(het_marginals %>% 
            filter(x > 11 & x < 31) %>% 
            mutate(terc = paste0("quantile ", str_sub(tercile, -1, -1)),
                   terc = case_when(grepl("1", terc) ~ paste0(terc, " (low)"), 
                                       grepl("4", terc) ~ paste0(terc, " (high)"), 
                                       T ~ terc),
                   se = ifelse(terc %in% c("2", "3"), NA, se),
                   mod = gsub("^dengue_lag_|_tercile", "", mod), 
                   mod = gsub("_", " - ", mod), 
                   mod = paste0(mod, " months lagged dengue")) %>% 
            ggplot(aes(x = x, y = y + yoff, 
                       ymin = pmax(y - 1.96*se, -1) + yoff, 
                       ymax = pmin(y + 1.96*se, 1.4) + yoff,
                       group = interaction(terc, mod), 
                       color = terc, fill = terc)) + 
            geom_line(lwd = 1) + 
            geom_histogram(data = clim_obs %>% 
                             filter(empirical_dengue_incidence > 0) %>% 
                             mutate(mod = "7 - 42 months lagged dengue"),
                           aes(x = mean_2m_air_temp_degree1,
                               y = 4*after_stat(density)),
                           alpha = 0.5, color = "white",
                           inherit.aes = FALSE) +
            geom_hline(yintercept = 0 + yoff) + 
            geom_ribbon(alpha = 0.3, color = NA) +
            facet_wrap(~mod, ncol = 1) + 
            scale_color_manual(values = immunity_colors,
                               aesthetics = c("color", "fill")) + 
            scale_y_continuous(breaks = seq(-1, 1.5, by = 0.5) + yoff, 
                               labels = function(x){x - yoff}, 
                               expand = expansion(mult = 0)) +
            scale_x_continuous(expand = expansion(mult = 0), 
                               limits = c(11, 31)) + 
            xlab("temperature (°C)") + ylab("d log(dengue)/d temp") +  
            theme_classic() + 
            theme(plot.margin = unit(c(37.5, 5.5, 5.5, 5.5), "points"),
                  legend.position = "inside", 
                  legend.position.inside = c(0.22, 0.815),
                  legend.background = element_blank(),
                  legend.key.size = unit(0.8, "lines"),
                  legend.text = element_text(size = 7.5),
                  legend.title = element_blank(),
                  strip.background = element_blank()),
          clim_obs %>% 
            filter(empirical_dengue_incidence > 0) %>% 
            select(id, country, year, month, x = mean_2m_air_temp_degree1) %>% 
            mutate(x = round(x, 3)) %>% 
            left_join(het_marginals %>% mutate(x = round(x, 3))) %>% 
            mutate(terc = str_sub(tercile, -1, -1),
                   mod = gsub("^dengue_lag_|_tercile", "", mod), 
                   mod = gsub("_", " - ", mod), 
                   mod = paste0(mod, " months lagged dengue")) %>% 
            filter(grepl("1|4", terc)) %>%
            {ggplot(data = ., aes(y = y)) + 
                geom_histogram(aes(group = interaction(mod, terc), fill = terc, 
                                   x = after_stat(density)), 
                               color = "white",
                               position = "identity",
                               alpha = 0.5) + 
                geom_hline(yintercept = 0) + 
                geom_hline(data = summarise(., 
                                            mean_marginal = mean(y), 
                                            .by = c(terc, mod)), 
                           aes(yintercept = mean_marginal, 
                               color = terc), 
                           linewidth = 1.2) + 
                geom_text(data = summarise(., 
                                           mean_marginal = mean(y), 
                                           .by = c(terc, mod)) %>% 
                            mutate(mod_min = rank(mean_marginal) == 1, 
                                   .by = mod), 
                          aes(x = 1.85,  
                              y = mean_marginal + ifelse(mod_min, -0.1, 0.1),
                              label = round(mean_marginal, 2),
                              color = terc)) + 
                facet_wrap(~mod, ncol = 1) + 
                scale_color_manual(values = immunity_colors[c(1, 4)],
                                   aesthetics = c("color", "fill")) + 
                theme_classic() + 
                # scale_x_continuous(expand = expansion(mult = c(0.02, 0.02))) + 
                scale_y_continuous(limits = c(-1, 1.4),
                                   breaks = seq(-0.5, 1.5, by = 0.5),
                                   expand = expansion(mult = 0)) +
                ylab("d log(dengue)/d temp") +  
                theme(legend.position = "none", 
                      strip.background = element_blank(),
                      plot.margin = unit(c(37.5, 5.5, 5.5, 5.5), "points"))}, 
          ncol = 2, 
          rel_widths = c(1, 0.7),
          labels = c("a) marginal effect by immunity quantiles", 
                     "b) distribution of marginal effects\n    over observed temperatures"), 
          label_size = 12.5, 
          vjust = c(1.5, 1.2),
          hjust = 0, label_x = c(0.03, -0.03)) %>% 
  ggsave(filename = "figures/immunity_responses_expanded.png",
         width = 6.5, height = 9)