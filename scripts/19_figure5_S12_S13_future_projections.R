library(ggridges)
library(magrittr)
library(tidyverse)
library(sf)
library(rworldmap)
library(cowplot)
library(gtable)
# library(ggrepel)

mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

# full set of scenarios
all_scenarios <- c("ssp126", "ssp245", "ssp370", "plusone")

country_names <- c("Bolivia", 
                  "Peru", 
                  "Mexico", 
                  "Colombia", 
                  "Costa Rica",
                  "Taiwan (Province of China)", 
                  "Brazil", 
                  "Honduras", 
                  "El Salvador", 
                  "Laos", 
                  "Nicaragua", 
                  "Venezuela", 
                  "Vietnam", 
                  "Dominican Republic", 
                  "Indonesia", 
                  "Panama", 
                  "Sri Lanka", 
                  "Philippines", 
                  "Thailand", 
                  "Cambodia", 
                  "Malaysia")
# load data -----
# observed temp data
obs_temp <- readRDS("./data/dT_combined/scenarios_era_current_current.gz")
# population by country
pop <- readRDS("./data/all_pop2015.rds") %>% 
  filter(country != "CHN")
# calculate country averages to use later
# SHOULD THE COUNTRY AVERAGES USE ONLY UNITS WITH DENGUE?
country_temps <- obs_temp %>% 
  summarise(mean = mean(mean_2m_air_temperature_degree1), 
            .by = c(country, id)) %>% 
  left_join(pop) %>% 
  summarise(mean_temp = weighted.mean(mean, sum, na.rm = T), 
            .by = country) %>% 
  rbind(data.frame(country = "overall", mean_temp = -Inf))

# dengue_units <- readRDS("./data/dengue_temp_full.rds") %>% 
#   summarise(total_dengue = sum(dengue_cases, na.rm = T), 
#             .by = c(country, mid_year, id)) %>% 
#   # drop LKA1, filter to the max mid_year for each country 
#   filter(country != "LKA1") %>% 
#   filter(mid_year == max(mid_year), 
#          .by = country) %>% 
#   select(-mid_year)
dengue_units <- readRDS("./data/unit_covariates.rds") %>% 
  filter(mid_year == max(mid_year), 
         .by = country)

# shapefiles (simplifying the admin boundaries)
shp <- readRDS("./data/shapefiles_plotting.rds")
continents <- shp$continents 
countries <- shp$countries
all_shapes <- shp$admin %>% st_simplify(dTolerance = 5000)
rm(shp)

cities <- st_read("./data/World_Cities")
# downloaded from https://hub.arcgis.com/datasets/esri::world-cities/about 

# unit-specific estimates
unit <- expand.grid(x = all_scenarios, 
                    y = c("main", "het_continent_tercile")) %>% 
  purrr::pmap(function(x, y){
    readRDS(paste0("./output/projection_ests/unit_changes_maxBoot100_mod_", y, "_scenario_", x, ".rds")) %>% 
      mutate(scenario = x, mod = y)
  }) %>% list_rbind %>% 
  left_join(country_temps, by = "country") %>% 
  arrange(scenario, mean_temp) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country))) %>% 
  mutate(id_join = case_when(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX") ~ stringi::stri_trans_general(id, id = "Latin-ASCII"), 
                             country == "BRA" ~ str_sub(id, 1, 8), 
                             country == "IDN" ~ case_when(id == "Dki Jakarta" ~ "Jakarta", 
                                                          id == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta", 
                                                          # kalimantan utara gets assigned the dengue cases from timur, which is okay since we only care about whether dengue is > 0
                                                          id == "Kalimantan Utara" ~ "Kalimantan Timur", 
                                                          T ~ id), 
                             T ~ id) %>% tolower) %>% 
  filter(id_join != "br430000" & id != "[unknown]") %>% 
  full_join(dengue_units %>% transmute(id_join = id, country, pop, sub_country_dengue)) 

# country-specific estimates
country <- expand.grid(x = all_scenarios, 
                       y = c("main", "het_continent_tercile")) %>% 
  purrr::pmap(function(x, y){
    readRDS(paste0("./output/projection_ests/country_changes_maxBoot100_mod_", y, "_scenario_", x, ".rds")) %>% 
      mutate(scenario = x, mod = y)
  }) %>% list_rbind %>% 
  filter(country != "overall_country_avg") %>% 
  mutate(country = gsub("_.*", "", country)) %>% 
  left_join(country_temps, by = "country") %>% 
  arrange(scenario, desc(mean_temp)) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country)))

# between scenario comparisons
scenario_comp <- c("main", "het_continent_tercile") %>% 
  purrr::map(function(y){
    readRDS(paste0("./output/projection_ests/country_changes_maxBoot100_mod_", y, "_scenario_ssp370_minus_ssp126.rds")) %>% 
      mutate(mod = y)
  }) %>% list_rbind %>% 
  filter(country != "overall_country_avg") %>% 
  mutate(country = gsub("_.*", "", country)) %>% 
  left_join(country_temps, by = "country") %>% 
  arrange(desc(mean_temp)) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country)))

# panel a: map median change by unit ----
{ggplot() + 
    geom_sf(data = continents,
            fill="grey90", colour="grey10") +
    geom_sf(data = all_shapes %>% 
              filter(country != "LKA1") %>% 
              mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
              full_join(unit %>% filter(scenario == "ssp370" & mod == "main" & sub_country_dengue > 0)), 
            mapping = aes(fill = pmin(pct_change_dengue_mean, 1.5)*100,
                          color = pmin(pct_change_dengue_mean, 1.5)*100), 
            size = 0.01) + 
    # geom_sf(data = countries, color = "grey90", 
    #         lwd = 0.37, fill = NA) +
    geom_sf(data = countries, color = "grey10", 
            lwd = 0.32, fill = NA) +
    # geom_sf(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
    #         shape = 1, color = "white", size = 1.5, linewidth = 1.5) + 
    # geom_sf(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
    #         shape = 1, color = "black") + 
    geom_point(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% country_names),
               aes(geometry = geometry),
               stat = "sf_coordinates", 
               color = "white", shape = 1, size = 1.7, stroke = 1.5) +
    geom_point(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% country_names),
               aes(geometry = geometry),
               stat = "sf_coordinates", 
               color = "black", shape = 1, size = 2.1, stroke = 0.9) +
    ylim(-34, 33) + 
    xlim(-92, 137) +
    theme_void() +
    # scale_colour_gradient2(name = "% change\nin dengue", 
    #                        high = scales::muted("red"),
    #                        mid = "white",
    #                        low = scales::muted("blue"),
    #                        midpoint = 0, 
    #                        aesthetics = c("fill", "color")) +
    scale_fill_gradientn(name = "% change\nin dengue",
                         colors = cmocean::cmocean("curl", clip = 0.05)(20),
                         aesthetics = c("fill", "color"),
                         rescaler = mid_rescaler(), 
                         breaks = c(0, 0.5, 1, 1.5)*100, 
                         labels = c("0%", "50%", "100%", ">150%"), 
                         na.value = "grey90") + 
    # scico::scale_color_scico(name = "% change\nin dengue",
    #                          palette = "vik",
    #                          midpoint = 0, 
    #                          aesthetics = c("fill", "color")) + 
    theme(panel.background = element_rect("white", NA), 
          legend.position = c(0.6, 0.45))} -> unit_map 

# panel b: between scenario comparison ----
# show the ssp 126, ssp245, ssp370 and then their difference 
plot_comp2 <- rbind(country %>% 
                      select(country, scenario, mod, mean_temp, starts_with("pct_inc")) %>% 
                      rename_with(~gsub("pct_inc_dengue_change_", "", .x)), 
                    scenario_comp %>% select(country, mod, mean_temp, starts_with("abs_change")) %>% 
                      rename_with(~gsub("abs_change_", "", .x)) %>%  
                      mutate(scenario = "ssp126_ssp370")) %>% 
  filter(!grepl("hist|plusone|ssp245", scenario) & mod == "main") %>% 
  mutate(panel = grepl("_", scenario)) %>% 
  mutate(scenario = factor(scenario, 
                           levels = c("ssp126", "ssp370", "ssp126_ssp370")),
         panel = grepl("_", scenario)) %>% 
  mutate(q_0.975 = ifelse(grepl("_", scenario), 
                          pmin(q_0.975, 2),
                          pmin(q_0.975, 3))) %>%
  # # mutate(across(starts_with("pct_change_dengue"),
  #               ~ifelse(scenario == "ssp126_ssp370",
  #                       .x*3.5,
  #                       .x))) %>%
  {ggplot(data = ., aes(x = as.numeric(scenario) + 
                          -(as.numeric(country)-11)/22 + 
                          case_when(scenario == "ssp126_ssp370" ~ 0.2, 
                                    scenario == "ssp370" ~ 0.075,
                                    T ~ 0), 
                        color = country)) + 
      # hack-y, but lets add white points to get the 0s to align on the y-axis between panels
      geom_point(data = summarise(.,
                                  ymax = max(q_0.975),
                                  ymin = min(q_0.025),
                                  .by = panel) %>% 
                   # rescale the other panels to match ssp126_ssp370
                   # essentially need to multiply the low value of the other panel by the ratio of ymax/ymin 
                   mutate(rescaler = ymin[panel == T]/ymax[panel == T],
                          ymin = ifelse(!panel, ymax*rescaler, ymin)) %>%
                   pivot_longer(starts_with("y")),
                 aes(x = ifelse(panel, 3, 1), y = value),
                 alpha = 0, color = "white", shape = 2, inherit.aes = FALSE) +
      geom_hline(yintercept = 0, alpha = 0.4) + 
      geom_point(aes(y = mean)) +
      # geom_vline(xintercept = 2.625, linewidth = 1.7) + 
      geom_linerange(aes(ymin = q_0.025,
                         ymax = q_0.975)) +
      # lemon::facet_rep_grid(~panel, scales = "free", space = "free_x") +
      facet_wrap(~panel, scales = "free")  + 
      scale_color_manual(values = c(MetBrewer::met.brewer("Hiroshige", 21), "grey40"),
                         aesthetics = c("fill", "color")) +
        scale_y_continuous(labels = scales::percent, 
                           expand = expansion(mult = c(0.02, 0))) +
        # , 
        # sec.axis = sec_axis(~ ./3.5, labels = scales::percent, 
        #                     name = "% change in dengue")) + 
  geom_text(data = data.frame(x = c(1, 2.075, 3.2),
                              labs = c("current\nvs\nSSP1-2.6", #"current\nvs\nSSP2-4.5", 
                                       "current\nvs\nSSP3-7.0", "difference between\nSSP1-2.6 and SSP3-7.0")) %>% 
              mutate(panel = !grepl("current", labs)), 
            aes(x = x, y = Inf, label = labs), 
            vjust = 1, size = 3,
            inherit.aes = FALSE) + 
  scale_x_continuous(breaks = c(1 - (1:22-11)/23, 
                                2.075 - (1:22-11)/23, 
                                3.2 - (1:22-11)/23), 
                     expand = expansion(mult = 0.02),
                     labels = country_temps %>% 
                       arrange(desc(mean_temp)) %>% 
                       pull(country) %>% 
                       rep(times = 3)) + 
  # scale_x_continuous(breaks = c(1:2, 3.2),
  #                    labels = c("current\nvs\nSSP1-2.6", #"current\nvs\nSSP2-4.5", 
  #                               "current\nvs\nSSP3-7.0", "SSP1-2.6\nvs\nSSP3-7.0"), 
  #                    expand = expansion(mult = 0.02)) +
  ylab("% change in dengue") + xlab("") + 
  theme_classic() + 
  theme(plot.margin = unit(c(15.5, 5.5, -5.5, 5.5), "points"),
        legend.position = "none",
        strip.background = element_blank(), 
        strip.text = element_blank(),
        axis.text.x = element_text(angle = 55, hjust = 1, size = 6))}  

# read the panel scales
# ggplot_build(plot_comp2)$layout$panel_scales_x
# 
# $layout$panel_scales_x[[1]]$range$range

# adjust the size of the panels so left is twice as large 
plot_comp2  %<>% ggplotGrob
plot_comp2$widths[[5]] <- plot_comp2$widths[[5]]*2

# combine panels ----
plot_grid(unit_map + 
            theme(plot.margin = unit(c(15.5, 0, 0, 0), "points")),
          plot_comp2,  
          ncol = 1, nrow = 2, rel_heights = c(1.1, 1),
          hjust = 0, label_x = 0.01, label_size = 12, 
          vjust = c(1.5, 0.75),
          labels = c("a) projected change in dengue incidence under SSP3-7.0", 
                     'b) comparison between future scenarios')) %>% 
  ggsave(filename = "./figures/figure5.png",
         width = 8, height = 5, bg = "white")

# figure S7: maps of other 2 future scenarios  ---- 
# plot_grid(
#   {ggplot() + 
#         geom_sf(data = continents,
#                 fill="grey90", colour="grey10") +
#         geom_sf(data = all_shapes %>% 
#                   filter(country != "LKA1") %>% 
#                   mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
#                   full_join(unit %>% filter(scenario %in% c("hist-nat") & mod == "main")), 
#                 mapping = aes(fill = -pct_change_dengue_q_0.5*100,
#                               color = -pct_change_dengue_q_0.5*100), 
#                 size = 0.01) + 
#         geom_sf(data = countries, color = "grey10", 
#                 lwd = 0.32, fill = NA) +
#         ylim(-34, 33) + 
#         xlim(-92, 137) +
#         theme_void() +
#         scale_fill_gradientn(name = "% dengue\ndue to\nclimate change",
#                              colors = cmocean::cmocean("diff", clip = 0.05)(20),
#                              aesthetics = c("fill", "color"),
#                              rescaler = mid_rescaler(),
#                              labels = c("0%", "20%", "40%", "60%"),
#                              breaks = c(0, 0.2, 0.4, 0.6)*100) + 
#         theme(panel.background = element_rect("white", NA), 
#               plot.margin = unit(c(10.5, 5.5, 5.5, 5.5), "points"),
#               legend.position = c(0.57, 0.45), 
#               legend.justification = c("left", "center"))}, 
{ggplot() + 
    geom_sf(data = continents,
            fill="grey90", colour="grey10") +
    geom_sf(data = all_shapes %>% 
              filter(country != "LKA1") %>% 
              mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
              left_join(unit %>% 
                          filter(scenario %in% c("ssp245", "ssp126") & mod == "main" & 
                                   sub_country_dengue > 0),
                        multiple = "all") %>% 
              filter(!is.na(scenario)), 
            mapping = aes(fill = pmin(pct_change_dengue_mean, 1.5)*100,
                          color = pmin(pct_change_dengue_mean, 1.5)*100), 
            size = 0.01) + 
    geom_sf(data = countries, color = "grey10", 
            lwd = 0.32, fill = NA) +
    facet_wrap(~scenario, ncol = 1, 
               labeller = as_labeller(c(ssp126 = "a) projected changes under SSP1-2.6 (2040 - 2059)", 
                                        ssp245 = "b) projected changes under SSP2-4.5 (2040 - 2059)"))) + 
    ylim(-34, 33) + 
    xlim(-92, 137) +
    theme_void() +
    scale_fill_gradientn(name = "% change\nin dengue",
                         colors = cmocean::cmocean("curl", clip = 0.05)(20),
                         aesthetics = c("fill", "color"),
                         rescaler = mid_rescaler(), 
                         breaks = c(0, 0.5, 1, 1.5)*100, 
                         labels = c("0%", "50%", "100%", ">150%"),
                         na.value = "grey90") + 
    theme(panel.background = element_rect("white", NA), 
          plot.margin = unit(c(10.5, 5.5, 5.5, 5.5), "points"),
          strip.text = element_text(size = 13, face = "bold", 
                                    vjust = 1, hjust = 0),
          legend.justification = c("left", "center"),
          legend.position = c(0.57, 0.2))} %>% 
  # labels = c("a) estimated impact of current warming (1995 - 2014)", 
  #            "b) projected changes under future climate scenarios (2040 - 2059)"),
  # hjust = 0, label_x = 0.01, label_size = 14,
  #     rel_heights = c(1, 2), 
  #     ncol = 1) %>% 
  ggsave(filename = "./figures/figureS7_projection_maps.png", 
         width = 8, height = 5.5, bg = "white")

# figure S8 estimates with americas vs asia estimates ----
# plot_comp_sup <- rbind(country %>% 
#                          select(country, scenario, mod, mean_temp, starts_with("pct_inc")) %>% 
#                          rename_with(~gsub("pct_inc_dengue_change_", "", .x)), 
#                        scenario_comp %>% select(country, mod, mean_temp, starts_with("abs_change")) %>% 
#                          rename_with(~gsub("abs_change_", "", .x)) %>%  
#                          mutate(scenario = "ssp126_ssp370")) %>% 
#   filter(!grepl("hist|plusone", scenario) & mod == "het_continent_tercile") %>% 
#   mutate(panel = grepl("_", scenario)) %>% 
#   mutate(scenario = factor(scenario, 
#                            levels = c("ssp126", "ssp245", "ssp370", "ssp126_ssp370"))) %>% 
#   mutate(across(starts_with("pct_change_dengue"), 
#                 ~ifelse(scenario == "ssp126_ssp370", 
#                         .x*3.5, 
#                         .x))) %>% 
#   ggplot(aes(x = as.numeric(scenario) + 
#                -(as.numeric(country)-11)/22 + 
#                ifelse(scenario == "ssp126_ssp370", 0.2, 0), 
#              color = country)) + 
#   geom_hline(yintercept = 0, alpha = 0.4) + 
#   geom_point(aes(y = pct_change_dengue_mean)) + 
#   geom_vline(xintercept = 3.625, linewidth = 1.7) + 
#   geom_linerange(aes(ymin = pct_change_dengue_q_0.025,
#                      ymax = pmin(pct_change_dengue_q_0.975, 3))) +
#   scale_color_manual(values = MetBrewer::met.brewer("Hiroshige", 21),
#                      aesthetics = c("fill", "color")) +
#   scale_y_continuous(labels = scales::percent, 
#                      sec.axis = sec_axis(~ ./3.5, labels = scales::percent, 
#                                          name = "percent change in dengue")) + 
#   scale_x_continuous(breaks = c(1:3, 4.2),
#                      labels = c("current\nvs\nSSP1-2.6", "current\nvs\nSSP2-4.5", 
#                                 "current\nvs\nSSP3-7.0", "difference between\nSSP1-2.6 and SSP3-7.0"), 
#                      expand = expansion(mult = 0.02)) +
#   ylab("% change in dengue") + xlab("") + 
#   theme_classic() + 
#   theme(legend.position = "none")  
# 
# plot_comp_sup
rbind(country %>% 
        select(country, scenario, mod, mean_temp, starts_with("pct_inc")) %>% 
        rename_with(~gsub("pct_inc_dengue_change_", "", .x)), 
      scenario_comp %>% select(country, mod, mean_temp, starts_with("abs_change")) %>% 
        rename_with(~gsub("abs_change_", "", .x)) %>%  
        mutate(scenario = "ssp126_ssp370")) %>% 
  filter(!grepl("hist|plusone", scenario)) %>% 
  mutate(mod = case_match(mod, 
                          "het_continent_tercile" ~ "het", 
                          .default = mod)) %>% 
  select(country, q_0.025, mean, q_0.975, scenario, mod, mean_temp) %>% 
  pivot_wider(names_from = mod, values_from = c(q_0.025, mean, q_0.975)) %>% 
  left_join(dengue_units %>% select(country, continent_tercile) %>% distinct) %>% 
  mutate(continent_tercile = replace_na(continent_tercile, "overall")) %>% 
  arrange(desc(mean_temp)) %>% 
  mutate(continent_tercile = stringr::str_to_sentence(continent_tercile),
         country = factor(country, ordered = T, 
                          levels = unique(country))) %>% 
  ggplot(aes(x = mean_main, y = mean_het, shape = continent_tercile, 
             color = country)) + 
  geom_abline(slope = 1, intercept = 0, alpha = 0.75) + 
  geom_hline(yintercept = 0, linetype = "dashed", alpha = 0.75) + 
  geom_vline(xintercept = 0, linetype = "dashed", alpha = 0.75) + 
  geom_point() + 
  geom_linerange(aes(ymin = q_0.025_het,
                     ymax = pmin(q_0.975_het, 
                                 case_when(scenario == "ssp126" ~ 2, 
                                           scenario == "ssp245" ~ 2.5, 
                                           scenario == "ssp370" ~ 3, 
                                           scenario == "ssp126_ssp370" ~ 1.25))),
                 linewidth = 0.2) +
  geom_linerange(aes(xmin = q_0.025_main,
                     xmax = pmin(q_0.975_main, 
                                 case_when(scenario == "ssp126" ~ 2, 
                                           scenario == "ssp245" ~ 2.5, 
                                           scenario == "ssp370" ~ 3, 
                                           scenario == "ssp126_ssp370" ~ 1.25))), 
                 linewidth = 0.2) +
  facet_wrap(~scenario, scales = "free", 
             labeller = labeller(scenario = c("ssp126" = "current vs SSP1-2.6", 
                                              "ssp245" = "current vs SSP2-4.5", 
                                              "ssp370" = "current vs SSP3-7.0", 
                                              "ssp126_ssp370" = "difference between\nSSP1-2.6 and SSP3-7.0"))) +
  xlab("projected % change in dengue\nunder main model specification") + 
  ylab("projected % change in dengue\nunder continent-specific model specification") + 
  scale_color_manual(values = c(MetBrewer::met.brewer("Hiroshige", 21), "grey40"),
                     guide = "none") +
  scale_x_continuous(labels = scales::percent) + 
  scale_y_continuous(labels = scales::percent) + 
  theme_classic() + 
  theme(strip.background = element_blank(), 
        legend.position = c(0.15, 0.93), 
        legend.title= element_blank(),
        legend.background = element_blank()) -> proj_comp_plot

test <- all_shapes %>% 
  filter(country != "LKA1") %>% 
  mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
  full_join(unit %>% filter(scenario == "ssp370" & mod == "het_continent_tercile" & sub_country_dengue > 0)) 
{ggplot() + 
    geom_sf(data = continents,
            fill="grey90", colour="grey10") +
    geom_sf(data = test, 
            mapping = aes(fill = pmin(pct_change_dengue_mean, 1.5)*100,
                          color = pmin(pct_change_dengue_mean, 1.5)*100), 
            size = 0.01) + 
    # geom_sf(data = countries, color = "grey90", 
    #         lwd = 0.37, fill = NA) +
    geom_sf(data = countries, color = "grey10", 
            lwd = 0.32, fill = NA) +
    # geom_sf(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
    #         shape = 1, color = "white", size = 1.5, linewidth = 1.5) + 
    # geom_sf(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
    #         shape = 1, color = "black") + 
    # geom_point(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% country_names),
    #            aes(geometry = geometry),
    #            stat = "sf_coordinates", 
    #            color = "white", shape = 1, size = 1.7, stroke = 1.5) +
    # geom_point(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% country_names),
    #            aes(geometry = geometry),
    #            stat = "sf_coordinates", 
    #            color = "black", shape = 1, size = 2, stroke = 1) +
    ylim(-34, 33) + 
    xlim(-92, 137) +
    theme_void() +
    # scale_colour_gradient2(name = "% change\nin dengue", 
    #                        high = scales::muted("red"),
    #                        mid = "white",
    #                        low = scales::muted("blue"),
    #                        midpoint = 0, 
    #                        aesthetics = c("fill", "color")) +
    scale_fill_gradientn(name = "% change\nin dengue",
                         colors = cmocean::cmocean("curl", clip = 0.05)(20),
                         aesthetics = c("fill", "color"),
                         rescaler = mid_rescaler(), 
                         breaks = c(0, 0.5, 1, 1.5)*100, 
                         labels = c("0%", "50%", "100%", ">150%"), 
                         na.value = "grey90") + 
    # scico::scale_color_scico(name = "% change\nin dengue",
    #                          palette = "vik",
    #                          midpoint = 0, 
    #                          aesthetics = c("fill", "color")) + 
    theme(panel.background = element_rect("white", NA), 
          legend.position = c(0.6, 0.45))} -> unit_map_het 

# add inset on map with the two models compared
temp_seq <- seq(0, 40, 0.1)

boot_coef_ests <- rbind(readRDS("./output/mod_ests/main_coef_boot1000.rds") %>% 
                          select(contains("air_temp")) %>% 
                          mutate(het_tercile = "main"), 
                        readRDS("./output/mod_ests/het_continent_tercile_coef_boot1000.rds") %>% 
                          select(contains("air_temp")) %>% 
                          pivot_longer(everything(), 
                                       names_pattern = "continent_tercile(.*):(.*)", 
                                       names_to = c("het_tercile", ".value"))) 
  
temp_marg_mat <- colnames(boot_coef_ests) %>%
  grep("temp", ., value = TRUE) %>% 
  gsub(".*degree|_lag.", "", .) %>% 
  as.numeric %>%
  purrr::map(function(deg){
    deg*temp_seq^(deg - 1)
  }) %>% 
  reduce(cbind) 

boot_marg <- as.matrix(boot_coef_ests %>% select(contains("air_temp"))) %*% t(temp_marg_mat) %>% 
  set_colnames(paste0("temp_", temp_seq)) %>% 
  as.data.frame() %>% 
  cbind(mod = boot_coef_ests$het_tercile) %>% 
  mutate(boot_id = 1:n()) %>% 
  pivot_longer(!c(mod, boot_id), names_prefix = "temp_", 
               names_transform = as.numeric,
               names_to = "x", values_to = "y") %>% 
  summarise(ymin = quantile(y, 0.025), 
            ymid = mean(y, 0.5),
            ymax = quantile(y, 0.975), 
            .by = c(x, mod))

mod_temp_ranges = readRDS("./data/dengue_temp_full.rds") %>% 
  left_join(readRDS("./data/unit_covariates.rds") %>% 
              select(country, id, mid_year, mod = continent_tercile)) %>% 
  filter(!is.na(dengue_cases) & !is.na(pop) & nonzero_dengue) %>% 
  select(mod, x = mean_2m_air_temp_degree1) %>% 
  rbind(., 
        mutate(., mod = "main")) %>% 
  summarise(xmax = quantile(x, 0.99), 
            xmin = quantile(x, 0.01), 
            .by = mod) 

boot_marg %>% 
  left_join(mod_temp_ranges) %>% 
  filter(x >= xmin & x <= xmax) %>%  
  ggplot(aes(x = x, y = ymid, ymax = ymax, ymin = ymin,
             group = mod, color = mod, fill = mod)) +
  geom_hline(yintercept = 0) + 
  # geom_vline(data = country_temps %>% arrange(desc(mean_temp)) %>% 
  #              mutate(country = factor(country, ordered = T, 
  #                                      levels = unique(country))), 
  #            aes(xintercept = mean_temp)) + 
  geom_ribbon(color = NA, alpha = 0.5) + 
  geom_line(linewidth = 1.1) + 
  theme_classic() + 
  annotate("text", x = 29.5, y = 0.3, label = "Asia", color = "#90719f", size = 3) + 
  annotate("text", x = 23, y = 0.9, label = "Americas", color = "#466c4b", size = 3) + 
  # annotate("text", x = 16, y = 0.65, label = "main", color = "grey10") + 
  scale_color_manual(values = c("#466c4b","#90719f","grey10"),
                     aesthetics = c("color", "fill"),
                     guide = "none") -> mod_marg_inset

plot_grid(unit_map_het + 
            theme(plot.margin = unit(c(15.5, 0, 0, 0), "points"), 
                  legend.position = c(0.39, 0.67),
                  legend.title = element_text(size = 9),
                  legend.text = element_text(size = 8.5)) + 
            annotation_custom(grob=ggplotGrob(mod_marg_inset + 
                                                ylab("d log(dengue)/d temperature") + xlab("temperature (°C)") + 
                                                cowplot::theme_half_open() + 
                                                theme(plot.margin = unit(c(0, 0, 0, 0), "points"), 
                                                      axis.text = element_text(size = 8),
                                                      text = element_text(size = 8))),
                              xmin = 10, xmax = 95, 
                              ymin = -Inf, ymax = 10), 
          proj_comp_plot + 
            theme(plot.margin = unit(c(15.5, 5.5, 5.5, 5.5), "points")),  
          ncol = 1, nrow = 2, rel_heights = c(1, 1.85),
          hjust = 0, label_x = 0.01, label_size = 12, 
          vjust = c(1.5, 0.75),
          labels = c("a) projected change in dengue incidence under SSP3-7.0 with continent-specific estimates", 
                     'b) comparison projected changes with main and continent-specific estimates')) %>% 
  ggsave(filename = "./figures/figureS8_het_projections.png",
         width = 8, height = 8, bg = "white")

# SI table ----
purrr::map(c("ssp126", "ssp245", "ssp370", "hist-nat"), function(x){
  readRDS(paste0("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_", x, ".rds")) %>% 
      mutate(scenario = x)
  }) %>% list_rbind %>% 
  select(country, scenario, 
         lwr = pct_inc_dengue_change_q_0.025,
         mean = pct_inc_dengue_change_mean, 
         upr = pct_inc_dengue_change_q_0.975) %>% 
  rbind(readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_ssp370_minus_ssp126.rds") %>% 
          transmute(country, scenario = "ssp126_ssp370",
                    lwr = abs_change_q_0.025,
                    mean = abs_change_mean, 
                    upr = abs_change_q_0.975)) %>% 
  transmute(scenario, country, 
            est = paste0(round(mean*100, 1), "% (",
                         round(lwr*100, 1), "-", 
                         round(upr*100, 1), "%)")) %>%
  mutate(country = ifelse(country == "overall_pop_weight", "overall", country)) %>%
  mutate(scenario = factor(scenario, levels = c("hist-nat", 
                                                "ssp126", 
                                                "ssp245", 
                                                "ssp370", 
                                                "ssp126_ssp370"), 
                           ordered = T)) %>% 
  arrange(scenario) %>% # convoluted way to get factor in the right order for later
  mutate(scenario = case_match(scenario, 
                               "ssp126" ~ "current vs SSP1-2.6", 
                               "ssp245" ~ "current vs SSP2-4.5", 
                               "ssp370" ~ "current vs SSP3-7.0", 
                               "ssp126_ssp370" ~ "difference between SSP1-2.6 and SSP3-7.0", 
                               "hist-nat" ~ "current vs no anthropogenic forcing"), 
         scenario = factor(scenario, ordered = T, levels = unique(scenario))) %>%
  arrange(scenario, country) %>% 
  pivot_wider(names_from = scenario, values_from = est) %>% 
  xtable::xtable(., 
                 caption = "Projected percent change in dengue incidence for countries under different climate scenarios. Numbers are mean estimates followed by 95\\% CIs.",
                 label = "country_proj") %>% 
  print(file = "./figures/table_SX_country_projections.tex", 
        include.rownames = FALSE)
# in the future, update to fix column names to make multi-row to reduce post-processing
  

# historical for erin ----
# test <- all_shapes %>% 
#   full_join(unit %>% filter(scenario == "hist-nat")) %>% 
#   filter(country != "LKA1")
# {ggplot() + 
#     geom_sf(data = continents,
#             fill="grey90", colour="grey10") +
#     geom_sf(data = test, 
#             mapping = aes(fill = -pct_change_dengue_q_0.5*100,
#                           color = -pct_change_dengue_q_0.5*100), 
#             size = 0.01) + 
#     geom_sf(data = countries, color = "grey10", 
#             lwd = 0.32, fill = NA) +
#     ylim(-34, 33) + 
#     xlim(-92, 137) +
#     theme_void() +
#     scale_fill_gradientn(name = "% dengue\ndue to\nclimate change",
#                          colors = cmocean::cmocean("balance", clip = 0.1)(20),
#                          aesthetics = c("fill", "color"),
#                          rescaler = mid_rescaler()) + 
#     theme(panel.background = element_rect("white", NA), 
#           legend.position = c(0.6, 0.45))} %>% 
#   ggsave(filename = "./figures/hist-nat_map.png", 
#          width = 8, height = 5)
# 
# {unit %>% 
#     filter(scenario == "hist-nat") %>%
#     ggplot(aes(y = country, 
#                group = country, 
#                color = country,
#                fill = country)) +
#     geom_vline(xintercept = 0, alpha = 0.4) + 
#     geom_density_ridges(aes(x = `pct_change_dengue_q_0.5`, 
#                             height = ..ndensity..),
#                         alpha = 0.2, color = alpha("grey20", 0.7),
#                         # stat = "density", 
#                         # trim = TRUE,
#                         scale = 0.9,
#                         rel_min_height = 0.01,
#                         bandwidth = 0.1,
#                         jittered_points = TRUE,
#                         position = position_points_jitter(width = 0.005,
#                                                           height = 0),
#                         point_color = "black", point_alpha = 0.5,
#                         point_shape = "|", point_size = 1.5) +
#     geom_point(data = country %>% filter(scenario == "hist-nat"), 
#                aes(x = pct_change_dengue_q_0.5, 
#                    y = as.numeric(country) - 0.1), 
#                size = 2) + 
#     geom_linerange(data = country %>% filter(scenario == "hist-nat"),
#                    aes(xmin = pct_change_dengue_q_0.025,
#                        xmax = pmin(pct_change_dengue_q_0.975, 4),
#                        y = as.numeric(country) - 0.1), 
#                    linewidth = 0.7) +
#     geom_linerange(data = country %>% filter(scenario == "hist-nat"),
#                    aes(xmin = pct_change_dengue_q_0.05,
#                        xmax = pmin(pct_change_dengue_q_0.95, 4),
#                        y = as.numeric(country) - 0.1), 
#                    linewidth = 1.3) +
#     scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
#                        aesthetics = c("fill", "color")) +
#     # scale_y_discrete(expand = expansion(mult = c(0.03, 0.075))) +
#     # scale_x_continuous(lim = c(-0.001, 1),
#     # expand = expansion(mult = c(0.02, 0.05))) +
#     scale_x_continuous(labels = scales::percent, trans = "reverse", 
#                        breaks = c(-0.75, -0.5, -0.25, 0, 0.25),
#                        limits = c(0.4, -0.8)) + 
#     theme_classic() + 
#     xlab("% dengue due to existing climate change") + 
#     ylab("") + 
#     theme(legend.position = "none", 
#           text = element_text(size = 15))} %>% 
#   ggsave(filename = "./figures/hist-nat_distributions.png", 
#          width = 5, height = 8)

# scratch ----
# bhatt_est <- readxl::read_excel("./data/Bhatt estimates.xlsx") %>% 
#   mutate(country_code = case_match(Country, 
#                                    "Bolivia" ~ "BOL", 
#                                    "Peru" ~ "PER", 
#                                    "Mexico" ~ "MEX", 
#                                    "Colombia" ~ "COL", 
#                                    "Costa Rica" ~ "CRI",
#                                    "Taiwan" ~ "TWN", # missing in the Bhatt estimates
#                                    "Brazil" ~ "BRA", 
#                                    "Honduras" ~ "HND", 
#                                    "El Salvador" ~ "SLV", 
#                                    "Lao People's Democratic" ~ "LAO", 
#                                    "Nicaragua" ~ "NIC", 
#                                    "Venezuela" ~ "VEN", 
#                                    "Viet Nam" ~ "VNM", 
#                                    "Dominican Republic" ~ "DOM", 
#                                    "Indonesia" ~ "IDN", 
#                                    "Panama" ~ "PAN", 
#                                    "Sri Lanka" ~ "LKA", 
#                                    "Philippines" ~ "PHL", 
#                                    "Thailand" ~ "THA", 
#                                    "Cambodia" ~ "KHM", 
#                                    "Malaysia" ~ "MYS")) %>% 
#   filter(!is.na(country_code)) %>%
#   mutate(across(contains("pparent"), as.numeric)) %>% 
#   left_join(pop %>% 
#               summarise(pop = sum(sum), 
#                         .by = country), 
#             by = c("country_code" = "country")) %>% 
#   mutate(inc_mid = Inapparent...5/pop, 
#          inc_low = Inapparent...6/pop, 
#          inc_high = Inapparent...7/pop) 

# bhatt_est %>%
#   # gbd_est %>% 
#   left_join(country %>% filter(scenario == "hist-nat"), 
#             by = c("country_code" = "country_orig")) %>% 
#   ggplot(aes(x = inc_mid*10000, # val/10, #
#              color = country,
#              # x = Inapparent...5,
#              y = -q_0.5)) + 
#   geom_point() + 
#   geom_linerange(aes(ymin = -q_0.025, 
#                      ymax = -q_0.975)) + 
#   xlab("estimated incidence (per 10k?)") + 
#   ylab("estimated % due to existing climate change") + 
#   scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
#                      aesthetics = c("fill", "color")) +
#   theme_classic()
# 
# 
# country %>% 
#   filter(scenario != "plusone") %>% 
#   left_join(obs_temp %>% 
#               summarise(mean = mean(mean_2m_air_temperature_degree1), 
#                         .by = c(country, id)) %>% 
#               left_join(pop) %>% 
#               summarise(mean_temp = weighted.mean(mean, sum), 
#                         .by = country)) %>% 
#   ggplot(aes(x = mean_temp + case_when(scenario == "hist-nat" ~ 0, 
#                                        scenario == "ssp126" ~ 0, 
#                                        scenario == "ssp245" ~ 0.02,
#                                        scenario == "ssp370" ~ 0.04), 
#              y = `q_0.5`, 
#              color = scenario)) + 
#   geom_hline(yintercept = 0, color = "grey30") +
#   geom_errorbar(aes(ymin = `q_0.025`, 
#                     ymax = `q_0.975`)) +
#   geom_point() + 
#   theme_classic() + 
#   xlab("mean temperature") + ylab("percent change in dengue")  
# 
# rbind(country, 
#       scenario_comp %>% mutate(scenario = "ssp126_ssp370")) %>% 
#   filter(scenario == "hist-nat") %>% 
#   mutate(scenario = as.factor(scenario)) %>% 
#   ggplot(aes(x = as.numeric(scenario) + -(as.numeric(country)-11)/20, 
#              color = country)) + 
#   geom_hline(yintercept = 0, alpha = 0.4) + 
#   geom_point(aes(y = q_0.5)) + 
#   geom_linerange(aes(ymin = q_0.025,
#                      ymax = pmin(q_0.975, 3))) +
#   scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
#                      aesthetics = c("fill", "color")) +
#   scale_y_continuous(labels = scales::percent) + 
#   ylab("percent change") + xlab("") + 
#   theme_classic() + 
#   theme(legend.position = "none") 

# plot_comp <- rbind(country, 
#                    scenario_comp %>% mutate(scenario = "ssp126_ssp370")) %>% 
#   filter(!grepl("hist|plusone", scenario)) %>% 
#   mutate(panel = grepl("_", scenario)) %>% 
#   mutate(scenario = factor(scenario, 
#                            levels = c("ssp126", "ssp245", "ssp370", "ssp126_ssp370"))) %>% 
#   ggplot(aes(x = as.numeric(scenario) + -(as.numeric(country)-11)/20, 
#              color = country)) + 
#   geom_hline(yintercept = 0, alpha = 0.4) + 
#   geom_point(aes(y = q_0.5)) + 
#   geom_linerange(aes(ymin = q_0.025,
#                      ymax = pmin(q_0.975, 3))) +
#   scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
#                      aesthetics = c("fill", "color")) +
#   scale_y_continuous(labels = scales::percent, 
#                      sec.axis = sec_axis(~ ., labels = scales::percent, 
#                                          name = "percent change")) + 
#   scale_x_continuous(breaks = 1:4,
#                      labels = c("current\nvs\nssp1-2.6", "current\nvs\nssp2-4.5", 
#                                 "current\nvs\nssp3-7.0", "ssp1-2.6\nvs\nssp3-7.0")) +
#   facet_wrap(~panel, scales = "free") + 
#   ylab("percent change") + xlab("") + 
#   theme_classic() + 
#   theme(legend.position = "none", 
#         strip.background = element_blank(), 
#         strip.text = element_blank(), 
#         panel.spacing = unit(-3, "lines"))  
# plot_comp <- ggplot_gtable(ggplot_build(plot_comp))
# plot_comp$widths[5] = 3*plot_comp$widths[5]
# gtable_remove_grobs(plot_comp, c("axis-l-1-2", "axis-r-1-1")) %>% plot
# 
# gtable::gtable_show_layout(plot_comp)
# plot_comp

# so close! want to make the 0s align and leave the vertical lines between the panels 
# MAAAYBE instead, convert the ssp126 vs 370 numbers to  scale comparable to the current vs future, and then add a second axis?

# scenario_comp %>% 
#   ggplot(aes(y = country, color = country)) + 
#   geom_vline(xintercept = 0, alpha = 0.4) + 
#   geom_point(aes(x = q_0.5),
#              size = 2) + 
#   geom_linerange(aes(xmin = q_0.025,
#                      xmax = q_0.975), 
#                  linewidth = 0.7) +
#   geom_linerange(aes(xmin = q_0.05,
#                      xmax = q_0.95), 
#                  linewidth = 1.3) +
#   scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
#                      aesthetics = c("fill", "color")) +
#   theme_classic() + 
#   theme(legend.position = "none") + 
#   scale_x_continuous(labels = scales::percent) + 
#   ylab("") + xlab("percent change in dengue from ssp1-2.6 to ssp3-7.0")