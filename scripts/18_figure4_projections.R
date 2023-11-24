library(ggridges)
library(magrittr)
library(tidyverse)
library(sf)
library(rworldmap)
library(cowplot)
library(gtable)
library(ggrepel)

mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}


# full set of scenarios
all_scenarios <- c("ssp126", "ssp245", "ssp370", "hist-nat", "plusone")

# load data -----
# observed temp data
obs_temp <- readRDS("./data/dT_combined/scenarios_era_current_current.gz")
# population by country
pop <- readRDS("./data/all_pops2015.rds") %>% 
  filter(country != "CHN")
# calculate country averages to use later
# SHOULD THE COUNTRY AVERAGES USE ONLY UNITS WITH DENGUE?
country_temps <- obs_temp %>% 
  summarise(mean = mean(mean_2m_air_temperature_degree1), 
            .by = c(country, id)) %>% 
  left_join(pop) %>% 
  summarise(mean_temp = weighted.mean(mean, sum, na.rm = T), 
            .by = country)

# shapefiles (simplifying the admin boundaries)
shp <- readRDS("./data/shapefiles_plotting.rds")
continents <- shp$continents 
countries <- shp$countries
all_shapes <- shp$admin %>% st_simplify(dTolerance = 5000)
rm(shp)

cities <- st_read("./data/World_Cities")

# unit-specific estimates
unit <- all_scenarios  %>% 
  purrr::map(function(x){
    readRDS(paste0("./output/projection_ests/unit_changes_maxBoot25_scenario_", x, ".rds")) %>% 
      mutate(scenario = x)
  }) %>% list_rbind %>% 
  left_join(country_temps, by = "country") %>% 
  arrange(scenario, mean_temp) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country))) 
# country-specific estimates
country <- all_scenarios  %>% 
  purrr::map(function(x){
    readRDS(paste0("./output/projection_ests/country_changes_maxBoot25_scenario_", x, ".rds")) %>% 
      mutate(scenario = x)
  }) %>% list_rbind %>% 
  left_join(country_temps, by = "country") %>% 
  arrange(scenario, mean_temp) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country)))
  
# between scenario comparisons
scenario_comp <- readRDS("./output/projection_ests/country_changes_maxBoot25_scenario_ssp370_minus_ssp126.rds") %>% 
  left_join(country_temps, by = "country") %>% 
  arrange(mean_temp) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country)))

# GBD is incidence per 100k
gbd_est <- readxl::read_excel("./data/Global Burden Disease dataset.xls")  %>% 
  filter(measure == "Incidence") %>% 
  mutate(country_code = case_match(location, 
                                   "Bolivia" ~ "BOL", 
                                   "Peru" ~ "PER", 
                                   "Mexico" ~ "MEX", 
                                   "Colombia" ~ "COL", 
                                   "Costa Rica" ~ "CRI",
                                   "Taiwan (Province of China)" ~ "TWN", 
                                   "Brazil" ~ "BRA", 
                                   "Honduras" ~ "HND", 
                                   "El Salvador" ~ "SLV", 
                                   "Laos" ~ "LAO", 
                                   "Nicaragua" ~ "NIC", 
                                   "Venezuela" ~ "VEN", 
                                   "Vietnam" ~ "VNM", 
                                   "Dominican Republic" ~ "DOM", 
                                   "Indonesia" ~ "IDN", 
                                   "Panama" ~ "PAN", 
                                   "Sri Lanka" ~ "LKA", 
                                   "Philippines" ~ "PHL", 
                                   "Thailand" ~ "THA", 
                                   "Cambodia" ~ "KHM", 
                                   "Malaysia" ~ "MYS")) %>% 
  filter(!is.na(country_code)) 



# panel a: map median change by unit ----
test <- all_shapes %>% 
  filter(country != "LKA1") %>% 
  mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
  full_join(unit %>% filter(scenario == "ssp370")) 
{ggplot() + 
    geom_sf(data = continents,
            fill="grey90", colour="grey10") +
    geom_sf(data = test, 
            mapping = aes(fill = pmin(pct_change_dengue_q_0.5, 1.5)*100,
                          color = pmin(pct_change_dengue_q_0.5, 1.5)*100), 
            size = 0.01) + 
    # geom_sf(data = countries, color = "grey90", 
    #         lwd = 0.37, fill = NA) +
    geom_sf(data = countries, color = "grey10", 
            lwd = 0.32, fill = NA) +
    # geom_sf(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
    #         shape = 1, color = "white", size = 1.5, linewidth = 1.5) + 
    # geom_sf(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
    #         shape = 1, color = "black") + 
    geom_point(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
               aes(geometry = geometry),
               stat = "sf_coordinates", 
               color = "white", shape = 1, size = 1.7, stroke = 1.5) +
    geom_point(data = cities %>% filter(POP > 5e6 & CNTRY_NAME %in% gbd_est$location),
               aes(geometry = geometry),
               stat = "sf_coordinates", 
               color = "black", shape = 1, size = 2, stroke = 1) +
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
                         labels = c("0%", "50%", "100%", ">150%")) + 
    # scico::scale_color_scico(name = "% change\nin dengue",
    #                          palette = "vik",
    #                          midpoint = 0, 
    #                          aesthetics = c("fill", "color")) + 
    theme(panel.background = element_rect("white", NA), 
          legend.position = c(0.6, 0.45))} -> unit_map 
# %>% 
#   ggsave(filename = "./figures/proj_map_ssp370.png", 
#          width = 7, height = 3)

# panel b: country changes ---- 
{unit %>% 
    filter(scenario == "ssp370") %>%
    ggplot(aes(y = country, 
               group = country, 
               color = country,
               fill = country)) +
    geom_vline(xintercept = 0, alpha = 0.4) + 
    geom_density_ridges(aes(x = `pct_change_dengue_q_0.5`, 
                            height = ..ndensity..),
                        alpha = 0.2, color = alpha("grey20", 0.7),
                        # stat = "density", 
                        # trim = TRUE,
                        scale = 1,
                        rel_min_height = 0.01,
                        bandwidth = 0.1,
                        jittered_points = TRUE,
                        position = position_points_jitter(width = 0.005,
                                                          height = 0),
                        point_color = "black", point_alpha = 0.5,
                        point_shape = "|", point_size = 1.5) +
    geom_point(data = country %>% filter(scenario == "ssp370"), 
               aes(x = pct_change_dengue_q_0.5, 
                   y = as.numeric(country) - 0.1), 
               size = 2) + 
    geom_linerange(data = country %>% filter(scenario == "ssp370"),
                   aes(xmin = pct_change_dengue_q_0.025,
                       xmax = pmin(pct_change_dengue_q_0.975, 4),
                       y = as.numeric(country) - 0.1), 
                   linewidth = 0.7) +
    geom_linerange(data = country %>% filter(scenario == "ssp370"),
                   aes(xmin = pct_change_dengue_q_0.05,
                       xmax = pmin(pct_change_dengue_q_0.95, 4),
                       y = as.numeric(country) - 0.1), 
                   linewidth = 1.3) +
    scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
                       aesthetics = c("fill", "color")) +
    # scale_y_discrete(expand = expansion(mult = c(0.03, 0.075))) +
    # scale_x_continuous(lim = c(-0.001, 1),
    # expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(labels = scales::percent) + 
    theme_classic() + 
    xlab("percent change in dengue") + 
    ylab("") + 
    theme(legend.position = "none", 
          text = element_text(size = 15))} -> country_dist_plot
  # ggsave(filename = "./figures/proj_by_unit_ssp370.png", 
  #        width = 5, height = 7)

# panel c: between scenario comparison ----
# show the ssp 126, ssp245, ssp370 and then their difference 
plot_comp2 <- rbind(country, 
                   scenario_comp %>% mutate(scenario = "ssp126_ssp370")) %>% 
  filter(!grepl("hist|plusone", scenario)) %>% 
  mutate(panel = grepl("_", scenario)) %>% 
  mutate(scenario = factor(scenario, 
                           levels = c("ssp126", "ssp245", "ssp370", "ssp126_ssp370"))) %>% 
  mutate(across(starts_with("pct_change_dengue"), 
                ~ifelse(scenario == "ssp126_ssp370", 
                        .x*3.5, 
                        .x))) %>% 
  ggplot(aes(x = as.numeric(scenario) + -(as.numeric(country)-11)/20 + 
               ifelse(scenario == "ssp126_ssp370", 0.2, 0), 
             color = country)) + 
  geom_hline(yintercept = 0, alpha = 0.4) + 
  geom_point(aes(y = pct_change_dengue_q_0.5)) + 
  geom_vline(xintercept = 3.625, linewidth = 1.7) + 
  geom_linerange(aes(ymin = pct_change_dengue_q_0.025,
                     ymax = pmin(pct_change_dengue_q_0.975, 3))) +
  scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
                     aesthetics = c("fill", "color")) +
  scale_y_continuous(labels = scales::percent, 
                     sec.axis = sec_axis(~ ./3.5, labels = scales::percent, 
                                         name = "percent change in dengue")) + 
  scale_x_continuous(breaks = c(1:3, 4.2),
                     labels = c("current\nvs\nSSP1-2.6", "current\nvs\nSSP2-4.5", 
                                "current\nvs\nSSP3-7.0", "SSP1-2.6\nvs\nSSP3-7.0")) +
  ylab("percent change in dengue") + xlab("") + 
  theme_classic() + 
  theme(legend.position = "none")  

# panel d: % due to historical warming vs current incidence ----
gbd_est %>% 
  left_join(pop %>% 
              summarise(pop = sum(sum), 
                        .by = country), 
            by = c("country_code" = "country")) %>% 
  left_join(country %>% filter(scenario == "hist-nat"), 
            by = c("country_code" = "country_orig")) %>% 
  ggplot(aes(x = val/10, #inc_mid*1000, 
             color = country,
             # x = Inapparent...5,
             y = -pct_change_dengue_q_0.5)) + 
  geom_point(aes(size = pop)) + 
  geom_linerange(aes(ymin = -pct_change_dengue_q_0.025, 
                     ymax = -pct_change_dengue_q_0.975)) + 
  geom_text_repel(aes(label = country), force_pull = 0.9,
                  point.size = 1.2, min.segment.length = 0.3,
                  color = "black", size = 3) + 
  xlab("estimated incidence (per 10k)") + 
  ylab("estimated % dengue\ndue to existing climate change") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(name = NULL,
                     values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
                     aesthetics = c("fill", "color"),
                     guide = 'none') +
  scale_size_continuous(name = "population",
                        # trans = "log",
                        breaks = c(1e6, 5e6, 1e7, 5e7, 1e8),
                        labels = c("1M", "5M", "10M", "50M", "100M")) + 
  theme_classic() + 
  theme(legend.position = c(1.05, 0.82), 
        legend.background = element_blank()) -> hist_plot
  
# panel b inset ----
source("../scripts/00_functions.R")

# load model bootstrap fits
boot_ests <- readRDS("./output/mod_ests/main_coef_boot1000.rds") %>% 
  select(contains("temp"))
temp_seq <- seq(0, 40, 0.1)
temp_marg_mat <- colnames(boot_ests) %>%
  grep("temp", ., value = TRUE) %>% 
  gsub(".*degree|_lag.", "", .) %>% 
  as.numeric %>%
  purrr::map(function(deg){
    deg*temp_seq^(deg - 1)
  }) %>% 
  reduce(cbind) 
# calculate marginals
boot_marg <- as.matrix(boot_ests) %*% t(temp_marg_mat) %>% 
  set_colnames(paste0("temp_", temp_seq)) %>% 
  as.data.frame() %>% 
  mutate(boot_id = 1:n()) %>% 
  pivot_longer(!boot_id, names_prefix = "temp_", 
               names_transform = as.numeric,
               names_to = "x", values_to = "y")

boot_marg %>%
  filter(x >= 15 & x<= 30.5) %>%
  {ggplot() +
      geom_ribbon(data = summarise(., 
                                   ymin = quantile(y, 0.025),
                                   ymax = quantile(y, 0.975),
                                   .by = x),
                  aes(x = x, ymin = ymin, ymax = ymax),
                  color = NA, fill = "black",
                  alpha = 0.3, inherit.aes = FALSE) +
      geom_line(data = summarise(., y = median(y), .by = x), 
                aes(x = x,
                    y = y),
                color = "black") +
      geom_vline(data = country_temps %>% rename(current_mean = mean_temp) %>%
                   arrange(current_mean) %>%
                   mutate(country_orig = country,
                          country = factor(country, ordered = T,
                                           levels = unique(country))) ,
                 aes(xintercept = current_mean, color = country)) +
      geom_hline(yintercept = 0) + 
      # theme_classic() +
      theme_half_open(7) + 
      coord_cartesian(clip = "off") +
      xlab("temperature (°C)") + ylab("d log(dengue)/d T") +
      scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
                         aesthetics = c("color")) +
      theme(legend.position ="none", plot.margin = unit(c(10.5, 5.5, 5.5, 5.5), "points")) +
      scale_x_continuous(expand = expansion(mult = 0.0),
                         limits = c(15, 30.5)) +
      scale_y_continuous(#breaks = c(-0.25, 0, 0.25, 0.5, 0.75, 1),
                         # limits = c(0, NA),
                         expand = expansion(mult = 0.02))} -> inset_marginal_plot
# combine panels ----
plot_grid(unit_map,
            plot_grid(ggdraw() + 
                        draw_plot(country_dist_plot + theme(text = element_text(size = 10)), 
                                  0, 0, 1, 1) + 
                        draw_plot(inset_marginal_plot + theme(text = element_text(size = 7)), 
                                  0.45, 0.78, width = 0.55, height = 0.21),
                      plot_grid(plot_comp2, hist_plot, 
                                labels = c('c) comparison between future scenarios',
                                           'd) impact of historical warning'), 
                                rel_heights = c(0.75, 1),
                                hjust = 0, label_x = 0.01,           
                                vjust = -0.2, label_size = 12,
                                align = "v",
                                ncol = 1), 
                      nrow = 1,
                      rel_widths = c(0.78, 1),
                      hjust = 0, label_x = 0.01,
                      vjust = -0.2, label_size = 12,
                      labels = c("b) projected changes by country", "")), 
          ncol = 1, 
          rel_heights = c(0.45, 1),
          hjust = 0, label_x = 0.01, label_size = 12,
          labels = c("a) projected change in dengue incidence under SSP3-7.0", "")) %>% 
  ggsave(filename = "./figures/figure4.png",
         width = 8, height = 9, bg = "white")

# figure S6: maps of other 3 scenarios  ---- 
plot_grid({ggplot() + 
    geom_sf(data = continents,
            fill="grey90", colour="grey10") +
    geom_sf(data = all_shapes %>% 
              filter(country != "LKA1") %>% 
              mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
              full_join(unit %>% filter(scenario %in% c("ssp245", "ssp126"))), 
            mapping = aes(fill = pmin(pct_change_dengue_q_0.5, 1.5)*100,
                          color = pmin(pct_change_dengue_q_0.5, 1.5)*100), 
            size = 0.01) + 
    geom_sf(data = countries, color = "grey10", 
            lwd = 0.32, fill = NA) +
    facet_wrap(~scenario, ncol = 1, 
               labeller = as_labeller(c(ssp126 = "SSP1-2.6", 
                                        ssp245 = "SSP2-4.5"))) + 
    ylim(-34, 33) + 
    xlim(-92, 137) +
    theme_void() +
    scale_fill_gradientn(name = "% change\nin dengue",
                         colors = cmocean::cmocean("curl", clip = 0.05)(20),
                         aesthetics = c("fill", "color"),
                         rescaler = mid_rescaler(), 
                         breaks = c(0, 0.5, 1, 1.5)*100, 
                         labels = c("0%", "50%", "100%", ">150%")) + 
    theme(panel.background = element_rect("white", NA), 
          plot.margin = unit(c(10.5, 5.5, 5.5, 5.5), "points"),
          strip.text = element_text(size = 13, face = "bold"),
          legend.justification = c("left", "center"),
          legend.position = c(0.57, 0.7))},
    {ggplot() + 
        geom_sf(data = continents,
                fill="grey90", colour="grey10") +
        geom_sf(data = all_shapes %>% 
                  filter(country != "LKA1") %>% 
                  mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
                  full_join(unit %>% filter(scenario %in% c("hist-nat"))), 
                mapping = aes(fill = -pct_change_dengue_q_0.5*100,
                              color = -pct_change_dengue_q_0.5*100), 
                size = 0.01) + 
        geom_sf(data = countries, color = "grey10", 
                lwd = 0.32, fill = NA) +
        ylim(-34, 33) + 
        xlim(-92, 137) +
        theme_void() +
        scale_fill_gradientn(name = "% dengue\ndue to\nclimate change",
                             colors = cmocean::cmocean("diff", clip = 0.05)(20),
                             aesthetics = c("fill", "color"),
                             rescaler = mid_rescaler(),
                             labels = c("0%", "20%", "40%", "60%"),
                             breaks = c(0, 0.2, 0.4, 0.6)*100) + 
        theme(panel.background = element_rect("white", NA), 
              plot.margin = unit(c(10.5, 5.5, 5.5, 5.5), "points"),
              legend.position = c(0.57, 0.45), 
              legend.justification = c("left", "center"))}, 
    labels = c("a) projected changes under future climate scenarios (2040 - 2059)", 
               "b) estimated impact of current warming (1995 - 2014)"),
    hjust = 0, label_x = 0.01, label_size = 14,
    rel_heights = c(2, 1), 
    ncol = 1) %>% 
  ggsave(filename = "./figures/figureS6_projection_maps.png", 
         width = 8, height = 8.25, bg = "white")

# figure S7 estimates without brazil or americas vs asia? panels b - d ----



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