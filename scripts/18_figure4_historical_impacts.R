library(ggridges)
library(magrittr)
library(tidyverse)
library(ggrepel) # warning that this package and the repelled labels seems to sometimes crash R studio
library(cowplot)
library(sf)

source("./scripts/00_utilities/functions.R")
shp <- readRDS("./data/shapefiles_plotting.rds")
continents <- shp$continents 
countries <- shp$countries
all_shapes <- shp$admin %>% st_simplify(dTolerance = 5000)
rm(shp)

obs_temp <- readRDS("./data/dT_combined/scenarios_era_current_current.gz")
# population by country
pop <- readRDS("./data/all_pop2015.rds") %>% 
  filter(country != "CHN")
# calculate country averages to use later
country_temps <- obs_temp %>% 
  summarise(mean = mean(mean_2m_air_temperature_degree1), 
            .by = c(country, id)) %>% 
  left_join(pop) %>% 
  summarise(mean_temp = weighted.mean(mean, sum, na.rm = T), 
            .by = country) %>% 
  rbind(data.frame(country = "overall", mean_temp = -Inf))

dengue_units <- readRDS("./data/unit_covariates.rds") %>% 
  filter(mid_year == max(mid_year),
         .by = country)

# unit effects 
unit <- readRDS("./output/projection_ests/unit_changes_maxBoot100_mod_main_scenario_hist-nat.rds") %>% 
  left_join(country_temps, by = "country") %>% 
  mutate(id_join = case_when(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX") ~ stringi::stri_trans_general(id, id = "Latin-ASCII"), 
                             country == "BRA" ~ str_sub(id, 1, 8), 
                             country == "IDN" ~ case_when(id == "Dki Jakarta" ~ "Jakarta", 
                                                          id == "Daerah Istimewa Yogyakarta" ~ "Yogyakarta", 
                                                          # kalimantan utara gets assigned the dengue cases from timur, which is okay since we only care about whether dengue is > 0
                                                          id == "Kalimantan Utara" ~ "Kalimantan Timur", 
                                                          T ~ id), 
                             T ~ id) %>% tolower) %>% 
  filter(id_join != "br430000" & id != "[unknown]") %>% 
  left_join(dengue_units %>% select(id_join = id, country, sub_country_dengue), 
            multiple = "warning")  %>% 
  arrange(mean_temp) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country))) 

country <- readRDS("./output/projection_ests/country_changes_maxBoot100_mod_main_scenario_hist-nat.rds") %>% 
  filter(country != "overall_country_avg") %>% 
  mutate(country = gsub("_.*", "", country)) %>% 
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

# panel a: map 
test <- all_shapes %>% 
  filter(country != "LKA1") %>% 
  mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
  full_join(unit %>% filter(sub_country_dengue > 0)) 
{ggplot() + 
    geom_sf(data = continents,
            fill="grey90", colour="grey10") +
    geom_sf(data = test %>% mutate(pct_change_dengue_mean = pmax(pct_change_dengue_mean, -0.45)), 
            mapping = aes(fill = -pct_change_dengue_mean,
                          color = -pct_change_dengue_mean), 
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
                         labels = c("-15%", "0%", "15%", "30%", ">45%"),
                         breaks = c(-0.15, 0, 0.15, 0.3, 0.45),
                         na.value = "grey90") + 
    theme(panel.background = element_rect("white", NA), 
          legend.position = "inside", 
          legend.position.inside = c(0.6, 0.45))} -> unit_map 


# panel b: country changes ---- 
unit %>% 
  filter(sub_country_dengue > 0) %>% 
  mutate(country = factor(country, levels = c("overall", levels(country)))) %>%
  {rbind(.,
         mutate(., country = "overall"))} %>%
  mutate(n_units = n_distinct(id), 
         .by = country) %>% 
  {ggplot(data = ., 
          aes(y = country, 
              group = country, 
              color = country,
              fill = country)) +
      geom_vline(xintercept = 0, alpha = 0.4) + 
      geom_density_ridges(aes(x = -`pct_change_dengue_mean`, 
                              height = after_stat(ndensity), 
                              point_alpha = ifelse(country == "overall", 0, pmin(1.5/n_units^(1/3), 0.7))),
                          alpha = 0.2, color = alpha("grey20", 0.6),
                          scale = 0.9,
                          rel_min_height = 0.01,
                          bandwidth = 0.0381,
                          jittered_points = TRUE,
                          position = position_points_jitter(width = 0.005,
                                                            height = 0),
                          point_color = "black", 
                          point_shape = "—", point_size = 1.5) +
      geom_point(data = country, 
                 aes(x = -pct_inc_dengue_change_mean, 
                     y = as.numeric(country) + 0.1), 
                 size = 2) + 
      geom_linerange(data = country, 
                     aes(xmax = -pct_inc_dengue_change_q_0.025,
                         xmin = -pmin(pct_inc_dengue_change_q_0.975, 4),
                         y = as.numeric(country) + 0.1), 
                     linewidth = 1.15) +
    scale_color_manual(values = c("grey40", rev(MetBrewer::met.brewer("Hiroshige", 21))),
                       aesthetics = c("fill", "color")) +
    scale_x_continuous(labels = scales::percent, 
                       breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8),
                       limits = c(-.22, .7)) + 
    theme_classic() + 
    xlab("% dengue due to historical climate change") + 
    ylab("") + 
    coord_flip() + 
    theme(legend.position = "none", 
          axis.title.x = element_blank(),
          axis.text.x = element_text(angle = 35, hjust = 1),
          text = element_text(size = 14))} -> country_dist_plot


# panel b inset ----
# load model bootstrap fits
boot_ests <- readRDS("./output/mod_ests/main_coef_blockboot1000.rds") %>% 
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
      geom_line(data = summarise(., y = mean(y), .by = x), 
                aes(x = x,
                    y = y),
                color = "black") +
      geom_vline(data = country_temps %>% rename(current_mean = mean_temp) %>%
                   filter(country != "overall") %>% 
                   arrange(current_mean) %>%
                   mutate(country_orig = country,
                          country = factor(country, ordered = T,
                                           levels = unique(country))) ,
                 aes(xintercept = current_mean, color = country)) +
      geom_hline(yintercept = 0) + 
      theme_half_open(7) + 
      coord_cartesian(clip = "off") +
      xlab("temperature (°C)") + ylab("d log(dengue)/d T") +
      scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
                         aesthetics = c("color")) +
      theme(legend.position ="none", plot.margin = unit(c(10.5, 5.5, 5.5, 5.5), "points")) +
      scale_x_continuous(expand = expansion(mult = 0.0),
                         limits = c(15, 30.5)) +
      scale_y_continuous(expand = expansion(mult = 0.02))} -> inset_marginal_plot
# panel c ----
gbd_est %>% 
  left_join(pop %>% 
              summarise(pop = sum(sum), 
                        .by = country), 
            by = c("country_code" = "country")) %>% 
  left_join(country, 
            by = c("country_code" = "country_orig")) %>% 
  ggplot(aes(x = val/10, 
             color = country,
             y = -pct_inc_dengue_change_mean)) + 
  geom_point(aes(size = pop)) + 
  geom_linerange(aes(ymin = -pct_inc_dengue_change_q_0.025, 
                     ymax = -pct_inc_dengue_change_q_0.975)) + 
  geom_text_repel(aes(label = country),
                  point.padding = 2e-06,
                  size = 2,
                  seed = 9999,
                  color = "grey10") +
  xlab("estimated incidence (per 10k)") + 
  ylab("% dengue due to historical climate change") + 
  scale_y_continuous(labels = scales::percent) + 
  scale_color_manual(name = NULL,
                     values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
                     aesthetics = c("fill", "color"),
                     guide = 'none') +
  scale_size_continuous(name = "population",
                        # trans = "log",
                        breaks = c(5e6, 2.5e7, 2.5e8),
                        labels = c("5M", "25M", "250M")) + 
  theme_classic() + 
  theme(legend.position = "right", 
        text = element_text(size = 9.5),
        legend.key.size = unit(0.8, "lines"),
        legend.background = element_blank(), 
        legend.key = element_rect(fill = "transparent")) -> hist_plot

# combine panels ----
plot_grid(unit_map + 
            theme(plot.margin = unit(c(15.5, 0, 0, 0), "points")),
          plot_grid(
            ggdraw() +
              draw_plot(country_dist_plot +
                          theme(plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points"),
                                text = element_text(size = 10)),
                        0, 0, 1, 1) +
              draw_plot(inset_marginal_plot + theme(plot.margin = unit(c(2.5, 5.5, 2.5, 2.5), "points"),
                                                    text = element_text(size = 6.5)),
                        0.7, 0.68, width = 0.3, height = 0.32),
            hist_plot + 
              theme(legend.position = c(0.57, 0.16),
                    legend.title = element_text(size = 9),
                    plot.margin = unit(c(12.5, 5.5, 5.5, 5.5), "points")), 
            nrow = 1, 
            hjust = 0, label_x = 0.01,
            rel_widths = c(1, 0.75),
            vjust = 0.85,
            label_size = 12,
            labels = c("b) distribution within countries", 
                       "c) comparison to current dengue burden")), 
          ncol = 1, nrow = 2, rel_heights = c(0.82, 1),
          hjust = 0, label_x = 0.01, label_size = 12, 
          labels = c("a) estimated impact of historical warming (1995 - 2014)", 
                     '')) %>% 
            ggsave(filename = "./figures/figure4.pdf", width = 8*1.05, height = 5.75*1.05, 
                   bg = "white")
          

