library(ggridges)
library(magrittr)
library(tidyverse)
library(ggrepel)
library(cowplot)

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
            .by = country)

dengue_units <- readRDS("./data/dengue_temp_full.rds") %>% 
  summarise(total_dengue = sum(dengue_cases, na.rm = T), 
            .by = c(country, mid_year, id)) %>% 
  # drop LKA1, filter to the max mid_year for each country 
  filter(country != "LKA1") %>% 
  filter(mid_year == max(mid_year), 
         .by = country) %>% 
  select(-mid_year)

# unit effects 
unit <- readRDS("./output/projection_ests/unit_changes_maxBoot25_mod_main_scenario_hist-nat.rds") %>% 
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
  full_join(dengue_units %>% rename(id_join = id), 
            multiple = "warning")  %>% 
  arrange(mean_temp) %>% 
  mutate(country_orig = country, 
         country = factor(country, ordered = T, 
                          levels = unique(country))) 

country <- readRDS("./output/projection_ests/country_changes_maxBoot25_mod_main_scenario_hist-nat.rds") %>% 
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

# panel a: country changes ---- 
unit %>% 
  filter(total_dengue > 0) %>% 
  # mutate(country = factor(country, levels = c(levels(country), "overall"))) %>% 
  # {rbind(., 
  #        mutate(., country = "overall"))} %>% 
  mutate(n_units = n_distinct(id), 
         .by = country) %>% 
  {ggplot(data = ., 
          aes(y = country, 
              group = country, 
              color = country,
              fill = country)) +
      geom_vline(xintercept = 0, alpha = 0.4) + 
      geom_density_ridges(aes(x = -`pct_change_dengue_q_0.5`, 
                              height = after_stat(ndensity), 
                              point_alpha = pmin(1.5/n_units^(1/3), 0.7)),
                                # pmin(1.5/log(n_units), 0.7)),
                          alpha = 0.2, color = alpha("grey20", 0.6),
                          # stat = "density", 
                          # trim = TRUE,
                          scale = 0.9,
                          rel_min_height = 0.01,
                          bandwidth = 0.0381,
                          jittered_points = TRUE,
                          position = position_points_jitter(width = 0.005,
                                                            height = 0),
                          point_color = "black", 
                          point_shape = "—", point_size = 1.5) +
      geom_point(data = country, 
                 aes(x = -pct_change_dengue_q_0.5, 
                     y = as.numeric(country) + 0.1), 
                 size = 2) + 
      geom_linerange(data = country, 
                     aes(xmax = -pct_change_dengue_q_0.025,
                         xmin = -pmin(pct_change_dengue_q_0.975, 4),
                         y = as.numeric(country) + 0.1), 
                     linewidth = 1.3) +
    scale_color_manual(values = c(rev(MetBrewer::met.brewer("Hiroshige", 21)), "grey40"),
                       aesthetics = c("fill", "color")) +
    # scale_y_discrete(expand = expansion(mult = c(0.03, 0.075))) +
    # scale_x_continuous(lim = c(-0.001, 1),
    # expand = expansion(mult = c(0.02, 0.05))) +
    scale_x_continuous(labels = scales::percent, 
                       breaks = c(-0.2, 0, 0.2, 0.4, 0.6, 0.8),
                       limits = c(-.22, .7)) + 
    theme_classic() + 
    xlab("estimated % dengue\ndue to existing climate change") + 
    ylab("") + 
    coord_flip() + 
    theme(legend.position = "none", 
          axis.text.x = element_text(angle = 25),
          text = element_text(size = 15))} -> country_dist_plot


# panel a inset ----
source("./scripts/00_functions.R")

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
# panel b ----
gbd_est %>% 
  left_join(pop %>% 
              summarise(pop = sum(sum), 
                        .by = country), 
            by = c("country_code" = "country")) %>% 
  left_join(country, 
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
  theme(legend.position = "right", 
        legend.background = element_blank()) -> hist_plot

# combine panels ----
plot_grid(ggdraw() + 
            draw_plot(country_dist_plot + 
                        theme(plot.margin = unit(c(20.5, 5.5, 5.5, 5.5), "points"),
                              text = element_text(size = 10)), 
                      0, 0, 1, 1) + 
            draw_plot(inset_marginal_plot + theme(plot.margin = unit(c(2.5, 5.5, 2.5, 2.5), "points"), 
                                                  text = element_text(size = 7)), 
                      0.74, 0.69, width = 0.26, height = 0.31),
          hist_plot, 
          nrow = 2,
          hjust = 0, label_x = 0.01,
          vjust = c(1.5, -0.2), label_size = 12,
          labels = c("a) impacts of existing climate change by country", 
                     "b) existing impacts compared to current dengue burden")) %>% 
  ggsave(filename = "./figures/figure4.png", width = 6, height = 7)


