library(tidyverse)
library(magrittr)
library(sf)

source("./scripts/00_functions.R")
continent_colors <- c("#466c4b","#90719f")
other_colors <- c("#ff6020", "#376795", "#ffba42")# "#ffd07f")
het_ests <- readRDS("./output/mod_ests/het_models_coef_vcv.rds")
covars_vec <- c("continent", "sub_country_dengue", "health_expenditure", "pop_per_km2")
temp_seq <- seq(0, 40, 0.1)

unit_covar <- readRDS("./data/unit_covariates.rds")
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

dengue_temp %<>% left_join(unit_covar %>% select(country, id, mid_year, ends_with("tercile"))) %>% 
  filter(!is.na(dengue_cases) & !is.na(pop))

shp <- readRDS("./data/shapefiles_plotting.rds")
continents <- shp$continents 
countries <- shp$countries
all_shapes <- shp$admin
rm(shp)

all_shapes %<>% filter(country != "LKA1") %>%
  # clean up the ids on the shapefiles to match the ones in the dengue-temperature data, then merge
  mutate(id = tolower(id),
         id = ifelse(grepl("br\\d", id), substr(id, 1, 8), id) %>% 
           stringi::stri_trans_general(id="Latin-ASCII"), 
         # fix some ids in IDN
         id = case_match(id, 
                         "dki jakarta" ~ "jakarta", 
                         "daerah istimewa yogyakarta" ~ "yogyakarta", 
                         "kalimantan utara" ~ "kalimantan timur", 
                         .default = id)) 
all_shapes %<>% st_simplify(dTolerance = 5000)

test <- full_join(all_shapes, 
                  unit_covar %>% 
                    filter(mid_year == max(mid_year), 
                           .by = c(country, id)) %>% 
                    select(country, id, ends_with("tercile"), sub_country_dengue)) %>% 
  filter(sub_country_dengue > 0)

# het_marginal <- purrr::imap(het_ests[1], 
#             function(x, name){
#               marginal_est_se(x, "temp", temp_seq, "cluster", TRUE) %>% 
#                 mutate(mod = name) %>% 
#                 return
#             }) %>% 
#   extract2(1) %>% str

het_marginals <- het_ests %>% purrr::imap(function(x, name){
  terciles <- names(x$coef) %>% 
    str_split_i("\\:", i = 1) %>%
    unique %>% 
    grep(pattern = "tercile", value = T) 
  print(terciles)
  purrr::map(terciles, function(terc){
    marginal_est_se(coef_name_regex = terc, x_seq = temp_seq, 
                    debug = F, vcov_mat = x$vcov, coef_vec = x$coef)  %>% 
      mutate(tercile = terc) %>% 
      return
  }) %>% 
    list_rbind() %>%
    mutate(mod = name) %>% 
    return
}) %>% 
  list_rbind() 

het_marginals %>% 
  select(tercile, mod) %>% 
  unique

# construct df of temperature within each tercile 
covars_vec %>% 
  purrr::map(function(cov_name){
    dengue_temp %>% 
      filter(nonzero_dengue) %>% 
      transmute(country = country, 
                x = mean_2m_air_temp_degree1, 
                tercile = !!sym(paste0(cov_name, "_tercile")),
                panel = cov_name) %>% 
      filter(!is.na(tercile)) %>% # this just currently drops Taiwan in the health expenditure reg since it has no value 
      # filter(x > quantile(x, 0.01) & 
      #          x < quantile(x, 0.99), 
      #        .by = tercile) %>% 
      # filter(!is.na(dengue_inc) & !is.na(mean_2m_air_temp_degree1)) %>% 
      return
  }) %>% 
  list_rbind() -> temp_by_tercile

yoff <- 0.75
hist_scale = 2.05
y_top <- 1.65

fig3_main <- het_marginals %>% 
  separate_wider_delim(tercile, names = c("panel", "tercile"), 
                       delim = stringr::regex("_(?!.*_)"), too_many = "merge") %>% 
  mutate(tercile = gsub("rank_|tercile", "", tercile)) %>% 
  filter(panel %in% c("dengue") == FALSE) %>% 
  left_join(temp_by_tercile %>% 
              summarise(xmax = quantile(x, 0.99), 
                        xmin = quantile(x, 0.01), 
                        .by = c(tercile, panel))) %>% 
  filter(x > xmin & x < xmax) %>%
  mutate(tercile = ifelse(tercile %in% c("asia", "americas"), str_to_title(tercile), tercile)) %>% 
  # mutate(panel = case_match(panel, 
  #                           "DTR" ~ "daily temperature range", 
  #                           "continent" ~ "continent",
  #                           'CHE2010' ~ "health expenditure", 
  #                           "dengue" ~ "average dengue incidence", 
  #                           "density" ~ "population density") ) %>%
  {ggplot(data = ., 
          aes(x = x, 
             group = interaction(panel, tercile), 
             color = tercile, fill = tercile)) + 
  geom_ribbon(aes(ymin = pmax(y - 1.96*se + yoff, 0), 
                  ymax = pmin(y + 1.96*se + yoff, y_top + yoff)),
              alpha = 0.5, color = NA) + 
  geom_line(aes(y = pmin(y + yoff, y_top + yoff))) + 
  geom_hline(yintercept = yoff) + 
  geom_density(data = temp_by_tercile %>% mutate(tercile = ifelse(tercile %in% c("asia", "americas"), str_to_title(tercile), tercile)), 
               aes(y = hist_scale*after_stat(density)), 
               alpha = 0.5, trim = T) + 
  facet_wrap(~panel, labeller = as_labeller(c(sub_country_dengue = "Dengue incidence",
                                              continent = "Continent",
                                              health_expenditure = "Health expenditure per capita",
                                              pop_per_km2 = "Population density"))) +
  ylab("d log(dengue)/d temperature") + xlab("temperature (°C)") + 
  scale_color_manual(name = "",
                     aesthetics = c("fill", "color"),
                     values = c(continent_colors, other_colors)) +
  scale_y_continuous(labels = function(breaks){breaks - yoff},
                     breaks = pretty(c(-yoff, y_top)) + yoff,
                     expand = expansion(mult = 0.0),
                     limits = c(0, NA)) + 
  scale_x_continuous(limits = c(min(.$xmin), max(.$xmax)), 
                     expand = expansion(mult = 0.005)) + 
  theme_classic() + 
  theme(strip.background = element_blank(),
        strip.text = element_text(size = 12, face = "bold"),
        legend.position = "none")}

# map over different covariates and add the inset map
{fig3_main + 
    # add text labels for the lines in each panel
    geom_text(data = data.frame(tercile = c("Americas", "Asia", 
                                            "high", "mid", "low", 
                                            "high", "mid", "low", 
                                            "high", "mid", "low"), 
                                panel = rep(covars_vec, times = c(2, 3, 3, 3)), 
                                x = c(17, 18, 
                                      15.25, 17, 20,
                                      15, 16.3, 21,
                                      14, 15, 19), 
                                y = c(1.1, -0.3, 
                                      1.5, 0.87, 0.27,
                                      1.25, 0.59, 0.2, 
                                      1.52, 0.84, 0.37)), 
              aes(x = x, y = y + yoff, label = tercile, 
                  color = tercile, fontface = "bold"), 
              inherit.aes = FALSE) + 
    # add maps of the units in each tercile for each panel
    purrr::pmap(data.frame(covar_name = covars_vec, 
                           covar_pal = I(list(continent_colors, 
                                              other_colors,
                                              other_colors,
                                              other_colors))), 
                function(covar_name, covar_pal){
                  annotation_custom2(grob=ggplotGrob(make_tercile_map(paste0(covar_name, "_tercile"), 
                                                                      covar_pal, 
                                                                      test)), 
                                     data = data.frame(panel= covar_name),
                                     xmin = 18, xmax=31, ymin= 0.95 + yoff, ymax= 1.7 + yoff)
                })} %>% 
  ggsave(filename = "./figures/figure3.png", 
         width = 8, height = 6)
