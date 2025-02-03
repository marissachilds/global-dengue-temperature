library(tidyverse)
library(magrittr)
library(sf)

source("./scripts/00_utilities/functions.R")
continent_colors <- c("#466c4b","#90719f")
other_colors <- c("#ff6020", "#376795", "#efaa32") #"#ffba42") 
covars_vec <- c("continent", "sub_country_dengue", "health_expenditure", "pop_per_km2")
purrr::map(covars_vec, ~readRDS(paste0("output/mod_ests/het_", .x, "_tercile_coef_blockboot1000.rds"))) %>% 
  set_names(covars_vec) -> het_ests

temp_seq <- seq(0, 40, 0.1)

purrr::imap(het_ests, function(coefs, iname){
  coefs %<>% select(contains("temp"))
  purrr::map(unique(str_split_i(colnames(coefs), ":", 1)), 
             function(x_string){
               temp <- coefs %>% 
                 select(contains(x_string))
               temp_marg_mat <- colnames(temp) %>%
                 grep("temp", ., value = TRUE) %>% 
                 gsub(".*degree|_lag.", "", .) %>% 
                 as.numeric %>%
                 purrr::map(function(deg){
                   deg*temp_seq^(deg - 1)
                 }) %>% 
                 reduce(cbind) 
               temp %>% 
                 as.matrix %>% 
                 multiply_by_matrix(t(temp_marg_mat)) %>% 
                 set_colnames(paste0("temp_", temp_seq)) %>% 
                 as.data.frame() %>% 
                 mutate(boot_id = 1:n()) %>% 
                 pivot_longer(!boot_id, names_prefix = "temp_", 
                              names_transform = as.numeric,
                            names_to = "x", values_to = "y") %>% 
                 mutate(tercile = x_string, mod = iname) %>% return()
           }) %>% list_rbind %>% 
    return 
  }) %>% list_rbind -> boot_marg


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
  {ggplot(data = ., 
          aes(x = x, 
             group = interaction(panel, tercile), 
             color = tercile, fill = tercile)) + 
      geom_ribbon(data = boot_marg %>% rename(panel = mod) %>% 
                    summarise(upr = quantile(y, 0.975),
                              lwr = quantile(y, 0.025),
                              .by = c(x, tercile, panel)) %>% 
                    mutate(tercile = gsub(".*tercile", "", tercile)) %>% 
                    left_join(temp_by_tercile %>% 
                                summarise(xmax = quantile(x, 0.99), 
                                          xmin = quantile(x, 0.01), 
                                          .by = c(tercile, panel))) %>% 
                    filter(x > xmin & x < xmax)%>% 
                    mutate(tercile = ifelse(tercile %in% c("asia", "americas"), str_to_title(tercile), tercile)), 
                  aes(x = x, ymin = pmax(lwr + yoff, 0), 
                      ymax = pmin(upr + yoff, y_top + yoff), fill = tercile), 
                  alpha = 0.35, color = NA) + 
      geom_line(aes(y = pmin(y + yoff, y_top + yoff)), lwd = 1) + 
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
                     expand = expansion(mult = 0.0)) + 
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
                                panel = rep(sort(covars_vec), times = c(2, 3, 3, 3)), 
                                x = c(17, 17, 
                                      15, 16.3, 22,
                                      14, 15, 21.5,
                                      15.25, 14.8, 14), 
                                y = c(1.05, -0.3, 
                                      1.25, 0.59, 0.16, 
                                      1.52, 0.84, 0.37,
                                      1.4, 0.85, 0.4)), 
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
