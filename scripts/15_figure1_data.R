library(tidyverse)
library(magrittr)
library(stringi)
library(sf)
library(cowplot)
library(biscale)
library(rworldmap)
library(ggtext)

shp <- readRDS("./data/shapefiles_plotting.rds")
continents <- shp$continents 
countries <- shp$countries
all_shapes <- shp$admin
rm(shp)

all_shapes %<>% filter(country != "LKA1") %>%
  # clean up the ids on the shapefiles to match the ones in the dengue-temperature data, then merge
  mutate(id = tolower(id),
         id = ifelse(grepl("br\\d", id), substr(id, 1, 8), id) %>% 
           stri_trans_general(id="Latin-ASCII"), 
         # fix some ids in IDN
         id = case_match(id, 
                         "dki jakarta" ~ "jakarta", 
                         "daerah istimewa yogyakarta" ~ "yogyakarta", 
                         "kalimantan utara" ~ "kalimantan timur", 
                         .default = id)) 
test <- all_shapes %>% st_simplify(dTolerance = 5000)

dengue_temp <- readRDS("./data/dengue_temp_full.rds") %>% 
  mutate(dengue_inc = dengue_cases/pop) 

unit_covar <- readRDS("./data/unit_covariates.rds") 

unit_avgs <- dengue_temp %>% 
  # calculate annual averages
  summarize(mean_inc = mean(dengue_inc, na.rm=TRUE),
            mean_temp = mean(mean_2m_air_temp_degree1, na.rm=TRUE), 
            nobs = sum(!is.na(dengue_cases)),
            pop = unique(pop),
            .by = c(country, id, mid_year, year)) %>%
  # drop years with less than 12 obs 
  filter(nobs == 12) %>% 
  # and calculate period average for the remaining years 
  summarise(across(starts_with("mean"), mean), 
            nyear = n_distinct(year), 
            pop = unique(pop),
            .by = c(country, id, mid_year)) %>% 
  # for each country, filter to the latest mid year, recoding LKA1 = LKA 
  mutate(country = ifelse(country == "LKA1", "LKA", country)) %>%
  filter(mid_year == max(mid_year), 
         .by = country) %>% 
  left_join(unit_covar %>% 
              select(country, id, mid_year, gbd_scaled_dengue = sub_country_dengue))


country_ts <- dengue_temp %>% 
  summarise(cases = sum(dengue_cases, na.rm = T), 
            pop = sum(pop, na.rm = T), 
            .by = c(country, mid_year, year, month)) %>% 
  mutate(date = lubridate::ymd(paste0(year, "-", month, "-01")), 
         inc = cases/pop*10000) %>% 
  arrange(country, date) %>% 
  filter(cumsum(cases) > 0, 
         .by = c(country, mid_year)) 

# make own quantiles that are pop-weithed using DescTools::Quantile
finalPlot <- ggdraw() +
  draw_plot(ggplot() + 
              geom_sf(data = continents,
                      fill="white", colour="grey10") +
              geom_sf(data = test %>% 
                        mutate(country = ifelse(country == "LKA2", "LKA", country)) %>% 
                        left_join(unit_avgs, by = join_by(country, id)) %>% 
                        bi_class(x = mean_temp, 
                                 y = mean_inc, 
                                 style = "quantile", dim = 3), 
                      mapping = aes(fill = bi_class), 
                      color = NA, size = 0.01, show.legend = FALSE) + 
              geom_sf(data = countries, color = "grey10", 
                      lwd = 0.32, fill = NA) +
              annotate("text", x = -Inf, y = Inf, 
                       label = paste0(dengue_temp %>% 
                                        mutate(country = ifelse(country == "LKA1", "LKA", country)) %>%
                                        pull(country) %>% n_distinct(), " countries\n",
                                      unit_avgs %>% 
                                        filter(country != "LKA1" & gbd_scaled_dengue > 0) %>% 
                                        select(country, id) %>% n_distinct, " administrative units\n",
                                      dengue_temp %>% 
                                        filter(!is.na(dengue_inc)) %>% 
                                        left_join(unit_avgs %>% filter(country != "LKA1" & gbd_scaled_dengue > 0) %>% 
                                                    transmute(country, id, include = T)) %>% 
                                        filter(include) %>% 
                                        nrow %>% 
                                        divide_by(1e6) %>% round(2), "M unit-months"), 
                       size = 2.5, hjust = 0, vjust = 1) + 
              bi_scale_fill(pal =  "DkViolet", 
                            dim = 3, na.value="white") + 
              ylim(-34, 33) + 
              xlim(-92, 137) +
              theme_void() +
              theme(panel.background = element_rect("white", NA)), 
            0, 0, 1, 1) +
  draw_plot(bi_legend(pal = "DkViolet", #"DkViolet", #bipal, # 
                      dim = 3,
                      xlab = "Higher temperature ",
                      ylab = "Higher dengue  ",
                      size = 5) + 
              theme(panel.background = element_rect(fill='transparent'), 
                    plot.margin = unit(c(0, 0, 0, 0), "points")),
            0.81, 0.61, 0.27, 0.27) + 
  draw_plot(ggplot(country_ts %>% filter(country == "MEX"), 
                   aes(x = date, y = inc)) + 
              geom_line(aes(group = interaction(country, mid_year)),
                        lwd = 0.4) +
              ggtitle("Mexico") + 
              theme_classic() + 
              scale_y_continuous(limits = c(0, NA), 
                                 breaks = c(0, 0.5, 1)) + 
              xlab("") + ylab("cases (per 10k)") + 
              theme(text = element_text(size = 7.5), 
                    title = element_text(size = 6),
                    axis.line = element_line(linewidth = 0.4),
                    plot.margin = unit(c(5.5, 0, -2, 3.5), "points")), 
            0.46, 0.47, 0.2, 0.35) + 
  draw_plot(ggplot(country_ts %>% filter(country == "COL"), 
                   aes(x = date, y = inc)) + 
              geom_line(aes(group = interaction(country, mid_year)),
                        lwd = 0.4) +
              ggtitle("Colombia") + 
              theme_classic() + 
              scale_y_continuous(limits = c(0, NA)) + 
              xlab("") + ylab("cases (per 10k)") + 
              theme(text = element_text(size = 7.5), 
                    title = element_text(size = 6),
                    axis.line = element_line(linewidth = 0.4),
                    plot.margin = unit(c(5.5, 0, -2, 3.5), "points")), 
            0.6, 0.1, 0.2, 0.35) + 
  draw_plot(ggplot(country_ts %>% filter(country == "PHL"), 
                   aes(x = date, y = inc)) + 
              geom_line(aes(group = interaction(country, mid_year)),
                        lwd = 0.4) +
              ggtitle("Philippines") + 
              theme_classic() + 
              xlab("") + ylab("cases (per 10k)") +  
              theme(text = element_text(size = 7.5), 
                    title = element_text(size = 6),
                    axis.title.y = ggtext::element_markdown(),
                    axis.line = element_line(linewidth = 0.4),
                    plot.margin = unit(c(5.5, 0, -4, 3.5), "points")), 
            0.32, 0.15, 0.2, 0.35) + 
  draw_plot(ggplot(country_ts %>% filter(country == "VNM"), 
                   aes(x = date, y = inc)) + 
              geom_line(aes(group = interaction(country, mid_year)),
                        lwd = 0.4) +
              ggtitle("Vietnam") + 
              theme_classic() + 
              scale_y_continuous(limits = c(0, NA)) + 
              xlab("") + ylab("cases (per 10k)") + 
              theme(text = element_text(size = 7.5), 
                    title = element_text(size = 6),
                    axis.title.y = ggtext::element_markdown(),
                    axis.line = element_line(linewidth = 0.4),
                    plot.margin = unit(c(5.5, 0, -4, 3.5), "points")), 
            0.22, 0.52, 0.2, 0.35) 
ggsave("./figures/figure1.pdf", 
       finalPlot, 
       width = 8, height = 3)
