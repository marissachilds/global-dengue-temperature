# TO DO 
# update shapefile to have LKA, not LKA1
# better color scheme...
# show values or ranges on bi legend
# lines going from countries to their time series? maybe do it in post processing 

library(tidyverse)
library(magrittr)
library(stringi)
library(sf)
library(cowplot)
library(biscale)
library(rworldmap)

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

# saveRDS(all_shapes, "./data/all_shapes_merged.rds")

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

# full_join(unit_covar %>% 
#             mutate(country = ifelse(country == "LKA1", "LKA", country)) %>%
#             filter(mid_year == max(mid_year),
#                   .by = country) %>% 
#             select(country, id, mid_year, sub_country_dengue),
#           unit_avgs %>% select(-pop, -nyear)) %>% 
#   View

country_ts <- dengue_temp %>% 
  summarise(cases = sum(dengue_cases, na.rm = T), 
            pop = sum(pop, na.rm = T), 
            .by = c(country, mid_year, year, month)) %>% 
  mutate(date = lubridate::ymd(paste0(year, "-", month, "-01")), 
         inc = cases/pop*10000) %>% 
  arrange(country, date) %>% 
  filter(cumsum(cases) > 0, 
         .by = c(country, mid_year)) 

# c("#8a1f13", "#cc8800", "#264354", 
#   "#e34938", "#ffcf71", "#447997", 
#   "#f4b6af", "#ffecc6", "#afcbdb")

# bipal <- c(
#   # "#c3b3d8", "#e6e6e6", "#ffcc80", "#7b67ab", "#bfbfbf", "#f35926", "#240d5e", "#7f7f7f", "#b30000") %>% rev %>%
#   # "#f3f3f3", "#f3e6b3", "#f3b300", "#b4d3e1", "#b3b3b3", "#b36600", "#509dc2",  "#376387", "#000000") %>% rev %>%
#   # "#8a1f13", "#c2ae00", "#264354",
#   # "#e34938", "#f2e466", "#447997",
#   # "#f4b6af", "#fff7b3", "#afcbdb")  %>%
#   # "#003701", "#00595d","#0077ae",
#   # "#644e02","#6a7e68", "#70a8c2",
#   # "#bc6202","#c89e71", "#d3d3d3") %>%
#   "#8a1f13", "#777777", "#264354",
#   "#e34938", "#9d9d9d", "#447997",
#   "#f4b6af", "#d9d9d9", "#afcbdb")  %>%
#   rev %>%
#   set_names(expand.grid(as.numeric(1:3),
#                         as.numeric(1:3)) %>%
#               unite("out", Var1, Var2, sep = "-") %>%
#               pull(out))
            
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
              # geom_sf(data = countries, color = "grey90", 
              #         lwd = 0.37, fill = NA) +
              bi_scale_fill(pal =  "DkViolet", #bipal, #"DkViolet", #bipal
                            # best color options: DkViolet, my palette, orange-blue-green, orange-grey-purple, blue-yellow-black
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
            0.86, 0.56, 0.16, 0.16) + 
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
            0.47, 0.47, 0.2, 0.2) + 
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
            0.6, 0.27, 0.2, 0.2) + 
  draw_plot(ggplot(country_ts %>% filter(country == "PHL"), 
                   aes(x = date, y = inc)) + 
              geom_line(aes(group = interaction(country, mid_year)),
                        lwd = 0.4) +
              ggtitle("Philippines") + 
              theme_classic() + 
              # labs(y = paste0("<span style='font-size: 5.5pt'>cases</span><span style='font-size: 3.5pt'>(thousands)</span>"), 
              #      x = "") +
              xlab("") + ylab("cases (per 10k)") +  
              theme(text = element_text(size = 7.5), 
                    title = element_text(size = 6),
                    axis.title.y = ggtext::element_markdown(),
                    axis.line = element_line(linewidth = 0.4),
                    plot.margin = unit(c(5.5, 0, -4, 3.5), "points")), 
            0.32, 0.29, 0.2, 0.2) + 
  draw_plot(ggplot(country_ts %>% filter(country == "VNM"), 
                   aes(x = date, y = inc)) + 
              geom_line(aes(group = interaction(country, mid_year)),
                        lwd = 0.4) +
              ggtitle("Vietnam") + 
              theme_classic() + 
              scale_y_continuous(limits = c(0, NA)) + 
              # labs(y = paste0("<span style='font-size: 6pt'>incidence</span><br><span style='font-size: 5pt'>(per 10,000)</span>"), 
              #      x = "") +
              xlab("") + ylab("cases (per 10k)") + 
              theme(text = element_text(size = 7.5), 
                    title = element_text(size = 6),
                    axis.title.y = ggtext::element_markdown(),
                    axis.line = element_line(linewidth = 0.4),
                    plot.margin = unit(c(5.5, 0, -4, 3.5), "points")), 
            0.22, 0.52, 0.2, 0.2)
ggsave("./figures/figure1_raw.png", 
       finalPlot, 
       width = 8, height = 5)

# TESTING ---- 
# can we add density plots to the legend? 
# bi_legend(pal = bipal, # "DkViolet", # 
#           dim = 3,
#           xlab = "Higher temperature ",
#           ylab = "Higher dengue  ",
#           size = 10) + 
#   scale_y_continuous(breaks = 0.5 + (0:3), 
#                      labels = c(">0", "0.25", "1", "28")) + 
#   theme(panel.background = element_rect(fill='transparent'), 
#         plot.margin = unit(c(0, 0, 0, 0), "points"), 
#         axis.text.y = element_text(margin = margin(r = -20, unit = "pt")))
# 
# y_density <- axis_canvas(bi_legend, axis = "y", coord_flip = TRUE) +
#   geom_density(data = unit_avgs, aes(x = mean_inc), color = NA, alpha = 0.5) +
#   coord_flip()
# 
# ggplot() + 
#   geom_density(data = unit_avgs %>% filter(mean_inc > 0), 
#                aes(x = pmin(mean_inc*10000, 2), 
#                    weight = pop), alpha = 0.5) +
#   geom_vline(xintercept = c(0.25, 1)) + 
#   theme_classic()
# 
# ggplot() + 
#   geom_density(data = unit_avgs %>% filter(mean_inc > 0), 
#                aes(x = pmin(mean_inc*10000, 2), 
#                    weight = pop), alpha = 0.5) +
# 
# unit_avgs %>% filter(mean_inc > 0) %>%   
#   {DescTools::Quantile(pull(., mean_inc) %>% multiply_by(10000), 
#                       weights = pull(., pop), probs = c(0, 1/3, 2/3, 1))}
# 
# 
# 
# insert_yaxis_grob(bi_legend, y_density, position = "right")

# Messing around with colors 
# bipal <- c(
#   "1-1" = "#c3b3d8", #"#d3d3d3", # low x, low y
#   "2-1" = "#e7e7e7", #"#ba8890",
#   "3-1" = "#ffcc80", #"#9e3547", # high x, low y
#   "1-2" = "#7b67ab",# "#8aa6c2",
#   "2-2" = "#bfbfbf", #"#7a6b84", # medium x, medium y
#   "3-2" = "#f35926", ##682a41",
#   "1-3" = "#240d5e", # "#4279b0", # low x, high y
#   "2-3" = "#7f7f7f", # "#3a4e78",
#   "3-3" = "#b30000" #"#311e3b" # high x, high y
# )
# # scales::show_col(c("#8a1f13", "#cc8800", "#264354", 
#                    "#E76254", "#FFDD9A", "#4C86A8", 
#                    "#f1a198", "#ffebc2", "#91b7cd"))

# mutate(mean_inc_factor = cut(mean_inc*10000, breaks = c(0, 0.25, 1, Inf),
# include.lowest = T)) %>% 
  

