library(tidyverse)
library(countrycode)
library(cowplot)
library(rnaturalearth)
library(magrittr)

gbd <- readxl::read_excel("./data/Global Burden Disease dataset.xls")  %>% 
  filter(measure == "Incidence") %>% 
  filter(val > 0) %>% 
  mutate(gee_name = case_match(location, 
                               "American Samoa" ~ "American Samoa (US)", 
                               "Antigua and Barbuda" ~ "Antigua & Barbuda", 
                               "Cape Verde" ~ "Cabo Verde", 
                               "Central African Republic" ~ "Central African Rep",
                               "Congo" ~ "Congo, Rep of the",
                               "Democratic Republic of the Congo" ~ "Congo, Dem Rep of the",
                               "Federated States of Micronesia" ~ "Micronesia, Fed States of", 
                               "Marshall Islands" ~ "Marshall Is",
                               "Myanmar" ~ "Burma",
                               "Northern Mariana Islands" ~ "Northern Mariana Is (US)",
                               "Puerto Rico" ~ "Puerto Rico (US)",
                               "Saint Lucia" ~ "St Lucia",
                               "Saint Vincent and the Grenadines" ~ "St Vincent & the Grenadines", 
                               "Solomon Islands" ~ "Solomon Is", 
                               "Taiwan (Province of China)" ~ "Taiwan", 
                               "The Bahamas" ~ "Bahamas, The",
                               "The Gambia" ~ "Gambia, The",
                               "Trinidad and Tobago" ~ "Trinidad & Tobago", 
                               "Virgin Islands, U.S." ~ "US Virgin Is (US)", 
                               .default = location))

# add iso3 code as a column to gbd
gbd %<>% mutate(country_name = countrycode(location, "country.name", "iso3c"),
         val = val * 0.1)  # Convert cases per 100k to per 10k

# Add column for countries in our dataset, dengue = 1 for specified countries
dengue_countries <- c("BRA", "COL", "LKA", "PER", "PHL", "TWN", "PAN", "NIC", "THA", "HND", "VEN", "BOL", "MEX", "KHM", "MYS", "VNM", "LAO", "SLV", "IDN", "DOM", "CRI")
gbd %<>% mutate(dengue = ifelse(country_name %in% dengue_countries, 1, NA))

# Load world map data
world <- ne_countries(scale = "medium", returnclass = "sf")

# Merge your data with the world map data
world %<>% full_join(gbd, by = c("iso_a3" = "country_name"))

# Create the plot
map_plot = ggplot(data = world) +
  geom_sf(aes(fill = val), color = NA) +  # Adjust aes(fill = ...) as needed
  geom_sf(fill = NA, color = "gray") +
  geom_sf(data = subset(world, dengue == 1), fill = NA, color = "black", lwd = 0.7) +
  labs(fill = "cases \nper 10k") +  # Adjust labels as needed
  coord_sf(xlim = c(-115,150), ylim = c(-55,50))+
  scale_fill_gradientn(colors = cmocean::cmocean("rain", end = 0.7)(20),
                       aesthetics = c("fill", "color"),
                       na.value = "grey60") + 
  theme_void() +
  theme(
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.border = element_blank(),  # Hide the border lines
    panel.background = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    plot.margin = margin(0.1, 0, 0, 0, "cm") 
  )

dens_plot = read.csv("./data/endemic_dengue_country_temperatures.csv") %>% 
  mutate(in_sample = COUNTRY_NA %in% c("Bolivia", 
                                       "Brazil", 
                                       "Colombia", 
                                       "Costa Rica", 
                                       "Dominican Republic", 
                                       "Honduras", 
                                       "Indonesia", 
                                       "Cambodia", 
                                       "Laos", 
                                       "Sri Lanka",
                                       "Mexico", 
                                       "Malaysia", 
                                       "Nicaragua", 
                                       "Panama", 
                                       "Peru", 
                                       "Philippines", 
                                       "El Salvador",
                                       "Thailand", 
                                       "Taiwan", 
                                       "Venezuela", 
                                       "Vietnam"),
         in_sample = ifelse(in_sample, "study countries", "other endemic countries")) %>% 
  left_join(gbd, by = c("COUNTRY_NA" = "gee_name")) %>% 
  filter(val > 10) %>% 
  ggplot(aes(x = mean, group = in_sample, 
             fill = in_sample,
             color = in_sample)) + 
  geom_density(alpha = 0.5, adjust = 1/2) + 
  scale_color_manual(values = c("black", "#20856EFF"), 
                     aesthetics = c("color", "fill")) + 
  theme_half_open() + 
  theme(legend.title = element_blank(), 
        legend.position = c(0.075, 0.82), 
        legend.key.size = unit(12, "points"),
        text = element_text(size = 8),
        axis.text = element_text(size = 6)) + 
  xlab("mean temperature (°C)")

{ggdraw() + 
    draw_plot(map_plot, 0, 0.1, 1, 0.9) + 
    draw_plot(dens_plot, 0.5, 0, 0.3, 0.4)} %>% 
  ggsave(filename = "./figures/figureS1_endemic_countries.png", 
         ., width = 8, height = 3.75, bg = "white")

