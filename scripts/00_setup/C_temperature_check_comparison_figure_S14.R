library(tidyverse)
library(magrittr)
library(sf)


mid_rescaler <- function(mid = 0) {
  function(x, to = c(0, 1), from = range(x, na.rm = TRUE)) {
    scales::rescale_mid(x, to, from, mid)
  }
}

station_ll <- readRDS("./data/ghcn_station_data/study_station_inventory.rds")

ee_temps <- read.csv("./data/ghcn_comparison_data/ghcn_station_temps.csv") 
ee_temps %<>% mutate(date = as.Date(gsub(".*_", "", date), format = "%Y%m%d"))
# this can be slow because there are 526 files to read in
ghcn_temps <- list.files("./data/ghcn_station_data", full.names = T, 
                         pattern = "\\d") %>% 
  map(function(x){
    read_csv(x, show_col_types = FALSE, progress = FALSE) %>% 
      transmute(id = STATION,
                date = as.Date(DATE),
                elevation = ELEVATION,
                temp = TAVG, # tenths of degrees C
                temp_attr = as.character(TAVG_ATTRIBUTES)) %>%
      filter(!is.na(temp) & date >= as.Date("1979-01-01")) %>%
      return
  })

ghcn_temps %<>% list_rbind()

ghcn_temps %<>% mutate(temp = temp/10)

ghcn_temps %<>% left_join(ee_temps, by = c("id", "date")) 
rm(ee_temps)

library(fixest)
library(ggtext)
ghcn_temps %>% 
  rename(temp_ghcn = temp) %>% 
  mutate(temp_raw = temp_raw - 273.15) %>% 
  # slice_sample(n = 100000) %>% # for testing to make plotting run faster
  pivot_longer(c(temp_raw, temp_debias)) %>% 
  mutate(name = ifelse(name == "temp_debias", "Debiased temperature", "Raw temperature"),
         elev_group = ifelse(elevation < 1500 | is.na(elevation), "low elevation", "high elevation")) %>% 
  {ggplot(data = ., aes(x = temp_ghcn, y = value)) + 
      geom_point(aes(color = elevation), alpha = 0.4) + 
      geom_abline(slope = 1, intercept = 0, color = "blue") + 
      geom_richtext(data = nest_by(., name, elev_group) %>%
                      transmute(r2 = r2(feols(temp_ghcn ~ value, data = data), "r2"),
                                wr2 = r2(feols(temp_ghcn ~ value | id, data = data), "wr2"),
                                me = data %>% summarise(bias = mean(value - temp_ghcn, na.rm = T)) %>% pull(bias) %>% mean, 
                                rmse = data %>% summarise(rmse = sqrt(mean((value - temp_ghcn)^2, na.rm = T))) %>% pull(rmse) %>% mean) %>%
                      mutate(lab = paste0("R<sup>2</sup> = ", round(r2, 2),
                                          "<br>ME = ", round(me, 2), 
                                          "<br>RMSE = ", round(rmse, 2))),
                    aes(x = -15, y = 37, label = lab), hjust = 0, 
                    label.color = NA, fill = NA, size = 3) +
      facet_grid(elev_group ~ name) + 
      xlab("GHCN station temperature (°C)") + 
      ylab("ERA5 temperature (°C)") + 
      xlim(-20, 43) + ylim(-20, 43) + 
      scale_color_gradientn(name = "elevation (m)",
                            colors = cmocean::cmocean("matter")(20),
                            # rescaler = mid_rescaler(200), 
                            aesthetics = c("color")) + 
      theme_classic() + 
      theme(strip.background = element_blank(), 
            strip.text = element_text(face = "bold", size = 14))} %>% 
  ggsave(filename = "./figures/figureS8_temp_debias.png", 
         width = 8, height = 6)
  
