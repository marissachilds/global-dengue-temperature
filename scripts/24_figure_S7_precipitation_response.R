library(tidyverse)
library(magrittr)
library(cowplot)
library(fixest)

source("./scripts/00_utilities/functions.R")

# load dengue data 
dengue_temp <- readRDS("./data/dengue_temp_full.rds") %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(countryFE, "_", id)) %>%
  filter(!is.na(dengue_inc))

# do we see a precipitation response? 
mod_ests <- readRDS("./output/mod_ests/all_models.rds")

precip_seq <- dengue_temp$total_precipitation %>% quantile(c(0.0, 0.99), na.rm = T) %>% 
  {seq(from = .[1], to = .[2], length.out = 100)}
mod_resp <- purrr::imap(mod_ests[c("main", "precip_sq")], 
                        function(x, name){
                          response_est_se(coef_name_regex = "precip", x_seq = precip_seq,
                                          vcov_mat = vcov_cluster(x, "countryFE"),
                                          coef_vec = coef(x), degree_regex = "\\^",
                                          debug = TRUE) %>%
                            mutate(mod = name) %>% 
                            return
                        }) %>% 
  list_rbind

yoff = 0.32
{ggplot(mod_resp) + 
    geom_histogram(data = dengue_temp %>% 
                     filter(total_precipitation < quantile(total_precipitation, 0.99, na.rm = T)), 
                   aes(x = total_precipitation, 
                       y = after_stat(density)*0.001),
                   inherit.aes = FALSE, boundary = 0, color = "white", 
                   fill = "grey40") + 
    geom_line(aes(x = x, y = y + yoff, group = mod, color = mod)) + 
    geom_ribbon(aes(x = x, 
                    group = mod, fill = mod,
                    ymin = y + qnorm(0.025)*se + yoff, 
                    ymax = y + qnorm(0.975)*se + yoff), color = NA, alpha = 0.1) + 
    scale_y_continuous(breaks = seq(-0.3, 1.5, by = 0.3) + yoff, 
                       labels = function(x){x - yoff}, 
                       expand = expansion(mult = 0.01)) +
    scale_color_manual(values = c("black", "blue"), 
                       labels = c("Main (linear)", "Quadratic"),
                       aesthetics = c("color", "fill")) + 
    xlab("avg monthly precipitation (m)") + 
    ylab("relative log(dengue)") + 
    theme_classic() + 
    theme(legend.position = "inside",
          legend.title = element_blank(),
          legend.position.inside = c(0.2, 0.8))} %>% 
  ggsave(filename = "figures/figureS7_precipitation_response.png", width = 5, height = 3)
