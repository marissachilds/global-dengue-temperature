library(tidyverse)
library(magrittr)
library(fixest)

dengue_temp <- readRDS("./data/dengue_temp_full.rds")

# add lags of temperature
dengue_temp %<>% 
  arrange(country, mid_year, id, date) %>% 
  mutate(across(union(contains("temp"), contains("precipitation")), 
                list(lag1 =~ lag(.x, 1),
                     lag2 =~ lag(.x, 2),
                     lag3 =~ lag(.x, 3),
                     lag4 =~ lag(.x, 4))), 
         .by = c(country, mid_year, id)) %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(country, "_", id)) %>% 
  filter(!is.na(dengue_inc))
unit_covar <- readRDS("./data/unit_covariates.rds")


# first identify within each country, which places have average temps < 25 and > 25 year round 
unit_categories <- dengue_temp %>% 
  filter(country %in% c("MEX", "BRA", "VNM", "THA")) %>% 
  filter(mid_year == max(mid_year), .by = country) %>% 
  summarise(avg_month_temp = mean(mean_2m_air_temp_degree1), 
            .by = c(country, id, month)) %>% 
  summarise(min_temp = min(avg_month_temp), 
            max_temp = max(avg_month_temp), 
            .by = c(country, id)) %>% 
  mutate(temp_cat = case_when(max_temp < 25 ~ "< 25°C", 
                              min_temp > 25 ~ "> 25°C",
                              T ~ "mid") %>% 
           factor(levels = c("< 25°C", "mid", "> 25°C"), 
                  ordered = T)) %>% 
  # join on the unit covariates about which places have dengue 
  left_join(unit_covar %>% 
              filter(mid_year == max(mid_year), .by = country) %>% 
              select(country, id, empirical_dengue_incidence)) %>% 
  filter(empirical_dengue_incidence > 0) 
# now the country X temp_cat is our categorization 

dengue_temp %>% 
  filter(mid_year == max(mid_year), .by = country) %>% 
  left_join(unit_categories) %>% 
  filter(!is.na(temp_cat)) %>% 
  # calculate average dengue and temperature for each month-year in each category, we'll use 2 months lagged temperature
  summarise(dengue_cases = sum(dengue_cases), 
            temp = weighted.mean(mean_2m_air_temp_degree1_lag2, pop),
            pop = sum(pop), 
            n_admin = n_distinct(id),
            .by = c(country, temp_cat, year, month)) %>% 
  # calculate anomalies in dengue and temperature from seasonal average 
  mutate(temp_anom = temp - mean(temp), 
         dengue_anom = dengue_cases - mean(dengue_cases), 
         dengue_anom_z = (dengue_cases - mean(dengue_cases))/mean(dengue_cases), 
         .by = c(country, temp_cat, month)) %>% 
  # identify the "transmission season" for each category as 3 months per year where median dengue is highest and >0
  mutate(month_med_dengue = median(dengue_cases),
         .by = c(country, temp_cat, month)) %>% 
  mutate(season = month_med_dengue > 0 & month_med_dengue > quantile(month_med_dengue, 0.75), 
         .by = c(country, temp_cat)) %>% 
  # limit to activity season
  filter(season) %>%
  {ggplot(data = ., aes(x = temp_anom, y = dengue_anom_z)) + 
      geom_hline(yintercept = 0, color = "grey40", lwd = 0.2) + 
      geom_point(aes(color = temp), alpha = 0.35) + 
      geom_line(data = nest_by(., country, temp_cat) %>%
                    mutate(slope = lm(dengue_anom_z ~ -1 + temp_anom, data) %>% coef %>% extract("temp_anom"), 
                           se = lm(dengue_anom_z ~ -1 + temp_anom, data) %>% se %>% extract("temp_anom"))  %>%
                    unnest(data) %>% 
                    mutate(pred = slope*temp_anom), 
                  aes(x = temp_anom, y = pred), color = "black", lwd = 1) + 
      geom_ribbon(data = nest_by(., country, temp_cat) %>%
                    mutate(slope = lm(dengue_anom_z ~ -1 + temp_anom, data) %>% coef %>% extract("temp_anom"), 
                           se = lm(dengue_anom_z ~ -1 + temp_anom, data) %>% se %>% extract("temp_anom"))  %>%
                    unnest(data) %>% 
                    mutate(min = pmin((slope + qnorm(0.975)*se)*temp_anom,
                                      (slope + qnorm(0.025)*se)*temp_anom),
                           max = pmax((slope + qnorm(0.975)*se)*temp_anom,
                                      (slope + qnorm(0.025)*se)*temp_anom),
                           pred = slope*temp_anom),
                  aes(x = temp_anom, ymin = min, ymax = max), 
                  color = NA, alpha = 0.2) + 
      geom_text(data = select(., country, temp_cat, n_admin, pop) %>% distinct, 
                aes(label = paste0(n_admin, " units\n", round(pop/1e6, 2), "M pop"), 
                    x = -Inf, y = Inf), 
                hjust = -0.1, vjust = 1) + 
      facet_grid(country ~ temp_cat, scales = "free") + 
      scale_color_viridis_c(option = "C", name = "monthly\ntemp (°C)") + 
      scale_y_continuous(labels = scales::percent_format()) + 
      xlab("anomaly from seasonal average (°C)") + 
      ylab("% change in dengue from seasonal average") + 
      theme_classic() + 
      theme(strip.background = element_blank(), 
            strip.text = element_text(face = "bold", size = 12))} %>% 
  ggsave(filename = "./figures/figureSX_anomalies_comparison.png", 
         width = 9, height = 7)