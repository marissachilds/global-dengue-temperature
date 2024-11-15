library(tidyverse)
library(magrittr)
library(cowplot)
library(fixest)

# SET UP ---- 
# load functions 
source("./scripts/00_functions.R")

# load dengue data 
dengue_temp <- readRDS("./data/dengue_temp_full.rds") %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(countryFE, "_", id)) %>%
  filter(!is.na(dengue_inc))

# load model fits
mod_ests <- readRDS("./output/mod_ests/all_models.rds")
boot_ests <- readRDS("./output/mod_ests/main_coef_boot1000.rds") %>% 
  select(contains("temp"))

# load temperature data for current period to use for coloring the country lines 
# calculate country averages to use later
country_temps <- readRDS("./data/dT_combined/scenarios_era_current_current.gz") %>% 
  summarise(mean = mean(mean_2m_air_temperature_degree1), 
            .by = c(country, id)) %>% 
  left_join(readRDS("./data/all_pop2015.rds") %>% 
              filter(country != "CHN")) %>% 
  summarise(mean_temp = weighted.mean(mean, sum, na.rm = T), 
            total_pop = sum(sum),
            .by = country)

temp_seq <- seq(0, 40, 0.1)
mod_marg <- purrr::imap(mod_ests, 
                         function(x, name){
                           marginal_est_se(x, "temp", temp_seq, "cluster", FALSE) %>% 
                             mutate(mod = name) %>% 
                             return
                         }) %>% 
  list_rbind

mod_resp <- purrr::imap(mod_ests, 
                        function(x, name){
                          response_est_se(x, "temp", temp_seq, "cluster", FALSE) %>% 
                            mutate(mod = name) %>% 
                            return
                        }) %>% 
  list_rbind


# construct a temperature matrix with the correct degrees to multiply by the bootstrapped coefs
temp_resp_mat <- colnames(boot_ests) %>%
    grep("temp", ., value = TRUE) %>% 
    gsub(".*degree|_lag.", "", .) %>% 
    as.numeric %>%
    purrr::map(function(deg){
      temp_seq^deg
    }) %>% 
    reduce(cbind) 
temp_marg_mat <- colnames(boot_ests) %>%
  grep("temp", ., value = TRUE) %>% 
  gsub(".*degree|_lag.", "", .) %>% 
  as.numeric %>%
  purrr::map(function(deg){
    deg*temp_seq^(deg - 1)
  }) %>% 
  reduce(cbind) 
  
    
boot_resp <- as.matrix(boot_ests) %*% t(temp_resp_mat) %>% 
  set_colnames(paste0("temp_", temp_seq)) %>% 
  as.data.frame() %>% 
  mutate(boot_id = 1:n()) %>% 
  pivot_longer(!boot_id, names_prefix = "temp_", 
               names_transform = as.numeric,
               names_to = "x", values_to = "y")

boot_marg <- as.matrix(boot_ests) %*% t(temp_marg_mat) %>% 
  set_colnames(paste0("temp_", temp_seq)) %>% 
  as.data.frame() %>% 
  mutate(boot_id = 1:n()) %>% 
  pivot_longer(!boot_id, names_prefix = "temp_", 
               names_transform = as.numeric,
               names_to = "x", values_to = "y")

# country average temperatures for annotating the figure 
# country_temp_ranges <- dengue_temp %>% 
#   filter(!is.na(dengue_cases)) %>%
#   # for countries in the sample twice, keep the longer time range
#   mutate(nt = n_distinct(paste0(year, "_", month)), 
#          .by = c(country, mid_year)) %>% 
#   filter(nt == max(nt), 
#          .by = country) %>% 
#   summarise(mean_temp = mean(mean_2m_air_temp_degree1, na.rm = T), 
#             pop = unique(pop),
#             .by = c(id, country)) %>% 
#   summarise(mid = weighted.mean(mean_temp, pop), 
#             mid_raw = mean(mean_temp),
#             min = min(mean_temp), 
#             max = max(mean_temp), 
#             pop = sum(pop),
#             .by = country) %>% 
#   mutate(label = (mid == min(mid) | mid == max(mid) | 
#                     min == min(min) | max == max(max) | 
#                     rank(-pop) <= 2))

# figure 2 ---- 
# bootstrapped CIs from main, with all estimates
# calculate mean temperature in the sample, for use in relative response curve
temp_mean = dengue_temp %>% 
  filter(nonzero_dengue) %>% 
  # {mean(.$mean_2m_air_temp_degree1)}
  {weighted.mean(.$mean_2m_air_temp_degree1, .$pop)} %>% 
  round(1)

yoff <- 0.5
hist_scale <- 2.75
# mods_shown = data.frame(mod = c("main","lag4", "poly4",  "country_mos_FE", "no_brazil", "no_pop_weight"), 
#                         mod_clean = c("main", "1 - 4 month lags", "4th order polynomial", "month of sample control", "no Brazil", "unweighted")) %>% 
#   mutate(mod_clean = factor(mod_clean, levels = unique(mod_clean), ordered = T))
                        
{plot_grid(mod_resp %>% 
             filter(mod %in% c("pop_offset", "poly2", "country_trend") == FALSE) %>% 
             # filter(x >= 9 & x<= 33) %>%
             filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                      x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
             # left_join(mods_shown) %>% 
             # filter(!is.na(mod_clean)) %>%
             mutate(y = y - y[x == temp_mean],
                    .by = mod) %>% 
             arrange(mod == "main") %>% 
             filter(mod == "main") %>% 
             ggplot(aes(x = x, 
                        y = y,
                        linewidth = I(ifelse(mod == "main", 1, 0.5)),
                        alpha = I(ifelse(mod == "main", 1, 0.3)),
                        group = mod)) + 
             geom_vline(data = country_temps %>% 
                          arrange(mean_temp) %>% 
                          mutate(country = factor(country, ordered = T, 
                                                  levels = unique(country))),
                        aes(xintercept = mean_temp, color = country), 
                        lwd = 0.4) +
             geom_line() + 
             geom_ribbon(data = boot_resp %>% 
                           # filter(x >= 9 & x<= 33) %>%
                           filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                                    x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
                           mutate(y = y - y[x == temp_mean],
                                  .by = boot_id) %>% 
                           summarise(ymin = quantile(y, 0.025), 
                                     ymax = quantile(y, 0.975), 
                                     .by = x),
                         aes(x = x, ymin = ymin, ymax = ymax), 
                         color = NA, fill = "black",
                         alpha = 0.4, inherit.aes = FALSE) + 
             geom_text(data = country_temps %>%
                         filter(mean_temp == max(mean_temp) | mean_temp == min(mean_temp) | 
                                  rank(-total_pop) <= 3) %>%
                         mutate(country = paste0("  ", country, "  ")),
                       aes(x = mean_temp,
                           y = Inf,
                           label = country),
                       vjust = -0.3,
                       size = 2.25,
                       inherit.aes = FALSE) +
             coord_cartesian(clip = "off") +
             theme_classic() + 
             scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
                                aesthetics = c("color")) +
             theme(legend.position = "none",
                   legend.key.size = unit(10, "points"),
                   legend.background = element_blank(), 
                   plot.margin = unit(c(22.5, 5.5, 2.5, 5.5), "points")) +
             guides(color = guide_legend(override.aes = list(linewidth = 1))) + 
             # scale_color_manual(name = "", 
             #                    values = c("grey10", "red"),
             #                               # MetBrewer::met.brewer("Juarez", nrow(mods_shown) - 1)),
             #                    aesthetics = c("color", "fill")) +
             xlab("temperature (°C)") + ylab("relative log(dengue)") + 
             scale_x_continuous(expand = expansion(mult = 0.0)) + 
             scale_y_continuous(expand = expansion(mult = 0.02), limits = c(-10.5, NA)), 
           mod_marg %>% 
             filter(mod %in% c("pop_offset", "poly2", "country_trend") == FALSE) %>% 
             # filter(mod %in% mods_shown) %>%
             # mutate(mod = factor(mod, levels = mods_shown, ordered = T)) %>% 
             # left_join(mods_shown) %>% 
             # filter(!is.na(mod_clean)) %>%
             # filter(x >= 9 & x<= 33) %>% 
             filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                      x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
             arrange(mod != "main") %>% 
             mutate(mod = factor(mod, levels = unique(mod), ordered = T)) %>% 
             ggplot(aes(x = x, 
                        y = y + yoff, 
                        linewidth = I(ifelse(mod == "main", 1, 0.4)),
                        alpha = I(ifelse(mod == "main", 1, 0.8)),
                        group = mod)) + 
             geom_hline(yintercept = 0 + yoff) + 
             # geom_vline(data = country_temps %>% 
             #              arrange(mean_temp) %>% 
             #              mutate(country = factor(country, ordered = T, 
             #                                      levels = unique(country))),
             #            aes(xintercept = mean_temp, color = country)) +
             geom_line(aes(color = mod)) +  # aes(color = I(ifelse(mod == "main", "black", "grey")))
             geom_ribbon(data = boot_marg %>%
                           # filter(x >= 9 & x<= 33) %>%
                           filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                                    x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
                           summarise(ymin = quantile(y, 0.025),
                                     ymax = quantile(y, 0.975),
                                     .by = x),
                         aes(x = x, ymin = pmax(ymin, -yoff) + yoff, ymax = ymax + yoff),
                         color = NA, fill = "black",
                         alpha = 0.4, inherit.aes = FALSE) +
             geom_histogram(data = dengue_temp %>% 
                              filter(!is.na(dengue_inc) & !is.na(mean_2m_air_temp_degree1)) %>% 
                              filter(mean_2m_air_temp_degree1 > quantile(mean_2m_air_temp_degree1, 0.01) & 
                                       mean_2m_air_temp_degree1 < quantile(mean_2m_air_temp_degree1, 0.99)) %>% 
                              mutate(type = "marginal"),
                            aes(x = mean_2m_air_temp_degree1, 
                                y = after_stat(density)*hist_scale),
                            breaks = dengue_temp %>% 
                              filter(!is.na(dengue_inc) & !is.na(mean_2m_air_temp_degree1)) %>% 
                              pull(mean_2m_air_temp_degree1) %>% 
                              quantile(c(0.01, 0.99)) %>% 
                              {seq(from = .[1], to = .[2], length.out = 30)},
                            color = "white", fill = "grey65",
                            inherit.aes = FALSE) +
             theme_classic() + 
             coord_cartesian(clip = "off") +
             scale_color_manual(name = "", 
                                values = c("grey10",
                                           rep("grey60", 11)),
                                # MetBrewer::met.brewer("Juarez", 11)
                                aesthetics = c("color", "fill")) +
             # scale_color_manual(values = rev(MetBrewer::met.brewer("Hiroshige", 21)),
             #                    aesthetics = c("color")) +
             # scale_color_manual(values = c("grey10", "red"),
             #                               # MetBrewer::met.brewer("Juarez", nrow(mods_shown) - 1)),
             #                    aesthetics = c("color", "fill")) +
             xlab("temperature (°C)") + ylab("d log(dengue)/d temp") + 
             scale_x_continuous(expand = expansion(mult = 0.0), 
                                limits = dengue_temp %>% 
                                  filter(!is.na(dengue_inc) & !is.na(mean_2m_air_temp_degree1)) %>% 
                                  pull(mean_2m_air_temp_degree1) %>% 
                                  quantile(c(0.01, 0.99))) + 
             scale_y_continuous(labels = function(breaks){breaks - yoff},
                                breaks = c(-0.5, 0, 0.5, 1, 1.5, 2) + yoff,
                                expand = expansion(mult = 0.01),
                                limits = c(0, 1.6 + yoff)) + 
             guides(color=guide_legend(ncol=2)) + 
             theme(legend.position = "none", #c(0.72, 0.87), 
                   legend.text = element_text(size = 4), 
                   legend.key.height = unit(2, "points"),
                   plot.margin = unit(c(12.5, 5.5, 2.5, 5.5), "points")), 
           nrow = 2, 
           labels = c("a) dengue-temperature response",  "b) marginal effect"), 
           hjust = 0, label_x = 0.01, label_size = 12, vjust = c(1.5, 0.75),
           align = "v", 
           axis = "lr")} %>% 
  ggsave(filename = "./figures/figure2.png", width = 4, height = 5)


# figure S4 ----
# panel a) other specs, b) bootstrapped + analytic CIs for main
# compare bootstrapped CIs with analytic
plot_grid(boot_marg %>% 
            filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                     x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
            {ggplot(data = .,) + 
                geom_line(aes(group = boot_id, x = x, y = y),
                          alpha = 0.01) +
                geom_ribbon(data = summarise(., ymin = quantile(y, 0.025), 
                                             ymax = quantile(y, 0.975), 
                                             .by = x),
                            aes(x = x, ymin = ymin, ymax = ymax), 
                            color = NA, fill = "grey10", alpha = 0.2) +
                geom_ribbon(data = mod_marg %>% 
                              filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                                       x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
                              filter(mod == "main") %>% 
                              filter(x >= 10 & x<= 32.5),
                            aes(x = x, ymin = y - 1.96*se, ymax = y + 1.96*se), 
                            color = NA, fill = "red", alpha = 0.2) + 
                geom_line(data = boot_marg %>% 
                            filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                                     x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
                            summarise(y = mean(y),
                                      .by = x),
                          aes(x = x, y = y), 
                          linewidth = 1.5,
                          color = "grey10") +
                geom_line(data = mod_marg %>% 
                            filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                                     x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
                            filter(mod == "main"), 
                          aes(x = x, y = y), 
                          linewidth = 1.5, 
                          color = "red") + 
                annotate("text", x = 15, y = 0.5, color = "red", 
                         label = "analytic estimate") + 
                annotate("text", x = 16, y = 1.5, color = "grey10", 
                         label = "bootstrapped estimate") + 
                geom_hline(yintercept = 0) +
                xlab("temperature (°C)") + ylab("d log(dengue)/d temp") + 
                theme_classic() + 
                theme(plot.margin = unit(c(15.5, 5.5, 5.5, 5.5), "points"))}, 
          mod_marg %>%  
            filter(x >= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.01) & 
                     x <= quantile(dengue_temp$mean_2m_air_temp_degree1, 0.99)) %>% 
            filter(mod %in% c("pop_offset", "poly2", "country_trend") == FALSE) %>% 
            cross_join(., 
                       summarise(., .by = mod) %>% 
                         rename(panel = mod)) %>% 
            mutate(se = ifelse(mod == panel, se, NA), 
                   col = ifelse(mod == panel, mod, "other")) %>% 
            mutate(panel = case_match(panel, 
                                      "country_mos_FE" ~ "country-month-year\nFEs", 
                                      "lag2" ~ "1 - 2 month lags", 
                                      "lag3_w_lag0" ~ "0 - 3 month lags", 
                                      "lag4" ~ "1 - 4 month lags", 
                                      "main" ~ "main specification", 
                                      "no_brazil" ~ "no Brazil", 
                                      "no_pop_weight" ~ "unweighted", 
                                      "no_precip" ~ "no precipitation\ncontrol", 
                                      "poly4" ~ "4th order polynomial", 
                                      "poly5" ~ "5th order polynomial", 
                                      "precip_sq" ~ "2nd order precipitation\ncontrol", 
                                      "unit_season" ~ "unit-month FEs")) %>% 
            ggplot(aes(x = x, 
                       ymin = y - 1.96*se, 
                       ymax = pmin(y + 1.96*se, 2.6), 
                       y = y, 
                       alpha = I(ifelse(col == "other", 0.45, 1)),
                       linewidth = I(ifelse(col == "other", 0.6, 1)),
                       color = col, 
                       group = mod, 
                       fill = col)) + 
            geom_ribbon(alpha = 0.45, color = NA) + 
            geom_line() + 
            # geom_histogram(data = dengue_temp %>% filter(!is.na(dengue_inc)), 
            #                aes(x = mean_2m_air_temp_degree1), 
            #                inherit.aes = FALSE) + 
            geom_hline(yintercept = 0) + 
            facet_wrap(~panel, nrow = 3) + 
            theme_classic() + 
            scale_color_manual(values = c(MetBrewer::met.brewer("Austria", 14)[1:8],
                                          "grey10",
                                          MetBrewer::met.brewer("Austria", 14)[9:12]),
                               aesthetics = c("color", "fill")) +
            xlab("temperature (°C)") + ylab("d log(dengue)/d temp") + 
            theme(legend.position = "none", 
                  plot.margin = unit(c(15.5, 5.5, 5.5, 5.5), "points"),
                  strip.background = element_blank(),
                  strip.text = element_text(face = "bold")), 
          nrow = 2, hjust = 0, label_x = 0.01, vjust = c(1.2, 1.5),
          label_size = 12,
          labels = c("a) comparison of bootstrapped and analytic confidence intervals",
                     "b) marginal response under different modeling choice"), 
          rel_heights = c(0.8, 1)) %>% 
  ggsave(filename = "./figures/figureS4.png", height = 9, width = 6)

# table S3 with coefficients under different models
# readRDS("./output/mod_ests/robust_check_coef_vcv.rds") %>% 
#   purrr::imap(function(x, name){
#     data.frame(coef = names(x$coef), 
#                est = x$coef, 
#                se = diag(x$vcov), 
#                mod = name) 
#   }) %>% 
#   list_rbind() %>% 
#   View

mod_ests <- readRDS("./output/mod_ests/all_models.rds")
dengue_temp <- readRDS("./data/dengue_temp_full.rds") %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(countryFE, "_", id)) %>%
  filter(!is.na(dengue_inc))

temp_variable_names = rbind(expand.grid(deg = 1:5, lag = 0:4) %>% 
                              mutate(in_mod = paste0("mean_2m_air_temp_degree", deg, ifelse(lag > 0, paste0("_lag", lag), "")), 
                                     in_table = paste0("temp", ifelse(deg > 1, paste0("$^", deg, "$"), ""), 
                                                       ", ", ifelse(lag > 0, paste0("lag ", lag), "current"))),
                            expand.grid(deg = 1:2, lag = 0:4) %>% 
                              mutate(in_mod = paste0(ifelse(deg > 1, "I(", ""), 
                                                     "total_precipitation", ifelse(lag > 0, paste0("_lag", lag), ""), 
                                                     ifelse(deg > 1, paste0("^", deg, ")"), "")), 
                                     in_table = paste0("precip", ifelse(deg > 1, paste0("$^", deg, "$"), ""), 
                                                       ", ", ifelse(lag > 0, paste0("lag ", lag), "current"))))

etable(
  # `main spec` = mod_ests[["main"]], 
  #      unweighted = mod_ests[["no_pop_weight"]],
  #      `no Brazil` = mod_ests[["no_brazil"]],
  #      `no precip` = 
       # , "no precip", "precip^2", "no brazil", 
       #               "4th order temp", "5th order temp", "lags 1-2", "lags 1-4", 
       #               "lags 0-3", "country-month-year FE", "unit seasonality"
  mod_ests[which(names(mod_ests) %in% c("pop_offset", "poly2", "country_trend") == FALSE)],
  #   set_names(c("main spec", "unweighted", "no precip", "precip^2", "no brazil", 
  #               "4th order temp", "5th order temp", "lags 1-2", "lags 1-4", 
  #               "lags 0-3", "country-month-year FE", "unit seasonality")), 
  headers = c("main spec", "unweighted", "no precip", "precip$^2$", "no brazil", 
              "temp$^4$", "temp$^5$", "lags 1-2", "lags 1-4",
              "lags 0-3", "country-month\n-year FE", "unit\nseasonality"),
  tex = T,
  drop = "precip",
  page.width = "fit",
  order = c("temp.*current", "temp.*lag 1", "temp.*lag 2", "temp.*lag 3", "temp.*lag 4", 
            "precip.*lag 1", "precip.*lag 2", "precip.*lag 3", "precip.*lag 4"),
  se = "cluster", fitstat = c("n", "pr2"),
  dict = c(temp_variable_names$in_table) %>% set_names(temp_variable_names$in_mod),
  postprocess.tex = function(x){
      gsub("        ", " ", x, fixed = T) %>%
      # gsub("country-month-year FE", "\\makecell{country-month-\\\\year FE}", ., fixed = T) %>% 
      # gsub("unit seasonality", "unit\\\\seasonality}", ., fixed = T) %>% 
      gsub("countryFE", "cntryFE", .) %>% 
      gsub("\\centering", "\\tiny \\centering", ., fixed = T)},
  replace = T,
  # adjustbox = 1.1,
  label = "model_coefs",
  title = "Estimated coefficients from main model and alternative specifications. Only coefficient estimate for temperature covariates are show. Standard errors are shown in parantheses. Unless noted in the model name, all models use population-weights, and linear precipitation controls with the lags of precipitation matching the temperature lags. For the fixed effects, `cntryFE\' indicates country-data source fixed effects (see ``Estimating dengue-temperature responses\").",
  file = "./figures/tableS3_model_coefficients.tex") 

# all marginals with SEs
# mod_marg %>% 
#   filter(x >= 10 & x<= 32.5) %>%
#   ggplot(aes(x = x, y = y, ymax = y + 1.96*se, ymin = y - 1.96*se, 
#              fill = mod, color = mod)) + 
#   geom_ribbon(alpha = 0.4, color = NA) + 
#   geom_line() + 
#   facet_wrap(~mod) + 
#   theme_classic()
# 
