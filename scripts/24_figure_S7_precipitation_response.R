library(tidyverse)
library(magrittr)
library(cowplot)
library(fixest)

source("./scripts/00_functions.R")

# load dengue data 
dengue_temp <- readRDS("./data/dengue_temp_full.rds") %>% 
  mutate(dengue_inc = dengue_cases/pop, 
         countryFE = paste0(country, "_", mid_year),
         country_id = paste0(countryFE, "_", id)) %>%
  filter(!is.na(dengue_inc))

# do we see a precipitation response? 
mod_ests <- readRDS("./output/mod_ests/all_models.rds")


marginal_est_se <- function(mod,
                            coef_name_regex = "",
                            x_seq,
                            vcov_type = "cluster",
                            debug = FALSE,
                            vcov_mat, coef_vec,
                            degree_regex = "\\^"){
  if(!missing(mod)){
    if(!missing(coef_vec)) warning("Both model and coefficient vector provided. Using coef(model) by default")
    coef_vec <- coef(mod)
  } else if(missing(mod) & missing(coef_vec)){
    stop("Must specify either model (mod) or coefficient vector (coef_vec).")
  }
  
  if(!missing(mod)){
    if(!missing(vcov_mat)) warning("Both model and variance-covariance matrix provided. Using coef(model) by default.")
    vcov_mat = mod %>% vcov(vcov = vcov_type)
  } else if(missing(mod) & missing(coef_vec)){
    stop("Must specify either model (mod) or variance-covariance matrix (vcov_mat).")
  }
  coef_names <- names(coef_vec)
  if(debug){print(coef_names)}
  ind <- which(grepl(coef_name_regex, coef_names))
  if(debug){print(ind)}
  degs <- stringr::str_extract(coef_names[ind], stringr::regex(paste0(degree_regex, "\\d"))) %>%
    gsub(pattern = degree_regex, replacement = "") %>%
    as.numeric %>%
    replace_na(1)
  if(debug){print(degs)}
  coef_vcv <- vcov_mat %>%
    magrittr::extract(ind, ind)
  coef_est <- coef_vec %>%
    magrittr::extract(ind)
  est <- coef_est %*% (sapply(degs, function(x)  x*x_seq^(x-1)) %>% t)
  var <- (sapply(degs, function(x)  x*x_seq^(x-1))) %*% coef_vcv %*% t(sapply(degs, function(x)  x*x_seq^(x-1)))
  data.frame(x = x_seq,
             y = est[1,],
             se = sqrt(diag(var))) %>%
    return
}

response_est_se <- function(mod, coef_name_regex,
                            x_seq,
                            vcov_type = "cluster",
                            debug = FALSE, 
                            degree_regex = "\\^"){
  
  coef_names <- mod %>%
    coef %>%
    names
  if(debug){print(coef_names)}
  ind <- which(grepl(coef_name_regex, coef_names))
  if(debug){print(ind)}
  degs <- stringr::str_extract(coef_names[ind], stringr::regex(paste0(degree_regex, "\\d"))) %>%
    gsub(pattern = degree_regex, replacement = "") %>%
    as.numeric %>%
    replace_na(1)
  if(debug){print(degs)}
  if(!is.na(vcov_type)){
    coef_vcv <- mod %>%
      vcov(vcov = vcov_type) %>%
      magrittr::extract(ind, ind)
  }
  # if(debug){print(coef_vcv)}
  coef_est <- mod %>%
    coef %>%
    magrittr::extract(ind)
  # if(debug){print(coef_est)}
  est <- coef_est %*% (sapply(degs, function(x)  x_seq^x) %>% t)
  # if(debug){print(est)}
  if(!is.na(vcov_type)){
    var <- (sapply(degs, function(x)  x_seq^x)) %*% coef_vcv %*% t(sapply(degs, function(x)  x_seq^x))
    se <- sqrt(diag(var))
  } else{se <- NA}
  
  # if(debug){print(var)}
  data.frame(x = x_seq,
             y = est[1,],
             se = se) %>%
    return
}

precip_seq <- dengue_temp$total_precipitation %>% quantile(c(0.01, 0.99), na.rm = T) %>% 
  {seq(from = .[1], to = .[2], length.out = 100)}
mod_marg <- purrr::imap(mod_ests[c("main", "precip_sq")], 
                        function(x, name){
                          marginal_est_se(x, "precip", precip_seq, "cluster", TRUE) %>% 
                            mutate(mod = name) %>% 
                            return
                        }) %>% 
  list_rbind

mod_resp <- purrr::imap(mod_ests[c("main", "precip_sq")], 
                        function(x, name){
                          response_est_se(x, "precip", precip_seq, "cluster", TRUE) %>% 
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
                    ymin = y -1.96*se + yoff, 
                    ymax = y + 1.96*se + yoff), color = NA, alpha = 0.1) + 
    # geom_hline(yintercept = yoff) + 
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
  ggsave(filename = "figures/precipitation_response.png", width = 5, height = 3)
