marginal_est_se <- function(mod,
                            coef_name_regex = "",
                            x_seq,
                            vcov_type = "cluster",
                            debug = FALSE,
                            vcov_mat, coef_vec){
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
  degs <- stringr::str_extract(coef_names[ind], stringr::regex("degree\\d")) %>%
    gsub(pattern = "degree", replacement = "") %>%
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
                        debug = FALSE){

  coef_names <- mod %>%
    coef %>%
    names
  if(debug){print(coef_names)}
  ind <- which(grepl(coef_name_regex, coef_names))
  if(debug){print(ind)}
  degs <- stringr::str_extract(coef_names[ind], stringr::regex("degree\\d")) %>%
    gsub(pattern = "degree", replacement = "") %>%
    as.numeric %>%
    replace_na(1)
  if(debug){print(degs)}
  coef_vcv <- mod %>%
    vcov(vcov = vcov_type) %>%
    magrittr::extract(ind, ind)
  # if(debug){print(coef_vcv)}
  coef_est <- mod %>%
    coef %>%
    magrittr::extract(ind)
  # if(debug){print(coef_est)}
  est <- coef_est %*% (sapply(degs, function(x)  x_seq^x) %>% t)
  # if(debug){print(est)}
  var <- (sapply(degs, function(x)  x_seq^x)) %*% coef_vcv %*% t(sapply(degs, function(x)  x_seq^x))
  # if(debug){print(var)}
  data.frame(x = x_seq,
             y = est[1,],
             se = sqrt(diag(var))) %>%
    return
}

# function for adding annotations to multi-panel/faceted plots
# from https://www.blopig.com/blog/2019/08/combining-inset-plots-with-facets-using-ggplot2/ 
# which says in turn they got the idea from here https://stackoverflow.com/questions/37867758/insetting-on-facet-grided-and-grid-arrangeed-plot
annotation_custom2 <- function (grob, xmin = -Inf, xmax = Inf, ymin = -Inf, ymax = Inf, data) {
  layer(data = data, stat = StatIdentity, position = PositionIdentity, 
        geom = ggplot2:::GeomCustomAnn,
        inherit.aes = FALSE, 
        params = list(grob = grob, 
                      xmin = xmin, xmax = xmax, 
                      ymin = ymin, ymax = ymax))
}

# map function 
make_tercile_map <- function(tercile_col, color_vals, tercile_sf){
  ggplot() + 
    geom_sf(data = continents,
            fill= NA, colour="grey10") +
    geom_sf(data = tercile_sf %>% 
              transmute(tercile = !!sym(tercile_col)),
            mapping = aes(fill = tercile, color = tercile), 
            size = 0.01) + 
    geom_sf(data = countries, color = "grey10", 
            lwd = 0.32, fill = NA) +
    scale_fill_manual(values = color_vals, aesthetics = c("fill", "color")) + 
    ylim(-34, 33) + 
    xlim(-92, 137) +
    theme_void() + 
    theme(legend.position = "none")
}
