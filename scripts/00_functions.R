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
