library(tidyverse)
library(magrittr)
library(cowplot)
library(fixest)
library(pomp)

dengue_dt <- data.frame(day = seq(0, by = 30, length.out = 12*20), 
                        reports = 0)

sir_step <- Csnippet("
double N = S + E + I + R; 
double Ieff;
if (I < 1)
    Ieff = 1;
  else
    Ieff = I;
double ddeaths_S = nearbyint(dr*S);
double ddeaths_E = nearbyint(dr*E);
double ddeaths_I = nearbyint(dr*I);
double ddeaths_R = nearbyint(dr*R);
double dbirths_S = nearbyint(br*N);
double dN_SE = rbinom(S - ddeaths_S,1-exp(-Beta_max*rel_trans(temp)*Ieff/N*dt));
double dN_EI = rbinom(E - ddeaths_E,1-exp(-mu_EI*dt));
double dN_IR = rbinom(I - ddeaths_I,1-exp(-mu_IR*dt));
double dN_RS = rbinom(R - ddeaths_R,1-exp(-mu_RS*dt));

S += dbirths_S - dN_SE - ddeaths_S + dN_RS;
E += dN_SE - dN_EI - ddeaths_E;
I += dN_EI - dN_IR - ddeaths_I;
R += dN_IR - dN_RS - ddeaths_R;
H += dN_IR;
")
sir_rinit <- Csnippet("
S = nearbyint(eta*N0);
E = 0;
I = I0;
R = N0 - I0 - nearbyint(eta*N0);
H = 0;
")
sir_dmeas <- Csnippet("
lik = dpois(reports, rho*H, give_log);
")
# lik = dnbinom_mu(reports,k,rho*H,give_log);

sir_rmeas <- Csnippet("
reports = rpois(rho*H);
")
temp_dependent_transmission <- Csnippet("
  double max(double x, double y){
    if( x < y){
      return y;
    }
    else{
      return x;
    }
  }
  double min(double x, double y){
    if( x < y){
        return x;
    }
    else{
      return y;
    }
  }
  double briere(double x, double c, double T0, double Tm){
    if((x < T0) || (x > Tm)){
      return 0.0;
    }
    else{
      return c*x*(x-T0)*sqrt(Tm-x);
    }
  }
                        
  double quadratic(double x, double c, double T0, double Tm){
    if((x < T0) || (x > Tm)){
      return 0.0;
    }
    else{
      return c*(x-T0)*(x-Tm);
    }
  }
                        
  // probability of mosquito egg-to-adult survival                    
  double rel_trans(double temp){
    return quadratic(temp,-1e-02,15,35);
  }
  
  ")
# measSIR |>
temp_covar = data.frame(day = seq(0, max(dengue_dt$day))) %>%
  mutate(temp = rnorm(n(), cos(day/360*2*pi)*1.5 + 25, 1))
dengue_dt |> 
  pomp(times = "day", t0 = 0, 
       covar = temp_covar %>% 
         covariate_table(times = "day"),
       globals = temp_dependent_transmission, 
       rprocess=euler(sir_step,delta.t=1),
       rinit=sir_rinit,
       rmeasure=sir_rmeas,
       dmeasure=sir_dmeas,
       accumvars="H",
       statenames=c("S","E", "I","R","H"),
       paramnames=c("Beta_max","mu_EI", "mu_IR", "mu_RS", 
                    "N0", "I0", "eta","rho", "br", "dr"), 
       verbose = T
  ) -> SIR

temp_sim = data.frame(day = seq(0, max(dengue_dt$day))) %>%
  mutate(temp = rnorm(n(), cos(day/360*2*pi)*1.5 + 30, 1.5))

# SIR |>
#   simulate(
#     params=c(Beta_max=0.3, # transmission rate max 
#              mu_EI = 1/3, mu_IR=1/7, # transition rates
#              mu_RS = 1/365, # transition back to susceptible
#              eta=0.7, N0=1000000, I0 = 5, # initial conditions
#              br = 0.0001, dr = 0.0001, # birth and death rates
#              rho=0.5, k=10), # observation rate and noise
#     # params=c(Beta_max=0.5,mu_EI = 1/2, mu_IR=1/5, eta=0.7,
#     #          N0=1000000, I0 = 30, br = 0.0002, dr = 0.0002, 
#     #          rho=0.5,k=10),
#     covar = temp_sim %>% 
#       covariate_table(times = "day"),
#     nsim=50,format="data.frame",include.data=FALSE
#   ) -> sims

# sims |>
#   mutate(month = day/30, 
#          year = day/360) |>
#   # filter(.id == 1) |> 
#   # filter(day > 360*10) %>%
#   ggplot(aes(x=year,
#              y=reports,
#              # y= S/(S + E + I + R),
#              # y = I,
#              group=.id,color=.id=="data"))+
#   geom_line()+
#   guides(color="none") + 
#   theme_classic()
# 
# 
# fixest::fepois(inc ~ temp_deg1 + temp_deg2 + temp_deg3 + #temp_deg4 +
#                  temp_deg1_lag1 + temp_deg2_lag1 + temp_deg3_lag1 | id + year + moy, 
#                data = sims %>% 
#                  filter(day > 360*10) %>%
#                  rename(id = .id) %>% 
#                  left_join(temp_covar %>% 
#                              mutate(month = floor((day - 1)/30)) %>% 
#                              summarise(temp_deg1 = mean(temp), 
#                                        temp_deg2 = mean(temp^2), 
#                                        temp_deg3 = mean(temp^3), 
#                                        temp_deg4 = mean(temp^4), 
#                                        day = max(day),
#                                        .by = month) %>% 
#                              mutate(temp_deg1_lag1 = lag(temp_deg1, 1), 
#                                     temp_deg2_lag1 = lag(temp_deg2, 1), 
#                                     temp_deg3_lag1 = lag(temp_deg3, 1), 
#                                     temp_deg4_lag1 = lag(temp_deg4, 1), 
#                                     temp_deg1_lag2 = lag(temp_deg1, 2), 
#                                     temp_deg2_lag2 = lag(temp_deg2, 2),
#                                     temp_deg3_lag2 = lag(temp_deg3, 2),
#                                     temp_deg4_lag2 = lag(temp_deg4, 2))) %>% 
#                  mutate(year = floor(month/12), 
#                         moy = month - year*12,
#                         inc = reports / (S + E + I + R))) %>% coef -> coef_est
# temp_seq = seq(0, 50, by = 0.1)
# data.frame(x = temp_seq, 
#            y = t(t(as.matrix(coef_est)) %*% rbind(temp_seq, temp_seq^2, temp_seq^3, 
#                                                   temp_seq, temp_seq^2, temp_seq^3))) %>% 
#   filter(x > min(temp_sim$temp) & x < max(temp_sim$temp)) %>% 
#   mutate(y_rel = y - min(y)) %>% 
#   ggplot(aes(x = x, y = y_rel/max(y_rel)*10)) + 
#   geom_line(color = "blue") + 
#   geom_line(data = data.frame(x = temp_seq) %>% 
#               mutate(y = ifelse(x < 15 | x > 35, 0, -1e-02*(x-15)*(x-35))), 
#             aes(x = x, y = 10*y), inherit.aes = F) +
#   geom_vline(xintercept = c(15, 25), color = "red") + 
#   geom_histogram(data = temp_sim, aes(x = temp, y = 100*after_stat(density)), 
#                  alpha = 0.3, inherit.aes = FALSE) + 
#   theme_classic()

# temp_sim %>% 
#   ggplot(aes(x = day, y = temp)) + 
#   geom_line() + 
#   geom_line(aes(y = temp_avg), color = "blue") + 
#   theme_classic()


# lets do something "simple" for now 
# assume N countries, each country has M units, each unit is observed for 15 years
# temperature anomalies happen at the country level. repeat the simulation 20 times 
N = 10
M = 20
Y = 20 
nsim = 20 

set.seed(1001)
country_avgs = data.frame(country = 1:N) %>% 
  mutate(country_temp_avg = runif(n(), 17, 30), 
         country_temp_amp = runif(n(), 1.5, 4))
unit_avgs = cross_join(country_avgs, 
                       data.frame(id = 1:M)) %>% 
  mutate(unit_temp_avg = rnorm(n(), country_temp_avg, 2), 
         unit_temp_amp = rnorm(n(), country_temp_amp, 0.5))

# random daily country_anomalies 
country_anomalies = expand.grid(country = 1:N, 
                                day = seq(0, 360*Y)) %>% 
  mutate(country_temp_anom = rnorm(n(), 0, 1.5))
temp_sim = left_join(country_anomalies, 
                     unit_avgs, 
                     relationship = "many-to-many") %>% 
  mutate(temp = cos(day/360*2*pi)*unit_temp_amp + unit_temp_avg + rnorm(n(), country_temp_anom, 0.5), 
         unit_id = paste0(country, "_", id))  
temp_sim %>% 
  filter(id %in% c(1)) %>%
  filter(country == 1) %>% 
  ggplot(aes(x = day, y = temp)) +
  geom_line() + 
  facet_wrap(~country) + theme_classic()

# temp_sim %>% 
#   nest_by(unit_id) %>% 
#   pmap(function(unit_id, data){
#     SIR |>
#       simulate(
#         params=c(Beta_max=0.25, # transmission rate max 
#                  eta=0.65, N0=1000000, I0 = 15, # initial conditions
#                  mu_EI = 1/3, mu_IR=1/7, # transition rates
#                  mu_RS = 1/(1.25*360),
#                  br = 0.00005, dr = 0.00005, # birth and death rates
#                  rho=0.5), # observation rate 
#         # and noise (k = 10) --> dropped noise 
#         # params=c(Beta_max=0.5,mu_EI = 1/2, mu_IR=1/5, eta=0.7,
#         #          N0=1000000, I0 = 30, br = 0.0002, dr = 0.0002, 
#         #          rho=0.5,k=10),
#         covar = data %>% 
#           covariate_table(times = "day"),
#         times = seq(30, 360*Y, by = 30),
#         nsim=nsim,format="data.frame",include.data=FALSE
#       ) %>% 
#       mutate(unit_id = unit_id) %>% 
#       return
#   }) %>% 
#   list_rbind() -> sims

# sims %>% 
#   separate(unit_id, c("country", "admin_id"), 
#            sep = "_", remove = FALSE, convert = T) %>% 
#   filter(.id == 1 & admin_id %in% c("1", "2")) %>%
#   left_join(unit_avgs %>% mutate(unit_id = paste0(country, "_", id))) %>% 
#   mutate(panel_name = paste0("avg = ", 
#                              round(country_temp_avg, 1), 
#                              ", amp = ", 
#                              round(country_temp_amp, 1))) %>% 
#   ggplot(aes(x = day/360, 
#              y = reports,
#              # y = S/(S + E + I + R),
#              group = unit_id,
#              color = as.factor(admin_id))) + 
#   geom_line(alpha = 0.7) + 
#   facet_wrap(~panel_name, scales = "free") + 
#   scale_color_manual(values = c("grey40", "blue")) + 
#   theme_classic() + 
#   theme(legend.position = "none", strip.background = element_blank()) + 
#   ylab("reported cases") + xlab("year")
# 
# sims$.id %>% unique %>% 
#   purrr::map(function(x){
#     print(x)
#     fixest::fepois(inc ~ temp_deg1 + temp_deg2 + temp_deg3 + temp_deg4 + 
#                      temp_deg1_lag1 + temp_deg2_lag1 + temp_deg3_lag1 + temp_deg4_lag1 |
#                      unit_id + country^year + country^moy,
#                    data = sims %>%
#                      separate(unit_id, c("country", NA), sep = "_", remove = FALSE) %>%
#                      filter(day > 360*5) %>%
#                      filter(.id == x) %>%
#                      left_join(temp_sim %>%
#                                  mutate(month = floor((day - 1)/30)) %>%
#                                  summarise(temp_deg1 = mean(temp),
#                                            temp_deg2 = mean(temp^2),
#                                            temp_deg3 = mean(temp^3),
#                                            temp_deg4 = mean(temp^4),
#                                            day = max(day),
#                                            .by = c(unit_id, month)) %>%
#                                  arrange(unit_id, month) %>%
#                                  mutate(temp_deg1_lag1 = lag(temp_deg1, 1),
#                                         temp_deg2_lag1 = lag(temp_deg2, 1),
#                                         temp_deg3_lag1 = lag(temp_deg3, 1),
#                                         temp_deg4_lag1 = lag(temp_deg4, 1),
#                                         .by = unit_id)) %>%
#                      mutate(year = floor(month/12),
#                             moy = month - year*12,
#                             inc = reports / (S + E + I + R))) %>%
#       coef %>%
#       return
#     }) -> coef_ests
#     
# temp_seq = seq(0, 50, by = 0.1)
# temp_resp_mat <- reduce(coef_ests, rbind) %>% 
#   colnames %>%
#   grep("temp", ., value = TRUE) %>% 
#   gsub(".*deg|_lag.", "", .) %>% 
#   as.numeric %>%
#   purrr::map(function(deg){
#     temp_seq^deg
#   }) %>% 
#   reduce(cbind) 
# boot_resp <- as.matrix(reduce(coef_ests, rbind)) %*% t(temp_resp_mat) %>% 
#   set_colnames(paste0("temp_", temp_seq)) %>% 
#   as.data.frame() %>% 
#   mutate(boot_id = 1:n()) %>% 
#   pivot_longer(!boot_id, names_prefix = "temp_", 
#                names_transform = as.numeric,
#                names_to = "x", values_to = "y")
# 
# boot_resp %>% 
#   filter(x > quantile(temp_sim$temp, 0.01) & x < quantile(temp_sim$temp, 0.99)) %>% 
#   mutate(y_rel = (y - min(y))/(max(y) - min(y)), 
#          .by = boot_id) %>% 
#   ggplot(aes(x = x, y = y_rel, group = boot_id)) + 
#   geom_histogram(data = temp_sim, aes(x = temp, y = 3.5*after_stat(density)), 
#                  alpha = 0.3, color = "white", inherit.aes = FALSE) + 
#   geom_line(color = "blue", alpha = 0.3) + 
#   geom_line(data = data.frame(x = temp_seq) %>% 
#               mutate(y = ifelse(x < 15 | x > 35, 0, -1e-02*(x-15)*(x-35))), 
#             aes(x = x, y = y), inherit.aes = F) +
#   # geom_vline(xintercept = c(15, 25), color = "red") + 
#   theme_classic() + ylab("relative log(dengue)/transmission rate") + xlab("temperature")
# 


# try different levels of susceptibility --------------------------
set.seed(101)
country_trans_pars = data.frame(country = 1:N) %>% 
  mutate(country_S0 = runif(n(), 0.5, 0.9), 
         Beta_max = rnorm(n(), 0.25, 0.04))
unit_trans_pars = cross_join(country_trans_pars, 
                             data.frame(id = 1:M)) %>% 
  mutate(unit_S0 = pmin(rnorm(n(), country_S0, 0.05), 1),
         unit_I0 = runif(n(), 5, 100), 
         unit_Beta_max = rnorm(n(), Beta_max, 0.02), 
         unit_id = paste0(country, "_", id)) 

temp_sim %>% 
  left_join(unit_trans_pars) %>% 
  nest_by(unit_id, unit_S0, unit_I0, unit_Beta_max) %>% 
  pmap(function(unit_id, unit_S0, unit_I0, unit_Beta_max, data){
    SIR |>
      simulate(
        params=c(Beta_max=unit_Beta_max, # transmission rate max 
                 eta=unit_S0, N0=1000000, I0 = round(unit_I0), # initial conditions
                 mu_EI = 1/3, mu_IR=1/7, # transition rates
                 mu_RS = 1/(2*360),
                 br = 0.00007, dr = 0.00007, # birth and death rates
                 rho=0.5), # observation rate and noise
        # params=c(Beta_max=0.5,mu_EI = 1/2, mu_IR=1/5, eta=0.7,
        #          N0=1000000, I0 = 30, br = 0.0002, dr = 0.0002, 
        #          rho=0.5,k=10),
        covar = data %>% 
          covariate_table(times = "day"),
        times = seq(30, 360*Y, by = 30),
        nsim=nsim,format="data.frame",include.data=FALSE
      ) %>% 
      mutate(unit_id = unit_id) %>% 
      return
  }) %>% 
  list_rbind() -> sims_suscept

# plot sims 
sims_suscept %>% 
  separate(unit_id, c("country", "admin_id"), 
           sep = "_", remove = FALSE, convert = T) %>% 
  filter(.id == 1 & admin_id %in% c("1")) %>%
  left_join(unit_avgs %>% mutate(unit_id = paste0(country, "_", id))) %>%
  mutate(panel_name = paste0("avg = ",
                             round(country_temp_avg, 1),
                             ", amp = ",
                             round(country_temp_amp, 1))) %>%
  filter(day > 360*5) %>%
  {ggplot(data = ., 
          aes(x = day/360, y = reports, 
              group = unit_id)) + 
      # geom_line(aes(color = gsub(".*_", "", unit_id))) + 
      geom_line() +
      facet_wrap(~country, scales = "free") + 
      theme_classic() + 
      theme(legend.position = "none", 
            strip.background = element_blank()) + 
      xlab("year") + ylab("reported cases")} %>% 
  ggsave(filename = "./figures/sim_dynamics.png", 
         width = 8, height = 6)

# sims_suscept %>% 
#   separate(unit_id, c("country", "admin_id"), 
#            sep = "_", remove = FALSE, convert = T) %>% 
#   filter(.id == 1) %>%
#   left_join(unit_avgs %>% mutate(unit_id = paste0(country, "_", id))) %>%
#   filter(day > 360*5) %>%
#   ggplot(aes(x = day/360, y = S/(S + E + I + R), group = unit_id)) + 
#   geom_line() + 
#   geom_hline(yintercept = c(0.6),
#              color = "red") + 
#   geom_hline(yintercept = c(0.8),
#              color = "blue") + 
#   facet_wrap(~country) + 
#   scale_y_continuous(trans = "pseudo_log") + 
#   theme_classic() + 
#   theme(legend.position = "none") + xlab("year") 

sims_suscept$.id %>% unique %>%
  purrr::map(function(x){
    print(x)
    fixest::fepois(inc ~ temp_deg1 + temp_deg2 + temp_deg3 +
                     temp_deg1_lag1 + temp_deg2_lag1 + temp_deg3_lag1 |
                     unit_id + country^year + country^moy,
                   data = sims_suscept %>%
                     separate(unit_id, c("country", NA), sep = "_", remove = FALSE) %>%
                     filter(day > 360*5) %>%
                     filter(.id == x) %>%
                     left_join(temp_sim %>%
                                 mutate(month = floor((day - 1)/30)) %>%
                                 summarise(temp_deg1 = mean(temp),
                                           temp_deg2 = mean(temp^2),
                                           temp_deg3 = mean(temp^3),
                                           day = max(day),
                                           .by = c(unit_id, month)) %>%
                                 arrange(unit_id, month) %>%
                                 mutate(temp_deg1_lag1 = lag(temp_deg1, 1),
                                        temp_deg2_lag1 = lag(temp_deg2, 1),
                                        temp_deg3_lag1 = lag(temp_deg3, 1),
                                        .by = unit_id)) %>%
                     mutate(year = floor(month/12),
                            moy = month - year*12,
                            inc = reports / (S + E + I + R))) %>%
      coef %>%
      return
  }) -> coef_ests2
# bin by susceptibility? 
purrr::map(sims_suscept$.id %>% unique, function(id_val){
  print(id_val)
  fixest::fepois(inc ~ pct_S_bin:temp_deg1 + pct_S_bin:temp_deg2 + pct_S_bin:temp_deg3 + 
                   pct_S_bin:temp_deg1_lag1 + pct_S_bin:temp_deg2_lag1 + pct_S_bin:temp_deg3_lag1 | 
                   unit_id + country^year + country^moy, 
                 data = sims_suscept %>% 
                   separate(unit_id, c("country", NA), sep = "_", remove = FALSE) %>% 
                   filter(.id == id_val) %>% 
                   # are the days off by 1? 
                   left_join(temp_sim %>% 
                               mutate(month = floor((day - 1)/30)) %>% 
                               summarise(temp_deg1 = mean(temp), 
                                         temp_deg2 = mean(temp^2), 
                                         temp_deg3 = mean(temp^3), 
                                         temp_deg4 = mean(temp^4), 
                                         day = max(day),
                                         .by = c(unit_id, month)) %>% 
                               arrange(unit_id, month) %>% 
                               mutate(temp_deg1_lag1 = lag(temp_deg1, 1), 
                                      temp_deg2_lag1 = lag(temp_deg2, 1), 
                                      temp_deg3_lag1 = lag(temp_deg3, 1), 
                                      temp_deg4_lag1 = lag(temp_deg4, 1), 
                                      .by = unit_id)) %>% 
                   mutate(year = floor(month/12), 
                          moy = month - year*12,
                          pct_S = lag(S, 1)/(S + E + I + R),
                          inc = reports / (S + E + I + R)) %>% 
                   arrange(unit_id, day) %>% 
                   mutate(pct_S_bin = case_when(lag(pct_S, 1) < 0.6 ~ "low", 
                                                lag(pct_S, 1) <= 0.8 ~ "mid", 
                                                lag(pct_S, 1) > 0.8 ~ "high"),
                          .by = unit_id) %>% 
                   filter(day > 360*5)) %>% coef %>%
    # {data.frame(coef = ., 
    #             vars = names(.), 
    #             id = id_val)} %>% 
    return
}) -> coef_ests_suscept

temp_seq = seq(0, 50, by = 0.1)
temp_resp_mat <- reduce(coef_ests2, rbind) %>%
  colnames %>%
  grep("temp", ., value = TRUE) %>%
  gsub(".*deg|_lag.", "", .) %>%
  as.numeric %>%
  purrr::map(function(deg){
    temp_seq^deg
  }) %>%
  reduce(cbind)
boot_resp <- as.matrix(reduce(coef_ests2, rbind)) %*% t(temp_resp_mat) %>%
  set_colnames(paste0("temp_", temp_seq)) %>%
  as.data.frame() %>%
  mutate(boot_id = 1:n()) %>%
  pivot_longer(!boot_id, names_prefix = "temp_",
               names_transform = as.numeric,
               names_to = "x", values_to = "y")

boot_resp %>%
  filter(x > quantile(temp_sim$temp, 0.01) & x < quantile(temp_sim$temp, 0.99)) %>%
  mutate(y_rel = (y - min(y))/(max(y) - min(y)),
         .by = boot_id) %>%
  {ggplot(data = ., 
          aes(x = x, y = y_rel, group = boot_id)) +
      geom_histogram(data = temp_sim, aes(x = temp, y = 3.5*after_stat(density)),
                     alpha = 0.3, color = "white", inherit.aes = FALSE) +
      geom_line(color = "blue", alpha = 0.3) +
      geom_line(data = data.frame(x = temp_seq) %>%
                  mutate(y = ifelse(x < 15 | x > 35, 0, -1e-02*(x-15)*(x-35))),
                aes(x = x, y = y), inherit.aes = F) +
      xlim(8, 42) + 
      # geom_vline(xintercept = c(15, 25), color = "red") +
      theme_classic() + ylab("relative log(dengue)") + xlab("temperature")} %>% 
  ggsave(filename = "./figures/sim_response.png", width = 5, height = 3.5)


temp_marg_mat <- reduce(coef_ests_suscept, rbind) %>% 
  colnames %>%
  grep("binhigh", ., value = TRUE) %>% 
  grep("temp", ., value = TRUE) %>% 
  gsub(".*deg|_lag.", "", .) %>% 
  as.numeric %>%
  purrr::map(function(deg){
    deg*temp_seq^(deg - 1)
  }) %>% 
  reduce(cbind) 

yoff = 0.75
purrr::map(c("high", "mid", "low"), 
           function(bin_i){
             print(bin_i)
             reduce(coef_ests_suscept, rbind) %>% 
               {magrittr::extract(., ,grep(bin_i, colnames(.)))} %>%
               as.matrix() %>%
               multiply_by_matrix(t(temp_marg_mat)) %>% 
               set_colnames(paste0("temp_", temp_seq)) %>%
               as.data.frame() %>%
               mutate(boot_id = 1:n()) %>%
               pivot_longer(!boot_id, names_prefix = "temp_",
                            names_transform = as.numeric,
                            names_to = "x", values_to = "y") %>%
               mutate(bin = bin_i) %>% 
               return
           }) %>% list_rbind() %>% 
  filter(x > quantile(temp_sim$temp, 0.01) & x < quantile(temp_sim$temp, 0.99)) %>% 
  mutate(bin = factor(bin, levels = c("high", "mid", "low"), ordered = T)) %>% 
  {ggplot(data = ., 
          aes(x = x, y = y + yoff, color = bin, group = interaction(boot_id, bin))) + 
      geom_hline(yintercept = yoff) + 
      geom_histogram(data = temp_sim, aes(x = temp, y = 2*after_stat(density)), 
                     alpha = 0.3, inherit.aes = FALSE, color = "white") + 
      geom_line(alpha = 0.6) + 
      scale_y_continuous(breaks = seq(-1, 3, by = 0.25), labels = function(x){x - yoff}) + 
      # geom_vline(xintercept = c(15, 25), color = "red") + 
      theme_classic() + 
      xlim(quantile(temp_sim$temp, 0.005), quantile(temp_sim$temp, 0.995)) + 
      scale_color_manual(name = "susceptibility", 
                         values = c("#F21A00","#E1AF00", "#3B9AB2")) +
      theme(legend.position = c(0.8, 0.8)) + 
      ylab("d log(dengue)/dtemp") + xlab("temperature")} %>% 
  ggsave(filename = "./figures/sim_binned_response.png", 
         width = 5, height = 3.5)
