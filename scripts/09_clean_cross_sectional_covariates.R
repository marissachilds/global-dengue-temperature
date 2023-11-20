# construct cross sectional data with covariates for heterogeoentiy including
# continent, health expenditure, average dengue incidence (from GBD), 
# population density, unit population size, unit area
library(tidyverse)
library(magrittr)

tercile_from_vec <- function(x){
  case_when((ecdf(x))(x)<= 1/3 ~ "low", 
            (ecdf(x))(x)<= 2/3 ~ "mid", 
            (ecdf(x))(x)<= 1 ~ "high")
}

# country tasks for some general information
gee_tasks <- read.csv("../ref_tables/country_tasks.csv") %>% 
  mutate(start_date = as.Date(start_date, format = "%m/%d/%y"), 
         # end_date_inclusive = end_date, 
         end_date = as.Date(end_date, format = "%m/%d/%y") %>% 
           add(as.difftime(1, units = "days")))

# country-wide covariates for heterogeneity 
covar <- read.csv("./data/covariates1.csv") #%>% 
  
# areas for population density 
areas <- list.files("./data/from_gee", pattern = "area", full.names = T) %>% 
  purrr::map_dfr(function(x){
    temp <- read.csv(x) %>% 
      rename_with(function(x){"id"}, any_of(gee_tasks$identifier_col)) %>% 
      mutate(country = gsub("_areas.csv", "", x) %>% 
               gsub("./data/from_gee/", "", ., fixed = T),
             id = ifelse(country != "BOL", 
                         stringi::stri_trans_general(id, id = "Latin-ASCII") %>% tolower, 
                         tolower(id)), 
             id = case_when(id == "dki jakarta" ~ "jakarta", 
                            id == "daerah istimewa yogyakarta" ~ "yogyakarta", 
                            .default = id))
    if(grepl("BRA", x)){
      temp %<>% mutate(id = str_sub(id, 1, 8))
    }
    return(temp)
  })

# full df 
dengue_temp <- readRDS("./data/dengue_temp_full.rds") 

# grab population size from the dengue data 
pops <- dengue_temp %>% 
  select(id, country, pop, mid_year) %>% 
  distinct

# load dengue incidence from other source
gbd_est <- readxl::read_excel("./data/Global Burden Disease dataset.xls")  %>% 
  filter(measure == "Incidence") %>% 
  mutate(country_code = case_match(location, 
                                   "Bolivia" ~ "BOL", 
                                   "Peru" ~ "PER", 
                                   "Mexico" ~ "MEX", 
                                   "Colombia" ~ "COL", 
                                   "Costa Rica" ~ "CRI",
                                   "Taiwan (Province of China)" ~ "TWN", 
                                   "Brazil" ~ "BRA", 
                                   "Honduras" ~ "HND", 
                                   "El Salvador" ~ "SLV", 
                                   "Laos" ~ "LAO", 
                                   "Nicaragua" ~ "NIC", 
                                   "Venezuela" ~ "VEN", 
                                   "Vietnam" ~ "VNM", 
                                   "Dominican Republic" ~ "DOM", 
                                   "Indonesia" ~ "IDN", 
                                   "Panama" ~ "PAN", 
                                   "Sri Lanka" ~ "LKA", 
                                   "Philippines" ~ "PHL", 
                                   "Thailand" ~ "THA", 
                                   "Cambodia" ~ "KHM", 
                                   "Malaysia" ~ "MYS")) %>% 
  filter(!is.na(country_code))

# sub_country_dengue 
sub_country_dengue <- dengue_temp %>% 
  filter(!is.na(dengue_cases)) %>% 
  summarise(total_dengue = sum(dengue_cases), 
            pop = unique(pop),
            .by = c(country, mid_year, id)) %>%  
  mutate(pct_dengue = total_dengue/sum(total_dengue), 
         country_pop = sum(pop),
         .by = c(country, mid_year)) %>% 
  mutate(country_join = ifelse(country == "LKA1", "LKA", country)) %>% 
  left_join(gbd_est %>% 
              select(country_join = country_code, dengue = val)) %>% 
  mutate(sub_country_dengue = dengue * country_pop * pct_dengue / pop, 
         sub_country_dengue_tercile = tercile_from_vec(sub_country_dengue))
  
# country dengue = "true" total cases / total population 
# sub country dengue wants to be "true" unit cases / unit population 
# assume that observed unit cases are proportional to true unit cases within a country 
# then sub country dengue = country dengue * country population * (obs unit cases / obs country cases) / unit pop 
# alternatively, think of scaling the observed sub-country dengue by how much off the country total dengue is from GBD? 
# obs unit cases / unit pop * ("true" country inc * country pop / obs total country cases)
# so level of adjustment depends on how far off GBD estimates are from the country surveillance totals 
# maybe worth plotting just to check and understand whats happening? 

# combine all the data and convert to terciles ---- 
# start with population 
unit_covar <- pops %>% 
  # identify continent from the country information 
  mutate(continent_tercile = ifelse(country %in% c("BOL", "BRA", "COL", "DOM", 
                                                   "HND", "NIC", "PAN", "PER", 
                                                   "CRI", "SLV", "VEN", "MEX"), 
                                    "americas",
                                    "asia"),
         # add a country column for joining on that combines LKA and LKA1 
         country_join = ifelse(country == "LKA1", "LKA", country)) %>% 
  # join in area 
  left_join(areas, 
            by = c("country", "id")) %>% 
  # calculate population density 
  mutate(pop_per_km2 = pop/area_km2, 
         # calculate terciles of area, population, and pop density 
         across(c(pop, pop_per_km2, area_km2), 
                list(tercile = tercile_from_vec))) %>% 
  # join in health expenditure, calculating terciles at the country level
  left_join(covar %>% 
              select(country_join = Country.Code, health_expenditure = CHE2010) %>% 
              mutate(health_expenditure_tercile = tercile_from_vec(health_expenditure))) %>% 
  # join in dengue incidence from the GBD 
  left_join(gbd_est %>% 
              select(country_join = country_code, dengue = val) %>% 
              mutate(dengue_tercile = tercile_from_vec(dengue))) %>% 
  # join in sub-country dengue calculated above
  left_join(sub_country_dengue %>% 
              select(country, id, mid_year, sub_country_dengue, 
                     sub_country_dengue_tercile))

saveRDS(unit_covar, "./data/unit_covariates.rds")

