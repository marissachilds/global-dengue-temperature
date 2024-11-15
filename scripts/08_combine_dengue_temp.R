library(tidyverse)
library(magrittr)

# country tasks for some general information
gee_tasks <- read.csv("../ref_tables/country_tasks.csv") %>% 
  mutate(start_date = as.Date(start_date, format = "%m/%d/%y"), 
         # end_date_inclusive = end_date, 
         end_date = as.Date(end_date, format = "%m/%d/%y") %>% 
           add(as.difftime(1, units = "days")))

# read in the joined country data, 
# renaming the different identified columns to "id" and using the file name to add a country column
dengue_temp <- list.files("./data/joined", 
                          # pattern = "joined",
                          full.names = TRUE) %>% 
  purrr::map_dfr(function(x){
    print(x)
    # use the file name to get the country and mid year
    country = strsplit(x, "/|_")[[1]][4]
    mid_year = strsplit(x, "/|_")[[1]][6] %>% gsub(".csv", "", ., fixed = TRUE) 
    # match the country and mid year with the gee tasks to get the expected time range
    time_range = gee_tasks %>% 
      filter(country_shapefile == country & mid_date == mid_year) %>% 
      select(start_date, end_date) %>% 
      as.vector()
    print(paste0(country, ", ", mid_year))
    print(time_range)
    
    # read in the file
    dengue = read.csv(x, row.names = 1) %>%
      rename_with(.fn = function(x){"id"},
                  .cols = any_of(unique(gee_tasks$identifier_col))) %>%
      rename(dengue_cases = CountValue,
             year = Year) %>%
      mutate(year = ifelse(is.na(year),
                           str_sub(year_month, 1, 4) %>% as.numeric,
                           year),
             month = ifelse(is.na(month),
                            str_sub(year_month, 5, 6) %>% as.numeric,
                            month)) %>%
      # drop the weather data since we'll add the new stuff
      select(-any_of(c("sum", 
                       "mean_2m_air_temperature",
                       "monthRange_2m_air_temperature",
                       "total_precipitation")))
    # join with the full date range from gee_tasks 
    full_panel <- seq.Date(time_range$start_date, time_range$end_date, by = "1 month") %>% 
      head(-1) %>% 
      expand.grid(dates = ., 
                  id = unique(dengue$id)) %>% 
      mutate(year = lubridate::year(dates),
             month = lubridate::month(dates), 
             year_month = year*100+month) %>% 
      select(-dates)
    dengue %<>% full_join(full_panel) %>% 
      mutate(country = country)
    
    # get full list of files with temperature data for that country
    temp_files = list.files("./data/from_gee", pattern = paste0(country, "_temp"), full.names = T) %>% 
      data.frame(file = .) %>% 
      # then limit to the ones that are within the expected time range
      mutate(dates = gsub(".*precip_", "", file), 
             start = str_sub(dates, 3, 10) %>% as.Date(format = "%Y%m%d"), 
             end = str_sub(dates, 12, 19) %>% as.Date(format = "%Y%m%d")) %>% 
      mutate(start_inc = start >= time_range[,1] & start <= time_range[,2], 
             end_inc = end <= time_range[,2] & end >= time_range[,1]) %>% 
      filter(start_inc & end_inc) %>%
      pull(file)
    print(temp_files)
    # read in the temperature data
    temp =  temp_files %>% 
      purrr::map_dfr(read.csv) %>% 
      pivot_wider(names_from = property, values_from = mean) %>% 
      mutate(across(any_of(unique(gee_tasks$identifier_col)), tolower)) %>% 
      rename_with(.fn = function(x){"id"},
                  .cols = any_of(unique(gee_tasks$identifier_col)))
    # read in pop data, only taking files with the expected mid_year
    pop = list.files("./data/from_gee", pattern = paste0(country, "_pop.*", mid_year), full.names = T) %>% 
      purrr::map_dfr(read.csv) %>% 
      mutate(across(any_of(unique(gee_tasks$identifier_col)), tolower)) %>% 
      rename_with(.fn = function(x){"id"},
                  .cols = any_of(unique(gee_tasks$identifier_col))) %>% 
      rename(pop = sum)
    
    temp %<>% left_join(pop, by = "id")
    
    # for some files, clean the id column in temp and pop data to make it joinable to the dengue data
    if(country == "BRA"){
      temp %<>% mutate(id = tolower(id) %>% str_sub(1, 8))
      # pop %<>% mutate(id = tolower(id) %>% str_sub(1, 8))
      dengue %<>% filter(id != "br430000")
    } else if(country %in% c("DOM", "NIC", "PAN", "SLV", "VEN", "MEX")){
      temp %<>% mutate(id = stringi::stri_trans_general(id, id = "Latin-ASCII") %>% tolower)
      # pop %<>% mutate(id = stringi::stri_trans_general(id, id = "Latin-ASCII") %>% tolower)
    } else if(country == "IDN"){
      # # kalimantan utara (was part of kalimantan timur per wikipedia)
      temp %<>% mutate(id = case_match(id, 
                                       "dki jakarta" ~ "jakarta", 
                                       "daerah istimewa yogyakarta" ~ "yogyakarta", 
                                       "kalimantan utara" ~ "kalimantan timur", 
                                       .default = id))
      temp %<>% summarise(across(!pop, 
                                 ~ weighted.mean(.x, pop)),
                          pop = sum(pop),
                          .by = c(id, year, month))
    }
    
    # for NIC 2003, limit to pre 2005, which is when the second dataset starts 
    if(country == "NIC" & mid_year == 2003){
      dengue %<>% filter(year < 2005)
    }
    
    # for IDN, also combine kalimantan timur and kalimantan utara, using pop-weights 
    # join it all together
    out = left_join(dengue, temp, by = c("id", "year", "month")) %>% 
      # left_join(pop, by = "id") %>% 
      mutate(mid_year = mid_year)
    return(out)
  }) 

dengue_temp %<>% 
  filter(year <= 2019)

dengue_temp %<>%   
  rename_with(function(x){gsub("temperare", "temp", x)}, contains("temperare")) %>%
  rename_with(function(x){gsub("temperature", "temp", x)}, contains("temperatare")) %>%
  mutate(date = as.Date(paste0(year_month, "01"), format = "%Y%m%d")) %>%  
  select(-year_month) %>% 
  mutate(nonzero_dengue = any(dengue_cases > 0, na.rm = T),
         .by = c(country, mid_year, id))
  

# check the joined data 
# each unit-month has only one obs? 
dengue_temp %>%
  summarise(n = n(),
            .by = c(country, id, year, month, mid_year)) %>%
  filter(n > 1) %>%
  nrow

# which obs with dengue don't have temperature? 
dengue_temp %>% 
  filter(!is.na(dengue_cases) & is.na(mean_2m_air_temperare_degree1)) %>% 
  nrow

saveRDS(dengue_temp, "./data/dengue_temp_full.rds")
