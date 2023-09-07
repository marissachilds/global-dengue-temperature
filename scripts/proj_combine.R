library(tidyverse)
library(magrittr)

# create a folder called "rgee_extract" where each subfolder is a different model, 
# and the model folders contain subfolders for each scenario
# create a folder called "scenarios" to contain processed outcomes
# NOTE: I already processed era5

#folder link to id
model_folders <- dir("rgee_extract/")
  
for(model in model_folders){
  scenario_folders <- dir(paste0("rgee_extract/", model, "/"))
  print(scenario_folders)
  
  for(scenario in scenario_folders){
  full_df <- data.frame()
  file_name <- paste0("scenarios_", strsplit(model, " ")[[1]][1], "_", strsplit(scenario, "_")[[1]][5])
  files <- dir(paste0("rgee_extract/", model, "/", scenario, "/"))
  
      for(i in 1:length(files)){
      file <- files[i]
      # access file and transform to align with dataframe used to fit model
      df<-read.csv(paste0("rgee_extract/", model, "/", scenario, "/", file)) %>%
        pivot_wider(names_from="property", values_from="mean") 
      
      names(df)[1]<-"id"
      df$country <- strsplit(file, "_")[[1]][1]
      df %<>% select("id", "year", "country", "month",
                     'mean_2m_air_temperature_degree1',
                     'mean_2m_air_temperature_degree2',
                     'mean_2m_air_temperature_degree3',
                     'mean_2m_air_temperature_degree4',
                     'mean_2m_air_temperature_degree5')
      
      # create column for scenario based on which month it was extracted in
      if(strsplit(file, "_")[[1]][4]=="era5"){
        df$scenario <- "current"
      }
      
      else{
        df$scenario <- strsplit(file, "_")[[1]][6]
      }
      
      full_df <- rbind(full_df, df)
      #file.remove(file)
      
      if(i %% 200 == 0){
        print(paste(i, "/", length(files)))
        saveRDS(full_df, file=paste0("scenarios/",file_name))
      }
     saveRDS(distinct(full_df), file = paste0("scenarios/",file_name))
    }
  }  
}

