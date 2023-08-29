library(tidyverse)
library(magrittr)
library(ecmwfr)
library(raster)

# load your copernicus climate data store key
source("00_setup.R")
wf_set_key(cds_user,
           cds_key, 
           "cds")

# for each month, loop through the years and download the monthly averages 
# then read in and average the monthly averages to get monthly climatology 
# save the monthly climatology 
purrr::map(1:12, function(m){
  # download different year files
  y_files <- purrr::map_chr(1970:2000, function(y){
    request <- list(
      product_type = "monthly_averaged_reanalysis",
      variable = "2m_temperature",
      year = as.character(y),
      month = formatC(m, width=2, flag="0"),
      time = "00:00",
      format = "netcdf",
      dataset_short_name = "reanalysis-era5-single-levels-monthly-means",
      target = paste0("era5_monthly_2m_temperature_", 
                      as.character(y), 
                      "_", 
                      formatC(m, width=2, flag="0"),
                      ".nc"))
      ncfile <- wf_request(
        request = request,   
        transfer = TRUE,  
        path = "./temp",
        verbose = FALSE)
  })
  # read in year files and average 
  m_rasts <- stack(y_files)
  # calculate and save average
  m_avg <- calc(m_rasts, fun = mean, na.rm = T)
  # add the offset, since era5 is specified by the corners of pixels
  m_avg <- shift(m_avg, 0.125, -0.125)
  writeRaster(m_avg, 
              paste0("./data/era5_climatology/era5_climatology_1970-2000_",
                     formatC(m, width=2, flag="0"),
                     ".tif"),
              format = 'GTiff')
  # delete year files 
  unlink(y_files)
})

#test <- raster(ncfile)
# need to add 0.125 in x, -0.125 in y (i.e., needs to go down and right) 
# crs(test) <- paste0(crs(test)@projargs, " +x_0=0.125 +y_0=-0.125")

# extent(test)
# # lets save and upload to EE to check
# 
# writeRaster(test, 
#             "./data/era5_test_2000_01.tif", 
#             overwrite = TRUE,
#             format = "GTiff")

