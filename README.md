# global-dengue-temperature

## Download/extract data 

### Shapefiles 
1) Download all shapefiles 
2) Simplify PHL shapefile, upload all to earth engine assets 
 	- ADD relevant script

### Current and future temperature data 
3) Calculate ERA5 climatologies to debias with, upload to earth engine 
	- cds_setup.R with cds user and key 
	- era5_climatology.R 
4) Download worldclim climatologies, upload to earth engine
5) Calculate monthly temperature for relevant periods with debiased ERA5 using earth engine 
	- extract_temperature_gee_daily.R 
5) Calculate temperature change in CMIP6 climatologies, upload to earth engine
	- select which climatologies to use (select_gcm_climatologies.R)
	- calculate dT for selected climatologies and save as rasters (get_cmip6_climatologies.py)
6) Calculate temperature in the future using ERA5 with dT from above 
	- extract_climate_scenario_temperature_gee_daily.R 

### Dengue data 

## Clean and merge data 

## Fit regressions 

## Make figures and tables


## Supplemental scripts/analyses 
1) comparison of ERA5 and worldclim and why we need to debias 
2) check of climatologies