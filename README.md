# global-dengue-temperature

## Download/extract data 

### Shapefiles and population data 

1) Download all shapefiles and simplify PHL shapefile 
	- Manual downloads 
	- 00_setup/phl_shp_debug.R
2) Upload all shapefiles to earth engine assets 
3) Transform shapefiles for plotting centered around Pacific Ocean 
	- 00_setup/transform_shapefiles.R 
4) Download population data from Google Earth Engine
	- 01_extract_combine_pop.R

### Current and future temperature data 
4) Calculate ERA5 climatologies to debias with, upload to earth engine 
	- 02_era5_climatology.R 
5) Download worldclim climatologies, upload to earth engine
6) Calculate monthly temperature for relevant periods with debiased ERA5 using earth engine 
	- 03_extract_temperature_gee_daily.R  
7) Calculate temperature change in CMIP6 climatologies, upload to earth engine
	- 04_select_gcm_climatologies.R -- select which climatologies to use
	- 05_get_cmip6_climatologies.py -- calculate dT for selected climatologies and save as rasters
8) Calculate temperature in the future using ERA5 with dT from above 
	- 06_extract_climate_scenario_temperature_gee_daily.R 

### Dengue data 

9) Clean all of the dengue data by country 
  - Download dengue data by country as described in supplemental table S3
	- 07_clean_dengue_data

## Clean and merge data 
10) Combine dengue and temperature data 
	- 08_combine_dengue_temp.R
11) Clean up cross-sectional variables for heterogeneity analyses 
	- 09_clean_cross_sectional_covariates.R 

## Fit regressions and run projections
12) Fit main regressions and sensitivity analyses, including bootstrap of preferred model 
	- 10_fit_main.R
	- 11_bootstrap_main.R
13) Fit heterogeneity regressions, including bootstrap of continent-specific analysis 
	- 12_fit_heterogeneous.R
	- 13_bootstrap_heterogeneous.R
	- 13a_bootstrap_heterogeneous.sbatch
14) Run projections under climate scenarios using existing model fits 
	- 14_dengue_projections.R
	- 14a_dengue_projections_main.sbatch
	- 14b_dengue_projections_het.sbatch

## Make figures and tables
15) Make main text figures 
	- 15_figure1_data.R
	- 16_figure2_S5_main_response.R
	- 17_figure3_heterogeneity.R
	- 18_figure4_historical_impacts.R
	- 19_figure5_S12_S13_future_projections.R
16) Make supplemental figures illustrating data 
	- 20a_ee_extract_endemic_countries_temps.R
	- 20b_figure_S1_endemic_countries.R
	- 21_figure_S2_S3_dengue_viz.R
17) Simulate disease dynamics and estimate response 
	- 22_figures_S4_S8_S15_simulations.R
18) Compare magnitude of impacts 
	- 23_figure_S6_compare_anomalies.R
19) Make figures of other responses (precipitation and by immunity)
	- 24_figure_S7_precipitation_response.R
	- 25_figures_S9_S10_immunity_response.R
20) Visualize the climate change scenario temperature differences
	- 26_figure_S11_scenario_dT.R
21) Calculate numbers for manuscript 
	- 27_manuscript_numbers.R

## Supplemental scripts/analyses 
1) Set up scripts called by other scripts 
	- 00_utilities/functions.R
	- 00_utilities/mat_mult.cpp
	- 00_utilities/setup_private.R
	- 00_utilities/setup.R
2) comparison of ERA5 and worldclim and why we need to debias 
	- 00_setup/A_temperature_check_download_station_data.R
	- 00_setup/B_temperature_check_ee_extract_temps.R
	- 00_setup/C_temperature_check_comparison_figure_S14.R
	