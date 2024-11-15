#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Sat Aug 26 09:24:27 2023

@author: marissachilds
"""
# set the reference and future periods
ref_period = {"start": "1995-01-01", 
              "end": "2014-12-30"}

fut_period = {"start": "2040-01-01", 
              "end": "2059-12-30"}

# define which scenarios we want, and which periods we'll use for eac
scenario_dict = {"historical": ref_period | {"req": True}, 
                 "hist-nat": ref_period | {"req": False}, 
                 "ssp126": fut_period | {"req": True}, 
                 "ssp245": fut_period | {"req": True}, 
                 "ssp370": fut_period | {"req": True}}

clim_output_path = "./data/climate_scenarios_dT_monthly/" 
 
# load package
import intake
from xmip.preprocessing import combined_preprocessing
import numpy as np
import xarray as xr
import pandas as pd
import s3fs
import time
import rioxarray
import os

# probably not the most efficient way to do this... but lets make a function 
# to check if the scenario is required based on the dictions
def check_scenario_req(pair):
    key, value = pair
    return value.get("req")

# make function to slice the xarray cmip6 data to relevant period, depending on scenario
def slice_period(ds):
    ds_period = scenario_dict.get(ds.attrs.get("experiment_id"))
    ds = ds.sel(time=slice(ds_period.get("start"), 
                           ds_period.get("end")))
    return(ds)

# define a preprocessing step for the cmip6 model data 
# this uses functions from general precprocessing from xmip, 
# with aggregation by month to get climatologies
def wrapper(ds):
    # ds = ds.copy()
    ds = combined_preprocessing(ds)
    # sort by time? stolen from chelsa script
    ds['time'] = np.sort(ds['time'].values)
    ds = slice_period(ds)
    # as a backup, save some information as attrs 
    # after the temporal slice for any debugging later
    ds = ds.assign_attrs(
        climatology_period_n_time = len(ds["time"]), 
        climatology_start = ds["time"].values[0],
        climatology_end = ds["time"].values[-1])
    # calculate averages over time, for each month
    ds = (ds.groupby("time.month")
          .mean("time"))
    return ds

t_init = time.time()

# based on the dictionary, identify the required scenarios for each model
req_scenarios = list(dict(filter(check_scenario_req, scenario_dict.items())).keys())

# use the catalog of scenarios on aws
url = "https://cmip6-pds.s3.amazonaws.com/pangeo-cmip6.json" 
cat_full = intake.open_esm_datastore(url)

cmip6_ds_included = pd.read_csv("./data/GCM_variant_scenarios_to_include.csv")

# take the existing catalog data frame, and use the limited set from above as the df
cat_sub = cat_full
cat_sub.esmcat._df = cmip6_ds_included
# make sure we group by the usual things + member_id so we can include multiple variants
cat_sub.esmcat.aggregation_control.groupby_attrs = cat_sub.esmcat.aggregation_control.groupby_attrs + ["member_id"]

# now use the catalog, and make a dataset dictionary, including the preprocessing we defined above
t0 = time.time()
dset_dict = cat_sub.to_dataset_dict(zarr_kwargs={'consolidated': True, 'decode_times':True},
                                    preprocess=wrapper,
                                    storage_options={'anon': 'True'})
t1 = time.time()
print("finished making dataset dictionary in " + str(t1 - t0) + " seconds")
# takes ~1.5 - 2 min 


# double check all of the climatologies have the right length of time    
for k, ds in dset_dict.items():
    if(ds.attrs.get("climatology_period_n_time") != 240): 
        print(k 
              + " has " 
              + str(ds.attrs.get("climatology_period_n_time")) 
              + " time periods (expected 240)")
# if nothing prints, you're good to go

# for each gcm-variant pair, now go through and difference the scenarios from historical, 
# saving the output of monthly dT for each scenario as a separte tiff file
# before any of the saving, move to the desired output folder
os.chdir(clim_output_path)
gcm_variants = cat_sub.df[["source_id", "member_id"]].drop_duplicates()
#for gcm in cat_sub.df["source_id"].unique():
for index, row in gcm_variants.iterrows():
    t0 = time.time()
    gcm = row["source_id"] 
    variant = row["member_id"]
    print(gcm, variant)
    # grab the catalog df, and filter to the gcm and variant of interest
    gcm_df = cat_sub.keys_info()
    gcm_df = gcm_df.query("source_id == '" + gcm + "' & member_id == '" + variant + "'")
    
    # identify which non-historical scenarios that gcm has
    gcm_scenarios = np.setdiff1d(np.array(gcm_df["experiment_id"]), 
                                 np.array(["historical"]))
    # grab the key for the historical data 
    gcm_hist_key = gcm_df.query("experiment_id == 'historical'").index.values[0]
    print("  historical key:" + gcm_hist_key)
    for scenario in gcm_scenarios:
        print("     " + scenario)
        gcm_scenario_key = gcm_df.query("experiment_id == '" + scenario + "'").index.values[0]
        print("       key: " + gcm_scenario_key)
        # calculate change in temperatur betewen scenario and historical
        # drop the extra dimensions added by preproccessing 
        dT = dset_dict[gcm_scenario_key].squeeze() - dset_dict[gcm_hist_key].squeeze()
        outname = (gcm 
                   + "_" 
                   + variant 
                   + "_"
                   + scenario
                   + ".tif")
        # outname = f“{gcm}_{variant}_{scenario}.tif”
        out_rast = dT["tas"]
        out_rast.rio.write_crs("epsg:4326", inplace=True)
        out_rast.rio.to_raster(raster_path=outname)
    t1 = time.time()
    print(gcm + " " + scenario + " finished in " + str(t1 - t0) + " seconds")

t_end = time.time()
print("total time: " + str(t_init - t_end))

# debugging KACE
# (dset_dict["ScenarioMIP.NIMS-KMA.KACE-1-0-G.ssp370.Amon.gr"]
#      .sel(time=slice('2040-01-01', 
#                      '2059-12-30')))
# KACE fails because its 360 days/year so Dec 31 doesn't exist
# dset_dict["ScenarioMIP.NIMS-KMA.KACE-1-0-G.ssp370.Amon.gr"]["time"]
# but Dec 30 does, and will work for everyone

# Qs/problems: 
# find ref on unreliable cmip6 daily values 
# run checks on the cmip6 images 
# do we want precip? 

# SCRATCH --------        

# FGOALS-f3-L works when running alone or with MIROC6, not sure why it wasn't working before? 
# 'KACE-1-0-G' works without the wrapper, but not with it --> KACE-1-0-G fails at slice_period because its 360 days per year, so need to end with 12-30 not 12-31


# limit to monthly aggregates of avg temp
# query = dict(experiment_id=scenario_dict.keys(), 
#              table_id="Amon", 
#              variable_id='tas')
# cat_search = cat_full.search(**query)

# among those options, limit to the first member_id for each source_id and scenario
# cat_sub_df = (cat_search.df.groupby(["source_id", "experiment_id"])
#            .first()
#            .reset_index()) 
# then limit to only GCMs with all the future scenarios we want
# cat_sub_df["n_scenario_missing"] = (cat_sub_df
#     .groupby("source_id")["experiment_id"]
#     .transform(
#         lambda x: 
#             len(np.setdiff1d(np.array(req_scenarios), np.array(x.unique())))))
    
# cat_sub_df = cat_sub_df.query('n_scenario_missing == 0')
# some of the models don't seem to work well, maybe with the wrapper? 
# lets ignore them for now and circle back later
# cat_sub_df = cat_sub_df.query("source_id == ['KACE-1-0-G']")
# , 'KACE-1-0-G'
# 'FGOALS-f3-L'

# cat_sub_df = cat_sub.df
# update the catalog of the original search 
# this should be straightforward but wasn't. but this issue helped https://github.com/intake/intake-esm/issues/246
# cat_search.esmcat._df = cat_sub_df# .reset_index()


# dset_dict[gcm_scenario_key].squeeze()["tas"].rio.to_raster(raster_path="dT_test.tif")


# out = dset_dict[gcm_scenario_key].squeeze()["tas"]
# out.rio.write_crs("epsg:4326", inplace=True)
# out.rio.to_raster(raster_path="dT_test.tif")

        
#         dT["tas"].rio.to_raster(raster_path=outname)

        

# dT["tas"].rio.to_raster(raster_path="dT_test.tif")

# still do do: 
# add crs when saving raster  
# saving raster with name defined by gcm and scenario... 


# check the output looks reasonable when you plot it --> good for the single example, try again with latest code
# check that the resulting tif can be loaded into earth engine


# say its 3 mins for each scenario to load, and get climatologies
# so 3 min x 5 scenarios (historical, hist-nat, ssp126, ssp245, ssp370) x 38 models --> ~ 9 hours in serial, not counting differencing between scenarios
# we'll need to store ~ 2.7 MB x 4 scenario dT x 38 models -->  410 MB, so not so bad
# locally, lets just run this, save the dT, add them

# from xmip.preprocessing import (promote_empty_dims, 
#                                 broadcast_lonlat, 
#                                 replace_x_y_nominal_lat_lon, 
#                                 correct_lon, 
#                                 correct_coordinates, 
#                                 correct_units,
#                                 parse_lon_lat_bounds, 
#                                 maybe_convert_bounds_to_vertex, 
#                                 maybe_convert_vertex_to_bounds, 
#                                 rename_cmip6) 

    # ds = rename_cmip6(ds)
    # ds = correct_units(ds)
    # ds = promote_empty_dims(ds)
    # ds = broadcast_lonlat(ds)
    # ds = replace_x_y_nominal_lat_lon(ds)
    # ds = correct_lon(ds)
    # ds = correct_coordinates(ds)
    # ds = parse_lon_lat_bounds(ds)
    # ds = maybe_convert_bounds_to_vertex(ds)
    # ds = maybe_convert_vertex_to_bounds(ds)


