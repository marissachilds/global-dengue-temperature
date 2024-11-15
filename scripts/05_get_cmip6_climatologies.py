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

# make a function to check if the scenario is required based on the dictions
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
