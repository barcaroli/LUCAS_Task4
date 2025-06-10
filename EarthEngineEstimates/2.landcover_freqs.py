#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Created on Fri Sep  8 15:52:32 2023

@authors: faustobottini, michaelodonkor

Install the Google Cloud SDK
pip install earthengine-api
pip install earthengine-api --upgrade
"""

import os
import ee
import pandas as pd
import json

# Set the working directory
os.chdir("D:/Google Drive/LUCAS Copernicus/EarthEngine/")
# Alternative for another user:
# os.chdir("C:\\Users\\UTENTE\\Google Drive\\LUCAS Copernicus\\DynamicWorld")

# Initialize the Earth Engine library
ee.Authenticate(auth_mode='localhost')
ee.Initialize(project='landcovercopernicus')

# Create a 'data' folder if it does not exist
if not os.path.exists('data'):
    os.makedirs('data')

# Set parameters for analysis: country, start and end date, months offset
country = 'Italy'
start_Date = '2018-01-01'
end_Date = '2018-12-31'
months_offset = 1

# Create a list of date pairs for the specified period and offset
# Each pair defines the start and end date of a moving window (months_offset months long)
dates = pd.date_range(start_Date, end_Date, freq=pd.DateOffset(months=months_offset)).strftime("%Y-%m-%d").tolist()
pairs = list(zip(dates[:-1], dates[1:]))

# Load the geojson file with regional boundaries
path = 'italy_regions_gadm.geojson'
with open(path, 'r') as data_file:
    data = json.load(data_file)

# Create a FeatureCollection from the geojson
countries = ee.FeatureCollection(data)

# Set field names (as present in the geojson) for region identification
feature_name_1 = 'GID_1'     # Code for the region
feature_name_2 = 'NAME_1'    # Name of the region
feature_id = 'NAME_1'        # Used to filter by region

# Get sorted lists of region names and codes
districts = sorted(countries.aggregate_array(feature_name_2).getInfo())
codes = sorted(countries.aggregate_array(feature_id).getInfo())

# --- Helper functions for geometry handling and result extraction ---

# For MultiPolygon geometries in the geojson
def ifMultipolygon(Info):
    return ee.Geometry.MultiPolygon(Info['features'][0]['geometry']['coordinates'])

# For Polygon geometries
def ifPolygon(Info):
    return ee.Geometry.Polygon(Info['features'][0]['geometry']['coordinates'])

# Extract land cover class histogram from Dynamic World collection for a given geometry and period
def geometry2res(geom, startDate, endDate):
    # Filter Dynamic World images by date and region
    dw = ee.ImageCollection('GOOGLE/DYNAMICWORLD/V1').filterDate(startDate, endDate).filterBounds(geom)
    classification = dw.select('label')
    # Create a composite image by taking the mode of pixel labels
    dwComposite = classification.reduce(ee.Reducer.mode())
    dwComposite = dwComposite.rename('classification')
    # Calculate class frequencies (histogram) for the region
    pixelCountStats = dwComposite.reduceRegion(
        **{'reducer': ee.Reducer.frequencyHistogram().unweighted(), 'geometry': geom, 'scale': 10, 'maxPixels': 1e10}
    )
    pixelCounts = ee.Dictionary(pixelCountStats.get('classification'))
    return pixelCounts.getInfo()

# Extract results for a region (by code) and a date interval
def OneDisOutput_2level(start, end, code):
    filter_area = ee.Filter.inList(feature_id, [code])
    filteredArea = countries.filter(filter_area)
    Info = filteredArea.getInfo()
    code = Info['features'][0]['properties'][feature_id]
    type_ = Info['features'][0]['geometry']['type']
    adm_1 = Info['features'][0]['properties'][feature_name_1]
    
    # Handle different geometry types
    if type_ == 'MultiPolygon':
        geom = ifMultipolygon(Info)
    elif type_ == 'Polygon':
        geom = ifPolygon(Info)
    elif type_ == 'GeometryCollection':
        res = {}
        # Loop over all sub-geometries and aggregate results
        for i in range(len(Info['features'][0]['geometry']['geometries'])):
            type2 = Info['features'][0]['geometry']['geometries'][i]['type']
            if type2 == 'Polygon':
                geom = ee.Geometry.Polygon(Info['features'][0]['geometry']['geometries'][i]['coordinates'])
                a = geometry2res(geom, start, end)
                res.update(a)
        # Return the aggregated result
    res = geometry2res(geom, start, end)
    # Add metadata to the result
    res['code'] = code
    res['region'] = adm_1
    res['startDate'] = start
    res['endDate'] = end
    # If there is no 'null' class, set it to 0
    if 'null' not in res:
        res['null'] = 0
    return res

# --- Main script execution ---

# Open a log file to record successes/failures
log_file = open('log.txt', 'a')

# Resume from a checkpoint if exists, otherwise start fresh
if os.path.exists("checkpoint_df.csv"):
    l = [pd.read_csv("checkpoint_df.csv")]
else:
    l = []

i_temp = 0

# Main processing loop: for each region code, for each date window, extract results
for code in codes:
    i_temp += 1
    print(code, i_temp, '/', len(codes))
    for start_date, end_date in pairs:
        try:
            # Extract results for this region and time interval
            a = OneDisOutput_2level(start_date, end_date, code)
            log_file.write(f'OK, {code},{start_date}, {end_date},\n')
            b = pd.DataFrame([a])
            l.append(b)
        except:
            log_file.write(f'NO, {code},{start_date}, {end_date},\n')
            print(code, start_date, end_date, 'NO')
    # Save checkpoint after each region (so progress is not lost if script is interrupted)
    pd.concat(l).to_csv("checkpoint_df.csv")

log_file.close()

# After processing all regions and intervals, concatenate and export the final dataframe
df = pd.concat(l)
df.to_excel("Italy2018/"+"_".join((country, end_Date, str(months_offset)))+ '.xlsx', sheet_name='sheet1')

# Optionally, delete the checkpoint file to clean up
os.remove("checkpoint_df.csv")
