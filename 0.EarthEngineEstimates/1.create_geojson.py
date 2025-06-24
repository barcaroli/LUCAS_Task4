# -*- coding: utf-8 -*-
"""
Created on Mon Dec  4 12:15:20 2023
First, download the json file from https://gadm.org/download_country.html
(for NUTS2 it is level 1)
@author: Giulio
"""
import os
os.chdir("C:\\Users\\UTENTE\\Google Drive laptop\\LUCAS Copernicus\\EarthEngine\\EarthEngineEstimates")
import geopandas as gpd
file_path = 'Italy/italy_gadm.json'  # Replace with your file path
italy = gpd.read_file(file_path)
# lazio_region = italy_regions[italy_regions['NAME_1'] == 'Lazio']
# Export the Lazio region to a new GeoJSON file
italy.to_file('Italy/italy_gadm.geojson', driver='GeoJSON')

