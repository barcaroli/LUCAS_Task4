# -------------------------------------------------------
# Script to download Earth Engine yearly trajectories
# -------------------------------------------------------
#----------------------------------------
# To run in RStudio, execute first these in R console:
# library(reticulate)
# use_python("C:/PythonEnv/my_env/Scripts/python.exe", required = TRUE)
# py_config()
#----------------------------------------

#!pip install os
#!pip install ee
#!pip uninstall earthengine-api blessings
#!pip install earthengine-api --no-deps
#!pip install google-api-python-client google-auth httplib2
#!pip install datetime
#!pip install pandas
#!pip install tqdm
#!pip install time
from datetime import datetime, timedelta
import os
import ee
import pandas as pd
from tqdm import tqdm
import time
from functools import reduce

# -------------------------------------------------------
# 1. INITIAL SETTINGS
# -------------------------------------------------------
os.chdir("D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\2.EarthEngineTrajectories\\")
ee.Authenticate(auth_mode='localhost')
ee.Initialize(project='landcovercopernicus')

# Input points
df = pd.read_csv("D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\italy_master_points.csv")
bbox = ee.Geometry.BBox(
    df['longitude'].min(),
    df['latitude'].min(),
    df['longitude'].max(),
    df['latitude'].max()
)

# Sentinel parameters
bands_s2 = ['B4', 'B8']
bands_s1 = ['VV', 'VH']
orbit_pass = 'ASCENDING'
glcm_window = 3
batch_size = 1000
year = 2022

# -------------------------------------------------------
# 2. FUNZIONE DI ELABORAZIONE IMMAGINE MENSILE
# -------------------------------------------------------
def compute_monthly_image(start_str, end_str, orbit_pass, bands_s2, bands_s1, bbox, glcm_window):
    s2 = (
        ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
        .filterDate(start_str, end_str)
        .filterBounds(bbox)
        .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
        .map(lambda img: img.select(bands_s2))
    )
    if s2.size().getInfo() == 0:
        print(f"⚠️ Nessuna immagine Sentinel-2 tra {start_str} e {end_str}, mese saltato.")
        return None

    image_s2 = s2.mosaic()

    s1 = (
        ee.ImageCollection('COPERNICUS/S1_GRD')
        .filterDate(start_str, end_str)
        .filterBounds(bbox)
        .filter(ee.Filter.eq('instrumentMode', 'IW'))
        .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
        .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
    )
    if orbit_pass:
        s1 = s1.filter(ee.Filter.eq('orbitProperties_pass', orbit_pass))
    if s1.size().getInfo() == 0:
        print(f"⚠️ Nessuna immagine Sentinel-1 tra {start_str} e {end_str}, mese saltato.")
        return None

    image_s1 = s1.mosaic().select(bands_s1)
    return image_s2.addBands(image_s1)

# -------------------------------------------------------
# 3. FUNZIONE DI CAMPIONAMENTO PER BATCH DI PUNTI
# -------------------------------------------------------
def process_batch(batch_df, combined):
    features = [
        ee.Feature(ee.Geometry.Point([row['longitude'], row['latitude']]), {'POINT_ID': int(row['POINT_ID'])})
        for _, row in batch_df.iterrows()
    ]
    fc = ee.FeatureCollection(features)
    try:
        sampled = combined.sampleRegions(collection=fc, scale=10, geometries=False).getInfo()
        return [f['properties'] for f in sampled['features']]
    except Exception as e:
        print(f"Errore batch: {e}")
        return []

# -------------------------------------------------------
# 4. LOOP PRINCIPALE PER OGNI MESE DEL 2022
# -------------------------------------------------------
monthly_dfs = []

for month in range(1, 13):
    start_date = datetime(year, month, 1)
    end_date = datetime(year, month + 1, 1) - timedelta(days=1) if month < 12 else datetime(year + 1, 1, 1) - timedelta(days=1)
    start_str = start_date.strftime('%Y-%m-%d')
    end_str = end_date.strftime('%Y-%m-%d')
    print(f"\n📅 Elaborazione {start_str} → {end_str}")

    combined = compute_monthly_image(start_str, end_str, orbit_pass, bands_s2, bands_s1, bbox, glcm_window)
    if combined is None:
        continue

    all_results = []
    num_batches = (len(df) + batch_size - 1) // batch_size

    for i in tqdm(range(num_batches), desc=f"Batch mese {month:02d}"):
        batch_df = df.iloc[i * batch_size : min((i + 1) * batch_size, len(df))]
        all_results.extend(process_batch(batch_df, combined))
        time.sleep(1)

    df_month = pd.DataFrame(all_results)
    cols_to_keep = ['POINT_ID', 'B4', 'B8', 'VV', 'VH']
    df_month = df_month[[c for c in cols_to_keep if c in df_month.columns]]
    df_month = df_month.rename(columns={
        'B4': f'B4_{month:02d}',
        'B8': f'B8_{month:02d}',
        'VV': f'VV_{month:02d}',
        'VH': f'VH_{month:02d}',
    })
    df_month.to_csv(f"2022\\S2_S1_Italy_2022_{month:02d}.csv", index=False)
    monthly_dfs.append(df_month)

# -------------------------------------------------------
# 5. UNIONE FINALE
# -------------------------------------------------------
if monthly_dfs:
    df_final = reduce(lambda left, right: pd.merge(left, right, on='POINT_ID', how='outer'), monthly_dfs)
    ordered_cols = ['POINT_ID'] + [f'{b}_{m:02d}' for b in ['B4', 'B8', 'VV', 'VH'] for m in range(1, 13) if f'{b}_{m:02d}' in df_final.columns]
    df_final = df_final[ordered_cols]
    df_final.to_csv("S2_S1_Italy_2022_Monthly.csv", index=False)
    print("✅ File finale salvato: S2_S1_Italy_2022_Monthly.csv")
else:
    print("❌ Nessun dato valido per il 2022: nessun file unificato generato.")