import os
import ee
import pandas as pd
from tqdm import tqdm
import time

os.chdir("D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\Data")

# Initialize the Earth Engine library
ee.Authenticate(auth_mode='localhost')
ee.Initialize(project='landcovercopernicus')

# --- Carica i punti ---
start_date = '2018-04-01'
end_date   = '2018-07-31'
orbit_pass = 'ASCENDING'  # O 'DESCENDING', oppure togli per entrambe

batch_size = 1000
output_csv = 'VH_VV_Italy_2018.csv'

# --- Carica i punti e crea bounding box su tutta l'area ---
df = pd.read_csv("italy_points.csv")
bbox = ee.Geometry.BBox(df['longitude'].min(), df['latitude'].min(),
                        df['longitude'].max(), df['latitude'].max())

# --- Crea mosaico Sentinel-1 su tutta l'area ---
collection = ee.ImageCollection('COPERNICUS/S1_GRD') \
    .filterDate(start_date, end_date) \
    .filterBounds(bbox) \
    .filter(ee.Filter.eq('instrumentMode', 'IW')) \
    .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV')) \
    .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH')) \
    .filter(ee.Filter.eq('orbitProperties_pass', orbit_pass))

if collection.size().getInfo() == 0:
    raise Exception("Nessuna immagine Sentinel-1 trovata nel periodo e area selezionati!")

image = collection.mosaic()
bands_needed = ['VV', 'VH']

# --- Funzione per processare un batch di punti ---
def process_batch(batch_df):
    features = []
    for _, row in batch_df.iterrows():
        point = ee.Geometry.Point([row['longitude'], row['latitude']])
        feature = ee.Feature(point, {'POINT_ID': int(row['POINT_ID'])})
        features.append(feature)
    fc = ee.FeatureCollection(features)
    try:
        sampled = image.select(bands_needed).sampleRegions(
            collection=fc,
            scale=10,
            geometries=False
        ).getInfo()
        rows = []
        for f in sampled['features']:
            props = f['properties']
            rows.append(props)
        return rows
    except Exception as e:
        print(f"Errore batch: {e}")
        return []

# --- Loop sui batch ---
all_results = []
num_batches = (len(df) + batch_size - 1) // batch_size

for i in tqdm(range(num_batches), desc="Batch processing"):
    start_idx = i * batch_size
    end_idx = min((i + 1) * batch_size, len(df))
    batch_df = df.iloc[start_idx:end_idx]
    batch_results = process_batch(batch_df)
    all_results.extend(batch_results)
    # Salva risultati parziali dopo ogni batch
    pd.DataFrame(all_results).to_csv(output_csv, index=False)
    time.sleep(1)  # evita troppi request ravvicinati

print(f"Completato! Output: {output_csv} ({len(all_results)} record)")
