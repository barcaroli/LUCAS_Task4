import os
import ee
import pandas as pd
from tqdm import tqdm
import time

# -------------------------------------------------------
# 1. IMPOSTAZIONI INIZIALI
# -------------------------------------------------------
os.chdir("D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\Data")
ee.Authenticate(auth_mode='localhost')
ee.Initialize(project='landcovercopernicus')

start_date    = '2022-01-01'
end_date      = '2022-12-31'
orbit_pass    = 'ASCENDING'
batch_size    = 1000

# due nomi di output separati
output_mean   = 'Italy_water_2022_mean.csv'
output_median = 'Italy_water_2022_median.csv'

bands_s2 = ['B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12']
bands_s1 = ['VV','VH']

# -------------------------------------------------------
# 2. CARICA I PUNTI
# -------------------------------------------------------
df = pd.read_csv("Italy_water_points.csv", skiprows=range(1,2))
bbox = ee.Geometry.BBox(
    df.longitude.min(), df.latitude.min(),
    df.longitude.max(), df.latitude.max()
)

# -------------------------------------------------------
# 3. COSTRUISCI LE COLLEZIONI
# -------------------------------------------------------
col_s2 = (ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
            .filterDate(start_date, end_date)
            .filterBounds(bbox)
            .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20)))
if col_s2.size().getInfo() == 0:
    raise Exception("❌ Nessuna immagine S2!")

col_s1 = (ee.ImageCollection('COPERNICUS/S1_GRD')
            .filterDate(start_date, end_date)
            .filterBounds(bbox)
            .filter(ee.Filter.eq('instrumentMode','IW'))
            .filter(ee.Filter.listContains('transmitterReceiverPolarisation','VV'))
            .filter(ee.Filter.listContains('transmitterReceiverPolarisation','VH')))
if orbit_pass:
    col_s1 = col_s1.filter(ee.Filter.eq('orbitProperties_pass',orbit_pass))
if col_s1.size().getInfo() == 0:
    raise Exception("❌ Nessuna immagine S1!")

# -------------------------------------------------------
# 4. CREA I DUE COMPOSITE (MEAN e MEDIAN) PER OGNI SENSORE
# -------------------------------------------------------
# Sentinel-2
mean_s2   = col_s2.select(bands_s2).mean()
median_s2 = col_s2.select(bands_s2).median()

# Sentinel-1 (in dB)
mean_s1   = col_s1.select(bands_s1).mean()
median_s1 = col_s1.select(bands_s1).median()

# -------------------------------------------------------
# 5. UNISCI IN DUE IMMAGINI "COMBINED"
# -------------------------------------------------------
combined_mean = mean_s2.addBands(mean_s1)
combined_median = median_s2.addBands(median_s1)

# -------------------------------------------------------
# 6. FUNZIONE DI CAMPIONAMENTO GENERICA
# -------------------------------------------------------
def sample_points(image, points_df):
    feats = [ee.Feature(ee.Geometry.Point([r['longitude'],r['latitude']]),
                        {'POINT_ID':int(r['POINT_ID'])})
             for _,r in points_df.iterrows()]
    fc = ee.FeatureCollection(feats)
    sampled = image.sampleRegions(
        collection=fc,
        scale=10,
        geometries=False
    ).getInfo()['features']
    return [f['properties'] for f in sampled]

# -------------------------------------------------------
# 7. LOOP PER I DUE COMPOSITE E SALVATAGGIO
# -------------------------------------------------------
for img, out_csv in [(combined_mean, output_mean),
                     (combined_median, output_median)]:

    all_res = []
    n_batches = (len(df)+batch_size-1)//batch_size

    for i in tqdm(range(n_batches), desc=f"Batch per {out_csv}"):
        sub = df.iloc[i*batch_size : (i+1)*batch_size]
        res = sample_points(img, sub)
        all_res.extend(res)
        # salvataggio incrementale
        pd.DataFrame(all_res).to_csv(out_csv, index=False)
        time.sleep(1)

    print(f"✅ {out_csv} creato con {len(all_res)} record")
