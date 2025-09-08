# -------------------------------------------------------
# Script to download homogeneous Google Satellite Embedding data
# (based on 2018 band availability) from 2018 to 2022
# With retry logic and partial saving
# -------------------------------------------------------

#from datetime import datetime
import os
import ee
import pandas as pd
from tqdm import tqdm
import time

# -------------------------------------------------------
# 1. INITIAL SETTINGS
# -------------------------------------------------------
os.chdir("C:\\Users\\UTENTE\\Google Drive laptop\\LUCAS Copernicus\\EarthEngine\\ALPHAEARTH\\data")

# Earth Engine authentication
ee.Authenticate(auth_mode='localhost')
ee.Initialize(project='landcovercopernicus')

# Load point data
df = pd.read_csv("italy_master_points.csv")

# Bounding box for filtering
bbox = ee.Geometry.BBox(
    df['longitude'].min(),
    df['latitude'].min(),
    df['longitude'].max(),
    df['latitude'].max()
)

# Parameters
start_year = 2018
end_year = 2022
batch_size = 1000

# -------------------------------------------------------
# 2. GET BAND NAMES FROM 2018 IMAGE (REFERENCE YEAR)
# -------------------------------------------------------
def get_reference_band_names(year, bbox):
    collection = (
        ee.ImageCollection("GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL")
        .filterDate(f"{year}-01-01", f"{year}-12-31")
        .filterBounds(bbox)
    )
    image = collection.first()
    if image is None:
        raise RuntimeError(f"⚠️ No image found for reference year {year}")
    
    band_names = image.bandNames().getInfo()
    raw_bands = [b for b in band_names if b.startswith('A')]
    if not raw_bands:
        raise RuntimeError(f"⚠️ No embedding bands (A00–A63) found in {year}")
    
    return raw_bands

# -------------------------------------------------------
# 3. LOAD IMAGE WITH REFERENCE BANDS
# -------------------------------------------------------
def get_image_with_reference_bands(year, bbox, reference_bands):
    collection = (
        ee.ImageCollection("GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL")
        .filterDate(f"{year}-01-01", f"{year}-12-31")
        .filterBounds(bbox)
    )
    image = collection.first()
    if image is None:
        print(f"⚠️ No image found for {year}")
        return None

    image_band_names = image.bandNames().getInfo()
    if not all(b in image_band_names for b in reference_bands):
        print(f"⚠️ Year {year} missing required bands, skipping")
        return None

    return image.select(reference_bands)

# -------------------------------------------------------
# 4. SAMPLE A BATCH OF POINTS WITH RETRIES
# -------------------------------------------------------
def process_batch(batch_df, image, max_retries=3, delay_sec=5):
    features = [
        ee.Feature(ee.Geometry.Point([row['longitude'], row['latitude']]), {'POINT_ID': int(row['POINT_ID'])})
        for _, row in batch_df.iterrows()
    ]
    fc = ee.FeatureCollection(features)

    for attempt in range(1, max_retries + 1):
        try:
            sampled = image.sampleRegions(collection=fc, scale=10, geometries=False).getInfo()
            return [f['properties'] for f in sampled['features']]
        except Exception as e:
            print(f"⚠️ Batch error (attempt {attempt}): {e}")
            if attempt < max_retries:
                print(f"🔁 Retrying in {delay_sec} seconds...")
                time.sleep(delay_sec)
            else:
                print("❌ Giving up on this batch.")
                return []

# -------------------------------------------------------
# 5. MAIN EXECUTION LOOP (2018–2022)
# -------------------------------------------------------
# Step 1: Get 2018 reference band names
reference_bands = get_reference_band_names(2018, bbox)
print(f"✅ Reference band names from 2018: {reference_bands}")

# Step 2: Loop over all years
for year in range(start_year, end_year + 1):
    print(f"\n📅 Processing year {year}")
    image = get_image_with_reference_bands(year, bbox, reference_bands)
    if image is None:
        continue

    all_results = []
    num_batches = (len(df) + batch_size - 1) // batch_size

    for i in tqdm(range(num_batches), desc=f"Batch {year}"):
        batch_df = df.iloc[i * batch_size : min((i + 1) * batch_size, len(df))]
        batch_results = process_batch(batch_df, image)
        all_results.extend(batch_results)
        time.sleep(1)

    df_year = pd.DataFrame(all_results)

    if df_year.empty or 'POINT_ID' not in df_year.columns:
        print(f"⚠️ No usable data returned for year {year}, skipping.")
        continue

    # Rename raw bands to readable EMBEDDING_0, EMBEDDING_1, ...
    rename_dict = {band: f'EMBEDDING_{i}' for i, band in enumerate(reference_bands) if band in df_year.columns}
    df_year = df_year.rename(columns=rename_dict)

    # Build ordered column list
    ordered_cols = ['POINT_ID'] + [rename_dict[b] for b in reference_bands if b in rename_dict]

    missing = [col for col in ordered_cols if col not in df_year.columns]
    if missing:
        print(f"⚠️ Some expected columns missing for year {year}: {missing}")
        print("Available columns:", df_year.columns.tolist())
        continue

    df_year = df_year[ordered_cols]

    # Save output even if partial
    output_file = f"SatelliteEmbedding_Italy_{year}.csv"
    df_year.to_csv(output_file, index=False)
    print(f"✅ File saved: {output_file}")
