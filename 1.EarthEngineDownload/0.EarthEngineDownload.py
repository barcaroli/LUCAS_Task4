# -------------------------------------------------------
# Script to download Earth Engine values
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
#!pip install pandas
#!pip install tqdm
#!pip install time
import os
import ee
import pandas as pd
from tqdm import tqdm
import time

# -------------------------------------------------------
# 1. IMPOSTAZIONI INIZIALI
# -------------------------------------------------------

# (1) Cartella di lavoro:
#os.chdir("D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\Values")
os.chdir("C:\\Users\\HP\\Documents\\LUCAS Copernicus\\EarthEngine\\Data")

# (2) Autenticazione e inizializzazione di Earth Engine
ee.Authenticate(auth_mode='localhost')
ee.Initialize(project='landcovercopernicus')

# (3) Definisci il periodo di interesse (modifica se necessario)
start_date = '2021-04-01'
end_date   = '2021-07-31'

# (4) Per Sentinel-1: pass di orbita ('ASCENDING' o 'DESCENDING'; stringa vuota = entrambe)
orbit_pass = 'ASCENDING'

# (5) Parametri di batching e output
batch_size = 1000
output_csv = 'S2_S1_GLCM_Italy_2021.csv'

# (6) Bande Sentinel-2 che vogliamo (SR Harmonized)
bands_s2 = ['B1','B2','B3','B4','B5','B6','B7','B8','B8A','B9','B11','B12']

# (7) Bande Sentinel-1 (in dB) che vogliamo direttamente
bands_s1 = ['VV','VH']

# (8) Parametri GLCM
#     - window di analisi: 3×3 (puoi cambiarla se desideri)
glcm_window = 3

# -------------------------------------------------------
# 2. CARICA I PUNTI E CREA IL BOUNDING BOX
# -------------------------------------------------------

# Attendi di avere un CSV con almeno colonne: POINT_ID, longitude, latitude
#df = pd.read_csv("italy_master_points.csv")
n = 1  # numero di record da skippare
df = pd.read_csv(
    "italy_master_points.csv",
    skiprows=range(1, n+1)   # salta le righe 1,2,…,n (la riga 0 è l’header)
)

# Bounding box su tutta l'area interessata
bbox = ee.Geometry.BBox(
    df['longitude'].min(),
    df['latitude'].min(),
    df['longitude'].max(),
    df['latitude'].max()
)

# -------------------------------------------------------
# 3. CREAZIONE DEL MOSAICO SENTINEL-2 B1–B12
# -------------------------------------------------------

collection_s2 = (
    ee.ImageCollection('COPERNICUS/S2_SR_HARMONIZED')
      .filterDate(start_date, end_date)
      .filterBounds(bbox)
      .filter(ee.Filter.lt('CLOUDY_PIXEL_PERCENTAGE', 20))
)

if collection_s2.size().getInfo() == 0:
    raise Exception("❌ Nessuna immagine Sentinel-2 trovata nel periodo/area selezionati!")

# Crea il mosaico e seleziona le bande B1…B12
image_s2 = collection_s2.mosaic().select(bands_s2)

# -------------------------------------------------------
# 4. CREAZIONE DEL MOSAICO SENTINEL-1 VV, VH (in dB)
# -------------------------------------------------------

collection_s1 = (
    ee.ImageCollection('COPERNICUS/S1_GRD')
      .filterDate(start_date, end_date)
      .filterBounds(bbox)
      .filter(ee.Filter.eq('instrumentMode', 'IW'))
      .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VV'))
      .filter(ee.Filter.listContains('transmitterReceiverPolarisation', 'VH'))
)

# Se hai specificato un passaggio orbita, aggiungi il filtro:
if orbit_pass:
    collection_s1 = collection_s1.filter(ee.Filter.eq('orbitProperties_pass', orbit_pass))

if collection_s1.size().getInfo() == 0:
    raise Exception("❌ Nessuna immagine Sentinel-1 trovata nel periodo/area selezionati!")

# Crea il mosaico e seleziona VV e VH (in dB)
image_s1 = collection_s1.mosaic().select(bands_s1)

# -------------------------------------------------------
# 5. CALCOLO DELLE TEXTURE GLCM SU VV E VH (LINEARIZZATI)
# -------------------------------------------------------

# 5.1 Linearizza i valori dB in valore lineare * 1000, convertendo in uint16
#    - formula Earth Engine: 10^(dB/10) -> moltiplico *1000 per portare il valore in un range utile
image_vv_lin = (
    ee.Image(10)
      .pow(image_s1.select('VV').divide(10))
      .multiply(1000)
      .toUint16()
      .rename('VV_lin')
)
image_vh_lin = (
    ee.Image(10)
      .pow(image_s1.select('VH').divide(10))
      .multiply(1000)
      .toUint16()
      .rename('VH_lin')
)

# Concatena le due bande linearizzate in un'unica immagine
image_lin = image_vv_lin.addBands(image_vh_lin)

# 5.2 Applica glcmTexture con finestra (3×3) a ciascuna banda separatamente
glcm_vv = image_lin.select('VV_lin').glcmTexture(size=glcm_window)
glcm_vh = image_lin.select('VH_lin').glcmTexture(size=glcm_window)

# 5.3 Seleziona le metriche che ti interessano:
#     - dal GLCM su VV_lin prendo: VV_lin_savg, VV_lin_var, VV_lin_corr
#     - dal GLCM su VH_lin prendo: VH_lin_savg, VH_lin_var, VH_lin_corr
glcm_vv_sel = glcm_vv.select(['VV_lin_savg', 'VV_lin_var', 'VV_lin_corr'])
glcm_vh_sel = glcm_vh.select(['VH_lin_savg', 'VH_lin_var', 'VH_lin_corr'])

# 5.4 Unisci le metriche selezionate in una singola immagine GLCM
glcm_all = glcm_vv_sel.addBands(glcm_vh_sel)

# -------------------------------------------------------
# 6. UNIONE FINALE DI TUTTE LE BANDE IN UN UNICO IMAGE
# -------------------------------------------------------

# (a) Parti da Sentinel-2
combined = image_s2

# (b) Aggiungi VV e VH in dB
combined = combined.addBands(image_s1.select(bands_s1))

# (c) Aggiungi le metriche GLCM (VV_lin_* e VH_lin_*)
combined = combined.addBands(glcm_all)

# Ora “combined” contiene in ordine:
#   - B1…B12  (12 bande S2)
#   - VV, VH  (2 bande S1 in dB)
#   - VV_lin_savg, VV_lin_var, VV_lin_corr  (3 bande GLCM su VV)
#   - VH_lin_savg, VH_lin_var, VH_lin_corr  (3 bande GLCM su VH)
# Totale: 12 + 2 + 6 = 20 bande.

# -------------------------------------------------------
# 7. FUNZIONE PER CAMPIONARE I PUNTI (BATCH)
# -------------------------------------------------------

def process_batch(batch_df):
    """
    batch_df: DataFrame pandas con colonne POINT_ID, longitude, latitude.
    Restituisce una lista di dict con tutti i valori (B1..B12, VV, VH, GLCM_...) per ciascun punto.
    """
    # 7.1 Creo una lista di ee.Feature a partire dai punti del batch
    features = []
    for _, row in batch_df.iterrows():
        pt = ee.Geometry.Point([row['longitude'], row['latitude']])
        feat = ee.Feature(pt, {'POINT_ID': int(row['POINT_ID'])})
        features.append(feat)
    fc = ee.FeatureCollection(features)

    try:
        # 7.2 SampleRegions: estraggo tutti i valori di "combined" per ciascun punto
        sampled = combined.sampleRegions(
            collection=fc,
            scale=10,          # 10 metri di risoluzione (adeguato a S2 e S1 qui)
            geometries=False   # non serve restituire la geometria
        ).getInfo()

        rows = []
        for f in sampled['features']:
            props = f['properties']
            rows.append(props)
        return rows

    except Exception as e:
        print(f"⚠️ Errore batch (probabilmente troppi punti o timeout): {e}")
        return []

# -------------------------------------------------------
# 8. LOOP PRINCIPALE PER ELABORARE TUTTI I BATCH E SALVARE
# -------------------------------------------------------

all_results = []
num_batches = (len(df) + batch_size - 1) // batch_size

for i in tqdm(range(num_batches), desc="Elaborazione batch"):
    start_idx = i * batch_size
    end_idx   = min((i + 1) * batch_size, len(df))
    batch_df  = df.iloc[start_idx:end_idx]

    batch_results = process_batch(batch_df)
    all_results.extend(batch_results)

    # 8.1 Salvataggio parziale dopo ogni batch (per evitare di perdere tutto in caso di interruzione)
    pd.DataFrame(all_results).to_csv(output_csv, index=False)

    # 8.2 Breve pausa per non sovraccaricare il server EE
    time.sleep(1)

print(f"✅ Tutto completato! Il file è stato salvato in: {output_csv}  ({len(all_results)} record totali)")



