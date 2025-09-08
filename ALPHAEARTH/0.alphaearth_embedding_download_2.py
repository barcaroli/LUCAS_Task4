# -------------------------------------------------------
# Script to download homogeneous Google Satellite Embedding data
# (based on 2018 band availability) from 2018 to 2022
# With retry logic and partial saving
#   CREATE A SUBFOLDER ./AlphaEarth_Local/
# -------------------------------------------------------

import time
from datetime import datetime
import ee

# -------------------------------------------------------
# 1. INITIAL SETTINGS
# -------------------------------------------------------
os.chdir("C:\\Users\\UTENTE\\Google Drive laptop\\LUCAS Copernicus\\EarthEngine\\ALPHAEARTH\\data")

# Earth Engine authentication
ee.Authenticate(auth_mode='localhost')
ee.Initialize(project='landcovercopernicus')


# ======================== PARAMETRI UTENTE ================================= #
POINTS_ASSET   = 'projects/landcovercopernicus/assets/Italy_master_points'
ID_PROP        = 'POINT_ID'                   # ID numerico nei punti
YEARS          = [2018]                       # es. [2018,2019,2020,2021,2022,2023]

DRIVE_FOLDER   = 'EE_AlphaEarth'
FILE_BASENAME  = 'AlphaEarth_Italy'

SAMPLING_SCALE = 10                           # m (20–30 se vuoi più leggero)
TILE_SCALE     = 16                           # alza (20) se vedi abort per memoria
SPLITS         = 20                           # ~3.7k punti/blocco
RAND_SEED      = 12345                        # per split ripetibile

# Gestione invio task
ENABLE_EXPORTS     = True                     # False = “dry run” (non avvia task)
MAX_CONCURRENT     = 3                        # task RUNNING/READY contemporanei
POLL_INTERVAL_SEC  = 60                       # polling durante l’invio/monitoraggio

DATASET = 'GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL'
# =========================================================================== #


def ee_init():
    """Inizializza Earth Engine, chiedendo autenticazione se serve."""
    try:
        ee.Initialize()
    except Exception:
        print("EE non inizializzato. Avvio autenticazione…")
        ee.Authenticate()
        ee.Initialize()
    print("[EE] inizializzazione OK.")


def build_points_balanced(fc, id_prop, splits, seed):
    """Ritorna FC con id/lat/lon e bucket bilanciato via randomColumn."""
    seeded = fc.randomColumn('rnd', seed)

    def _map(f):
        coords = f.geometry().transform('EPSG:4326', 1).coordinates()
        lon = ee.Number(coords.get(0))
        lat = ee.Number(coords.get(1))
        pid_num = ee.Number.parse(ee.String(f.get(id_prop)))
        bucket  = ee.Number(f.get('rnd')).multiply(splits).floor()  # 0..splits-1
        return ee.Feature(f.geometry(), {
            'id': ee.String(pid_num.format('%.0f')),
            'longitude': lon,
            'latitude':  lat,
            'bucket':    bucket
        })

    return seeded.map(_map)


def italy_bounds(points_fc):
    return points_fc.geometry().dissolve().bounds()


def alphaearth_image(year, bounds):
    """Mosaico annuale sull’Italia."""
    col = (ee.ImageCollection(DATASET)
           .filterDate(ee.Date.fromYMD(year, 1, 1), ee.Date.fromYMD(year + 1, 1, 1))
           .filterBounds(bounds))
    print(f"Anno {year} – tasselli (Italia):", col.size().getInfo())
    return col.mosaic()


def sample_bucket(img, points_fc, year, bucket_idx):
    """Campiona un bucket; ritorna FC o None se bucket vuoto."""
    pts_subset = points_fc.filter(ee.Filter.eq('bucket', bucket_idx))
    n = pts_subset.size().getInfo()
    print(f"Anno {year} – bucket {bucket_idx+1}/{SPLITS}: punti={n}")
    if n == 0:
        return None
    fc = (img.sampleRegions(
            collection=pts_subset,
            properties=['id', 'latitude', 'longitude', 'bucket'],
            geometries=False,
            scale=SAMPLING_SCALE,
            tileScale=TILE_SCALE
         )
         .map(lambda ft: ft.set('year', year)))
    return fc


def make_drive_export(fc, desc, folder, prefix):
    return ee.batch.Export.table.toDrive(
        collection=fc,
        description=desc,
        folder=folder,
        fileNamePrefix=prefix,
        fileFormat='CSV'
    )


def task_state(task):
    try:
        return task.status().get('state', 'UNKNOWN')
    except Exception:
        return 'UNKNOWN'


def count_active(tasks):
    return sum(1 for t in tasks if task_state(t) in ('READY', 'RUNNING'))


def start_tasks_limited(tasks, max_concurrent, poll_s):
    """Invia i task rispettando il limite di concorrenza e monitora lo stato."""
    if not tasks:
        print("Nessun task da inviare.")
        return

    started = 0
    for t in tasks:
        while count_active(tasks) >= max_concurrent:
            act = count_active(tasks)
            print(f"[{datetime.now().strftime('%H:%M:%S')}] ACTIVE={act} ≥ {max_concurrent}. Sleep {poll_s}s…")
            time.sleep(poll_s)
        t.start()
        started += 1
        print(f"  → Avviato {started}/{len(tasks)}: {t.status().get('description','')}")
        time.sleep(2)

    # Monitor semplice finché non finiscono i RUNNING/READY
    while True:
        act = count_active(tasks)
        done = sum(1 for x in tasks if task_state(x) == 'COMPLETED')
        fail = sum(1 for x in tasks if task_state(x) in ('FAILED', 'CANCELLED'))
        print(f"[{datetime.now().strftime('%H:%M:%S')}] COMPLETED={done} ACTIVE={act} FAILED/CANCELLED={fail} / TOTAL={len(tasks)}")
        if act == 0:
            break
        time.sleep(poll_s)


def main():
    ee_init()

    # Carica e prepara i punti
    raw = ee.FeatureCollection(POINTS_ASSET)
    print("Punti totali nell’asset:", raw.size().getInfo())

    points = build_points_balanced(raw, ID_PROP, SPLITS, RAND_SEED)
    bounds = italy_bounds(points)

    # Prepara task: anno × bucket (univoci e bilanciati)
    tasks = []
    for year in YEARS:
        img = alphaearth_image(year, bounds)

        for b in range(SPLITS):
            fc = sample_bucket(img, points, year, b)
            if fc is None:
                print(f"Skip export {year} part{b+1}of{SPLITS} (bucket vuoto).")
                continue

            desc = f"{FILE_BASENAME}_{year}_part{b+1}of{SPLITS}"
            task = make_drive_export(fc, desc, DRIVE_FOLDER, desc)
            tasks.append(task)
            print("Preparato:", desc)

    print("Task pronti:", len(tasks))

    if ENABLE_EXPORTS:
        start_tasks_limited(tasks, MAX_CONCURRENT, POLL_INTERVAL_SEC)
        print("Fatto. Controlla i file su Google Drive ›", DRIVE_FOLDER)
    else:
        print("ENABLE_EXPORTS=False → dry run (nessun task avviato).")


if __name__ == "__main__":
    main()
