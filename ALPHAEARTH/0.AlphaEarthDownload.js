//---------------------------------------------
// JavaScript to download AlphaEarth embeddings
//---------------------------------------------
// -------------------------------
// AlphaEarth extraction batching
// -------------------------------

/******************** PARAMETRI UTENTE ***************************************/
var POINTS_ASSET    = 'projects/landcovercopernicus/assets/Italy_master_points';
var ID_PROP         = 'POINT_ID';                  // campo ID (intero)
var YEARS           = [2018];

var DRIVE_FOLDER    = 'EE_AlphaEarth';             // cartella su Google Drive
var FILE_BASENAME   = 'AlphaEarth_Italy';          // prefisso file CSV

// Prestazioni
var SAMPLING_SCALE  = 10;                          // m (puoi mettere 20 o 30 se ti basta più grosso)
var TILE_SCALE      = 16;                          // alza se hai abort per memoria (es. 12–20)

// Split automatico (numero di blocchi per anno)
var SPLITS          = 20;                          // 15 blocchi ≈ 5k punti l’uno

/******************** CARICAMENTO PUNTI **************************************/
var rawPoints = ee.FeatureCollection(POINTS_ASSET);
print('Punti totali:', rawPoints.size());

// Arricchisci: id, lat/lon (WGS84) e bucket di split
var points = rawPoints.map(function (f) {
  var coords = f.geometry().transform('EPSG:4326', 1).coordinates();
  var lon = ee.Number(coords.get(0));
  var lat = ee.Number(coords.get(1));

  // ID numerico → bucket deterministico
  var pidNum  = ee.Number.parse(ee.String(f.get(ID_PROP)));
  var bucket  = pidNum.mod(SPLITS);                      // 0..SPLITS-1
  var idValue = ee.String(ee.Number(pidNum).format('%.0f'));

  return ee.Feature(f.geometry(), {
    id: idValue,
    longitude: lon,
    latitude:  lat,
    bucket:    bucket
  });
});

Map.centerObject(points.limit(1), 7);
Map.addLayer(points.limit(2000), {color: 'yellow'}, 'Punti (anteprima)');

/******************** DATASET & BOUNDS ***************************************/
var DATASET = 'GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL';
var italyBounds = points.geometry().dissolve().bounds();

/******************** FUNZIONI ***********************************************/
// Mosaica tutti i tasselli dell'anno sopra l’Italia
function getAlphaEarthImage(year) {
  var byDate = ee.ImageCollection(DATASET)
    .filterDate(ee.Date.fromYMD(year,1,1), ee.Date.fromYMD(year+1,1,1))
    .filterBounds(italyBounds);
  print('Anno', year, '- tasselli (Italia):', byDate.size());
  return byDate.mosaic();
}

// Campiona un sottoinsieme (bucket) di punti per un dato anno
function sampleYearBucket(year, bucketIdx) {
  var img = getAlphaEarthImage(year);
  var ptsSubset = points.filter(ee.Filter.eq('bucket', bucketIdx));

  // diagnostica leggera
  print('Anno', year, 'Bucket', bucketIdx, '- punti:', ptsSubset.size());

  var sampled = img.sampleRegions({
    collection: ptsSubset,
    properties: ['id','latitude','longitude','bucket'],
    geometries: false,
    scale: SAMPLING_SCALE,
    tileScale: TILE_SCALE
  }).map(function(ft){ return ft.set('year', year); });

  // stampa solo size: economico
  print('Anno', year, 'Bucket', bucketIdx, '- righe campionate:', sampled.size());
  return sampled;
}

/******************** EXPORT: PER ANNO × PER BUCKET **************************/
YEARS.forEach(function (year) {
  for (var b = 0; b < SPLITS; b++) {
    var fc = sampleYearBucket(year, b);

    Export.table.toDrive({
      collection: fc,
      description: FILE_BASENAME + '_' + year + '_part' + (b+1) + 'of' + SPLITS,
      folder: DRIVE_FOLDER,
      fileNamePrefix: FILE_BASENAME + '_' + year + '_part' + (b+1) + 'of' + SPLITS,
      fileFormat: 'CSV'
      // Colonne: id, latitude, longitude, bucket, year, A01..Axx
    });
  }
});
