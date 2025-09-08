//---------------------------------------------
// JavaScript to download AlphaEarth embeddings
//---------------------------------------------
/**** PARAMETRI **************************************************************/
var POINTS_ASSET   = 'projects/landcovercopernicus/assets/Italy_master_points';
var ID_PROP        = 'POINT_ID';
var YEAR           = 2018;                          // anno da rifare
var PARTS          = [5, 8, 20];               // <-- più PART da rifare
var SPLITS         = 20;                            // come nei job originali
var RAND_SEED      = 12345;                         // stesso seed usato prima

// Se un part continua a fallire, spezzalo in sotto-blocchi:
var SUBSPLITS      = 1;                             // 1 = nessuno, 4 = in 4 sotto-part
var RAND_SEED_SUB  = 6789;                          // seed per il sotto-split

// Parametri robusti per il retry
var SAMPLING_SCALE = 20;                            // più alto = più resiliente
var TILE_SCALE     = 24;                            // aumenta se servisse (28–32)

/**** PUNTI + SPLIT **********************************************************/
var raw = ee.FeatureCollection(POINTS_ASSET);
var seeded = raw.randomColumn('rnd', RAND_SEED);

var points = seeded.map(function (f) {
  var coords = f.geometry().transform('EPSG:4326', 1).coordinates();
  var idNum  = ee.Number.parse(ee.String(f.get(ID_PROP)));
  var bucket = ee.Number(f.get('rnd')).multiply(SPLITS).floor();   // 0..SPLITS-1
  return ee.Feature(f.geometry(), {
    id:        ee.String(idNum.format('%.0f')),
    longitude: ee.Number(coords.get(0)),
    latitude:  ee.Number(coords.get(1)),
    bucket:    bucket
  });
});

/**** IMMAGINE ***************************************************************/
var DATASET = 'GOOGLE/SATELLITE_EMBEDDING/V1/ANNUAL';
var bounds  = points.geometry().dissolve().bounds();

var img = ee.ImageCollection(DATASET)
  .filterDate(ee.Date.fromYMD(YEAR,1,1), ee.Date.fromYMD(YEAR+1,1,1))
  .filterBounds(bounds)
  .mosaic();

/**** EXPORT MULTI-PART (con IIFE per evitare problemi di closure) **********/
var DRIVE_FOLDER  = 'EE_AlphaEarth';
var FILE_BASENAME = 'AlphaEarth_Italy';

PARTS.forEach(function (part) {
  (function(pi){                         // cattura il valore di "part"
    var bi = pi - 1;                     // indice 0-based del bucket
    var bucketFC = points.filter(ee.Filter.eq('bucket', bi));

    if (SUBSPLITS === 1) {
      // Export diretto del bucket scelto
      var fc = img.sampleRegions({
        collection: bucketFC,
        properties: ['id','latitude','longitude','bucket'],
        geometries: false,
        scale: SAMPLING_SCALE,
        tileScale: TILE_SCALE
      }).map(function(ft){ return ft.set('year', YEAR); });

      var desc = FILE_BASENAME + '_' + YEAR + '_part' + pi + 'of' + SPLITS + '_retry';
      Export.table.toDrive({
        collection: fc,
        description: desc,
        folder: DRIVE_FOLDER,
        fileNamePrefix: desc,
        fileFormat: 'CSV'
      });

    } else {
      // Sotto-split in SUBSPLITS blocchi con rnd2 ∈ [lo,hi)
      var bucketSeeded = bucketFC.randomColumn('rnd2', RAND_SEED_SUB + pi);
      for (var si = 0; si < SUBSPLITS; si++) {
        (function(sii){
          var lo = sii / SUBSPLITS;
          var hi = (sii + 1) / SUBSPLITS;
          var sub = bucketSeeded
            .filter(ee.Filter.gte('rnd2', lo))
            .filter(ee.Filter.lt('rnd2', hi));

          var fcSub = img.sampleRegions({
            collection: sub,
            properties: ['id','latitude','longitude','bucket'],
            geometries: false,
            scale: SAMPLING_SCALE,
            tileScale: TILE_SCALE
          }).map(function(ft){ return ft.set('year', YEAR); });

          var descSub = FILE_BASENAME + '_' + YEAR +
            '_part' + pi + 'of' + SPLITS +
            '_sub' + (sii+1) + 'of' + SUBSPLITS + '_retry';

          Export.table.toDrive({
            collection: fcSub,
            description: descSub,
            folder: DRIVE_FOLDER,
            fileNamePrefix: descSub,
            fileFormat: 'CSV'
          });
        })(si);
      }
    }
  })(part);
});
