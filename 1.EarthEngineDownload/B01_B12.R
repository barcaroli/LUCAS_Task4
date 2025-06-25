setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/Data")
# remotes::install_github("r-spatial/rgee")
library(rgee)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(reticulate)

# ee_install_set_pyenv(
#   py_path = "C:\\Users\\UTENTE\\AppData\\Local\\Programs\\Python\\Python311",
#   py_env  = "rgee")
# ee_mod <- import("ee")
# versione_ee <- ee_mod$`__version__`
# cat("Versione Earth Engine Python API:", versione_ee, "\n")

# rgee::ee_check()
ee_clean_user_credentials()
# ee_Authenticate(auth_mode = "localhost")
# ee_Initialize(project = "landcovercopernicus", drive = FALSE, gcs = FALSE)
# Modalità “notebook”
ee_Initialize(project = "landcovercopernicus")
# Modalità “gcloud”, se hai già login tramite gcloud CLI
ee_Initialize(project = "landcovercopernicus", auth_mode = "gcloud")
# Parametri
input_csv <- "sample_points.csv"         # deve avere: POINT_ID, longitude, latitude
start_date <- "2018-04-01"
end_date   <- "2018-07-31"
batch_size <- 1000
output_csv <- "s2_bands_points.csv"

bands_needed <- c('B1', 'B2', 'B3', 'B4', 'B5', 'B6', 'B7', 'B8', 'B8A', 'B9', 'B11', 'B12')

# Leggi i punti e calcola la bounding box
points <- read_csv(input_csv, show_col_types = FALSE)
bbox <- st_bbox(
  st_as_sf(points, coords = c("longitude", "latitude"), crs = 4326)
)

ee_bbox <- ee$Geometry$Rectangle(
  coords = c(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"])
)

# Prendi le immagini Sentinel-2 SR Harmonized nell'intervallo
collection <- ee$ImageCollection("COPERNICUS/S2_SR_HARMONIZED")$
  filterDate(start_date, end_date)$
  filterBounds(ee_bbox)$
  filter(ee$Filter$lt("CLOUDY_PIXEL_PERCENTAGE", 20))

# Crea il mosaico e seleziona solo le bande richieste
image <- collection$mosaic()$select(bands_needed)

# Funzione per processare un batch
process_batch <- function(batch_df) {
  # Crea una FeatureCollection dei punti con POINT_ID
  features <- map2(
    batch_df$longitude, batch_df$latitude,
    ~ ee$Feature(ee$Geometry$Point(c(.x, .y)), list(POINT_ID = .x))
  )
  fc <- ee$FeatureCollection(features)
  # Sample
  sampled <- tryCatch({
    image$sampleRegions(
      collection = fc,
      scale = 10,
      geometries = FALSE
    )$getInfo()
  }, error = function(e) NULL)
  if (is.null(sampled)) return(tibble())
  rows <- map_dfr(sampled$features, function(f) {
    as_tibble(f$properties)
  })
  return(rows)
}

# Processing in batch
n <- nrow(points)
batches <- split(points, ceiling(seq_along(1:n) / batch_size))

all_results <- list()
for (i in seq_along(batches)) {
  cat(sprintf("Processing batch %d of %d ...\n", i, length(batches)))
  batch_result <- process_batch(batches[[i]])
  all_results[[i]] <- batch_result
  # Salvataggio parziale
  bind_rows(all_results) %>% write_csv(output_csv, na = "")
  Sys.sleep(1)
}
final_df <- bind_rows(all_results)
write_csv(final_df, output_csv, na = "")
cat(sprintf("Completato! Output: %s (%d record)\n", output_csv, nrow(final_df)))
