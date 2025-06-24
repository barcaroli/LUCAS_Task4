# ---- Load libraries ----
library(rgee)
# ee_clean_pyenv()
# ee_install(py_env = "rgee") # It is just necessary once!
library(sf)
library(dplyr)
library(readr)
library(lubridate)
library(openxlsx)

# ---- Set working directory ----
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine")

# ---- Initialize Earth Engine ----
ee_Authenticate(auth_mode='localhost')
ee_Initialize(email = "gbarcaroli@gmail.com", project = 'landcovercopernicus')

# ---- Parameters ----
country <- "Italy"
start_Date <- as.Date("2018-01-01")
end_Date <- as.Date("2018-12-31")
months_offset <- 3

# ---- Create date pairs for moving window ----
dates <- seq(start_Date, end_Date, by = paste(months_offset, "months"))
if (tail(dates, 1) < end_Date) dates <- c(dates, end_Date)
pairs <- data.frame(start = head(dates, -1), end = tail(dates, -1))

# ---- Read regions GeoJSON ----
regions_sf <- sf::st_read("Italy/italy_regions_gadm.geojson")
regions_fc <- sf_as_ee(regions_sf)

# ---- Field names ----
feature_name_1 <- "GID_1"
feature_name_2 <- "NAME_1"
feature_id     <- "NAME_1"

# ---- List of region codes and names ----
codes <- regions_sf[[feature_id]]
districts <- regions_sf[[feature_name_2]]

# ---- Helper: extract histogram for region and time window ----
geometry2res <- function(geom, startDate, endDate) {
  dw <- ee$ImageCollection('GOOGLE/DYNAMICWORLD/V1')$
    filterDate(as.character(startDate), as.character(endDate))$
    filterBounds(geom)
  classification <- dw$select('label')
  dwComposite <- classification$reduce(ee$Reducer$mode())
  dwComposite <- dwComposite$rename('classification')
  pixelCountStats <- dwComposite$reduceRegion(
    reducer = ee$Reducer$frequencyHistogram(),
    geometry = geom,
    scale = 10,
    maxPixels = 1e10
  )
  pixelCounts <- ee$Dictionary(pixelCountStats$get('classification'))
  if (is.null(pixelCounts$getInfo())) return(NULL)
  return(pixelCounts$getInfo())
}

# ---- Main function: get results for one region and one window ----
OneDisOutput_2level <- function(start, end, code) {
  filtered <- regions_fc$filter(ee$Filter$eq(feature_id, code))
  region_info <- regions_sf[regions_sf[[feature_id]] == code, ]
  geom <- sf_as_ee(region_info)
  res <- geometry2res(geom, start, end)
  if (is.null(res)) res <- list()
  res$code <- code
  res$region <- as.character(region_info[[feature_name_1]])
  res$startDate <- as.character(start)
  res$endDate <- as.character(end)
  if (is.null(res$null)) res$null <- 0
  return(res)
}

# ---- Logging and checkpoint ----
log_file <- file("log.txt", open = "a")
checkpoint_path <- "checkpoint_df.csv"
l <- list()

if (file.exists(checkpoint_path)) {
  l <- list(read_csv(checkpoint_path))
}

# ---- Main loop ----
i_temp <- 0

for (code in codes) {
  i_temp <- i_temp + 1
  cat(code, i_temp, "/", length(codes), "\n")
  for (k in seq_len(nrow(pairs))) {
    start_date <- pairs$start[k]
    end_date   <- pairs$end[k]
    res <- tryCatch({
      a <- OneDisOutput_2level(start_date, end_date, code)
      writeLines(sprintf("OK, %s, %s, %s,", code, start_date, end_date), log_file)
      as.data.frame(t(unlist(a)))
    }, error = function(e) {
      writeLines(sprintf("NO, %s, %s, %s,", code, start_date, end_date), log_file)
      cat(code, start_date, end_date, "NO\n")
      NULL
    })
    if (!is.null(res)) l[[length(l) + 1]] <- res
  }
  # checkpoint save
  if (length(l) > 0) {
    checkpoint_df <- do.call(bind_rows, l)
    write_csv(checkpoint_df, checkpoint_path)
  }
}

close(log_file)

# ---- Export final result ----
final_df <- do.call(bind_rows, l)
write.xlsx(final_df, paste0("Italy/", country, "_", end_Date, "_", months_offset, ".xlsx"))

# Optionally, delete checkpoint
file.remove(checkpoint_path)
