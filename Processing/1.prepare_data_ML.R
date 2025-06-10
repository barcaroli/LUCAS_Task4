#------------------------------------------------------------------------------
# 1.prepare_data_ML.R
#
# Script to prepare the inputs for the machine learning step
# Input: 1. dataset obtained by Google Earth Engine for a given year / country
#        2. master dataset (processed by Ballin) for a given country 
#        3. LUCAS survey data of a given year / country
# Output: 1. master dataset for a given country augmented with EE data
#         2. LUCAS survey data of a given year / country augmented with EE data 
#------------------------------------------------------------------------------
library(data.table)
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine")
# setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine")
#---------------------------------------------------------
# Input: 1. dataset obtained by Google Earth Engine for a given year / country
inp <- fread(".\\values\\S2_S1_GLCM_Italy_2018.csv")
inp$NDVI <- (inp$B8-inp$B4) / (inp$B8+inp$B4)
inp$NDWI <- (inp$B8-inp$B3) / (inp$B8+inp$B3)
inp$EVI <- (inp$B8-inp$B4) / 2.5 * (inp$B8 + 6*inp$B4 - 7.5*inp$B2 + 0.5)
inp$SAVI <- (inp$B8-inp$B4) / (inp$B8 + inp$B4 + 0.5)
inp$NDI45 <- (inp$B5-inp$B4) / (inp$B5+inp$B4)
inp$MCARI <- ((inp$B5 - inp$B4) - 0.2*(inp$B5 - inp$B3)) * (inp$B5-inp$B4)
inp$GNDVI <- (inp$B3-inp$B4) / (inp$B3+inp$B4)
# inp$MSAVI <- ?
inp$PSSR <- inp$B7 / inp$B4
inp$IRECI <- (inp$B7-inp$B4) / (inp$B5/inp$B6)

#---------------------------------------------------------
# Input 2. master dataset (processed by Ballin) for a given country
load("Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP.Rdata")
master <- Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP
classifica_aspect <- function(val) {
  if (is.na(val)) return("flat")
  if (val >= 315 || val < 45) {
    return("315-45")   # Nord
  } else if (val >= 45 && val < 135) {
    return("45-135")   # Est
  } else if (val >= 135 && val < 225) {
    return("135-225")  # Sud
  } else {
    return("225-315")  # Ovest
  }
}
# questa funzione serve per classificare la slope
classifica_slope <- function(val) {
  if (is.na(val)) return(NA)
  if (val >= 0 & val < 1) {
    return("0-1")   # piano o pendio do
  } else if (val >= 1 & val < 5) {
    return("1-5")   # salita
  } else if (val >= 5 & val < 25) {
    return("5-25")  # forte salita
  } else {
    return("more_25")  # pendio ripido
  }
}
# Applica la classificazione a ogni punto
master$exposure_class <- sapply(master$aspect_exposure, classifica_aspect)
master$exposure_class <-ifelse(master$aspect_exposure %in% c(90,270),"flat", master$exposure_class)
table(master$exposure_class)
master$slope_class <- sapply(master$aspect_slope, classifica_slope)
table(master$slope_class)
summary(master[,c("POINT_ID","ELEV_DEM","aspect_slope","aspect_exposure","slope_class","exposure_class")])
#---------------------------------------------------------
# Output: 1. master dataset for a given country augmented with EE data
out <- merge(inp,
             master[,c("POINT_ID","ELEV_DEM","aspect_slope","aspect_exposure","slope_class","exposure_class"),],
             by="POINT_ID")
write.table(out,"data_Italy_master_2018.csv",sep=",",quote=F,row.names=F)
#---------------------------------------------------------
# Input 3. LUCAS survey data of a given year / country
s2018 <- fread("Survey_2018_cal_wgt.txt")
s <- s2018[,c("POINT_ID","land_cover")]
s$target <- substr(s$land_cover,1,1)
s$land_cover <- NULL
#---------------------------------------------------------
# Output 3. LUCAS survey data of a given year / country augmented with EE data
out2 <- merge(out,s,by="POINT_ID")
write.table(out2,"data_Italy_sample_2018.csv",sep=",",quote=F,row.names=F)
