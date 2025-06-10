# Con questo programma provo a calcolare alcune statistiche utili a descrivere 
# i pattern attorno a ogni pixel
# la metodologia suggerita dal paper dei croati si basa sul calcolo della matrice 
# Grey-Level-Co-occurence-Matrix
library(glcm)
library(raster)
library(sf)

# leggo l'immagine di solito un .tif

raster<-lettura dell immagine ad esempio con comando raster
master<- leggo il master (controllare che abbia struttura sf)

#  se non ha struttura sf allora
master<-st_as_sf(master, coord=c(X_LAEA,Y_LAEA), crs=)
plot(master)

#controllare se master e raster hanno lo stesso crs
st_crs(raster)==st_crs(master)
# se FALSE
master<-st_transform(master, crs(raster))

matr_glcm<-glcm(raster, shift=list(c(0,1), c(1,1), c(1,0), c(1,-1)), 
        statistics=c('mean',"variance", 'homogeneity', 'contrast', 'dissimilarity', 'entropy'))

# salvo il risultato
writeRaster(matr_glcm, file="matr_glcm.Tiff")
save(matr_glcm,file = "matr_glcm.Rdata")

#inserisco i valori sul master (qualche dubbio che debba esserci l'opzione fun=mean)
# eventualmente usiamo il secondo comando
master<-st_as_sf(extract(matr_glcm,master,fun=mean,sp=T))
master<-st_as_sf(extract(matr_glcm,master,sp=T))
master_sf<-st_as_sf(master)
#salvo
save(master_sf, file="master_sf.Rdata")
