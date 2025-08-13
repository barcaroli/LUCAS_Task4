setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES Germany")
datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES Germany\\data\\"
m <- read.csv(paste0(datapath,"master_only_coordinates.csv"))
m <- m[m$NUTS0_16 == "DE",]
m$longitude <- m$X_WGS84
m$latitude <- m$Y_WGS84
plot(m$longitude,m$latitude)
write.table(m[,c("POINT_ID","longitude","latitude")],paste0(datapath,"Germany_master_points.csv"),sep=",",quote=F,row.names=F)
