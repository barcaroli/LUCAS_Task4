setwd("D:/Google Drive/LUCAS Copernicus/CDSE/Italy")
library(data.table)
s2018 <- fread("Survey_2018_cal_wgt.txt")
m <- fread("master_only_coordinates.csv")
italy <- merge(s2018[s2018$NUTS0_16=="IT",],m[,c(1,5:8)])
italy <- italy[,c(1,22:25)]
plot(italy$X_WGS84,italy$Y_WGS84)
italy$longitude <- italy$X_WGS84
italy$latitude <- italy$Y_WGS84
plot(italy$longitude,italy$latitude)
italy <- italy[,c(1,6,7)]
write.table(italy,"italy_points.csv",sep=",",quote=F,row.names=F)
