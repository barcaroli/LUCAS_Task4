library(data.table)
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow")
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\ALPHAEARTH\\workflow\\data\\"
points <- read.csv(paste0(datapath,"Italy_master_points.csv"))
s18 <- fread(paste0(datapath,"Survey_2018_cal_wgt.txt"))
s18$LC1 <- substr(s18$land_cover,1,1)

m <- read.csv(paste0(datapath,"master_predicted_rf.csv"))
ms <- merge(m,s18[,c("POINT_ID","LC1")])
ms <- merge(ms,points,by="POINT_ID")
write.table(ms,paste0(datapath,"sample_predicted_rf.csv"),sep=",",row.names=F,quote=F)

m <- read.csv(paste0(datapath,"master_predicted_xgb.csv"))
ms <- merge(m,s18[,c("POINT_ID","LC1")])
ms <- merge(ms,points,by="POINT_ID")
write.table(ms,paste0(datapath,"sample_predicted_xgb.csv"),sep=",",row.names=F,quote=F)

head(ms)
