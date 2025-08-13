setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
load("Italy_master_reduced.RData")
datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES\\data\\2021\\"
c <- read.csv(paste0(datapath,"S2_S1_Italy_2021_Monthly.csv"))

summary(c)

c$NDVI_01 <- (c$B8_01 - c$B4_01) / (c$B8_01 + c$B4_01)
c$NDVI_02 <- (c$B8_02 - c$B4_02) / (c$B8_02 + c$B4_02)
c$NDVI_03 <- (c$B8_03 - c$B4_03) / (c$B8_03 + c$B4_03)
c$NDVI_04 <- (c$B8_04 - c$B4_04) / (c$B8_04 + c$B4_04)
c$NDVI_05 <- (c$B8_05 - c$B4_05) / (c$B8_05 + c$B4_05)
c$NDVI_06 <- (c$B8_06 - c$B4_06) / (c$B8_06 + c$B4_06)
c$NDVI_07 <- (c$B8_07 - c$B4_07) / (c$B8_07 + c$B4_07)
c$NDVI_08 <- (c$B8_08 - c$B4_08) / (c$B8_08 + c$B4_08)
c$NDVI_09 <- (c$B8_09 - c$B4_09) / (c$B8_09 + c$B4_09)
c$NDVI_10 <- (c$B8_10 - c$B4_10) / (c$B8_10 + c$B4_10)
c$NDVI_11 <- (c$B8_11 - c$B4_11) / (c$B8_11 + c$B4_11)
c$NDVI_12 <- (c$B8_12 - c$B4_12) / (c$B8_12 + c$B4_12)
c(grep("VV",names(c)))
d <- c[,c(c(grep("POINT_ID",names(c))),c(grep("VV",names(c))),c(grep("VH",names(c))),c(grep("NDVI",names(c))))]
d <- merge(m,d,by="POINT_ID")
write.table(d,"S2_S1_Italy_2021_trajectories_master.csv",quote=F,sep=",",row.names=F)
library(data.table)
#----------------
# For 2018 sample
# s <- fread("Survey_2018_cal_wgt.txt")
# s$LC1 <- substr(s$land_cover,1,1)
# sample <- merge(d,s[,c("POINT_ID","cal_wgt","point_area","LC1")],by="POINT_ID")
# sum(sample$cal_wgt/sample$point_area)
# sample <- sample[rep(seq_len(nrow(sample)), round(sample$cal_wgt/sample$point_area)),]
# round(table(sample$LC1)/nrow(sample),4)*100
# sample$cal_wgt <- sample$point_area <- NULL
#----------------
# For 2022 sample
s <- fread("Survey_2022_cal_wgt_2nd_phase.txt")
s$LC1 <- substr(s$SURVEY_LC1,1,1)
sample <- merge(d,s[,c("POINT_ID","wgt_2nd_phase","point_area","LC1")],by="POINT_ID")
sum(sample$wgt_2nd_phase/sample$point_area)
sample <- sample[rep(seq_len(nrow(sample)), round(sample$wgt_2nd_phase/sample$point_area)),]
round(table(sample$LC1)/nrow(sample),4)*100
sample$wgt_2nd_phase <- sample$point_area <- NULL


write.table(sample,"S2_S1_Italy_2022_trajectories_sample.csv",quote=F,sep=",",row.names=F)


