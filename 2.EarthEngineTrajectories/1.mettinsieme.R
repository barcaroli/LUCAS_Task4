# a <- read.csv(".\\2018\\S2_S1_Italy_2018_01.csv")
# b <- read.csv("S2_S1_Italy_2018_Monthly.csv")
# c <- merge(a,b,by="POINT_ID",all.x=TRUE,all.y=TRUE)
# names(c)
# names(c)[order(names(c))]
# write.table(c,"S2_S1_Italy_2018_Monthly_complete.csv",quote=F,sep=",",row.names=F)
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
c <- read.csv(paste0(datapath,"S2_S1_Italy_2022_Monthly.csv"))

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
# d <- c[,c(c(grep("POINT_ID",names(c))),c(grep("VV",names(c))),c(grep("VH",names(c))),c(grep("NDVI",names(c))))]
write.table(c,paste0(datapath,"S2_S1_Italy_2022_trajectories_master.csv"),quote=F,sep=",",row.names=F)
library(data.table)
s <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s$LC1 <- substr(s$SURVEY_LC1,1,1)
s <- s[,c("POINT_ID","LC1")]
d <- merge(c,s,by="POINT_ID")
write.table(d,paste0(datapath,"S2_S1_Italy_2022_trajectories_sample.csv"),quote=F,sep=",",row.names=F)
