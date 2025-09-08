#--------------------------------------------------------------------------------
# Script to produce land cover estimates from 2018 to 2022 based predicted values 
# Input: 1. 2018 LUCAS sample
#        2. 2022 LUCAS sample
#        2. master with predictions from 2018 to 2022
#           (2 options: plain or treated)
# Output: 1. estimates by year and type
# N.B. : CHOOSE THE RF OR XGB PREDICTIONS
#------------------------------------------------------------------------------

setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow")
datapath <- "D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow/data/"

#-------------------------------------
# Master predictions with RF
# mpred <- read.csv(paste0(datapath,"master_predicted_rf.csv"))
# out <-"estimates_rf.csv"
#-------------------------------------
# Master predictions with XGB
mpred <- read.csv(paste0(datapath,"master_predicted_xgb.csv"))
out <- "estimates_xgb.csv"


library(data.table)
# Sample 2018
s <- fread(paste0(datapath,"Survey_2018_cal_wgt.txt"))
s$LC2018 <- substr(s$land_cover,1,1)
s <- s[s$NUTS0_16=="IT",]

s2 <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s2$LC2022 <- substr(s2$SURVEY_LC1,1,1)
s2 <- s2[s2$POINT_NUTS0=="IT",]

# s <- merge(s,s2[,c("POINT_ID","LC2022","wgt_2nd_phase")],by="POINT_ID")

s <- merge(s,mpred)

obs2018 <- round(prop.table(xtabs(cal_wgt~LC2018,data=s)),4)
obs2022 <- round(prop.table(xtabs(wgt_2nd_phase~LC2022,data=s2)),4)
sample_pred2018 <- round(prop.table(xtabs(cal_wgt~pred2018,data=s)),4)
sample_pred2019 <- round(prop.table(xtabs(cal_wgt~pred2019,data=s)),4)
sample_pred2020 <- round(prop.table(xtabs(cal_wgt~pred2020,data=s)),4)
sample_pred2021 <- round(prop.table(xtabs(cal_wgt~pred2021,data=s)),4)
sample_pred2022 <- round(prop.table(xtabs(cal_wgt~pred2022,data=s)),4)
master_pred2018 <- round(prop.table(xtabs(~pred2018,data=mpred)),4)
master_pred2019 <- round(prop.table(xtabs(~pred2019,data=mpred)),4)
master_pred2020 <- round(prop.table(xtabs(~pred2020,data=mpred)),4)
master_pred2021 <- round(prop.table(xtabs(~pred2021,data=mpred)),4)
master_pred2022 <- round(prop.table(xtabs(~pred2022,data=mpred)),4)

estimates <- as.data.frame(rbind(
  obs2018,
  obs2022,
  sample_pred2018,
  sample_pred2019,
  sample_pred2020,
  sample_pred2021,
  sample_pred2022,
  master_pred2018,
  master_pred2019,
  master_pred2020,
  master_pred2021,
  master_pred2022
))
estimates$estimate <- row.names(estimates)
estimates
write.table(estimates,file=out,sep=",",quote=F,row.names=F)


