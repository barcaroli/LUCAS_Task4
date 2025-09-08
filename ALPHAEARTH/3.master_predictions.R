#--------------------------------------------------------------------------------
# Script to predict "land cover" using EE data for master data in available years
# Input: 1. rf or xgb model for the different land cover values
#        2. masters augmented with EE data in the different years
#        3. thresholds from "2.predict_test.R"
# Output: 1. master with the 2018-2019-2020-2021-2022 predictions
# N.B. : CHOOSE THE MODEL (RF OR XGBOOST)
#------------------------------------------------------------------------------
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow")
datapath <- "D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow/data/"
#--------------------------------
# CHOOSE RF
# mod <- readRDS(paste0(datapath,"rf_learner.rds"))
# out <- paste0(datapath,"master_predicted_rf.csv")
# th_mlr3 <- readRDS(paste0(datapath,"th_mlr3_rf.rds"))
# th_mlr3

#--------------------------------
# CHOOSE XGB
mod <- readRDS(paste0(datapath,"xgb_learner.rds"))
out <- paste0(datapath,"master_predicted_xgb.csv")
th_mlr3 <- readRDS(paste0(datapath,"th_mlr3_xgb.rds"))
th_mlr3


library(corrplot)
library(data.table)

suppressPackageStartupMessages({
  library(data.table)
  library(mlr3verse)   # per predict_newdata
})


#-------------------------------------------------------------------------------------
# 2018
m <- fread(paste0(datapath,"AlphaEarth_Italy_2018_master.csv"))
c <- cor(m[,c(5:68)])
corrplot(c, method = "color", type = "upper", tl.cex = 0.8)

pred <- mod$predict_newdata(m)   # new_df = solo feature A00..A63 (+eventuali extra)
pred$set_threshold(th_mlr3)
pred2018 <- pred$response
mpred <- as.data.frame(list(POINT_ID = m$POINT_ID,
                            pred2018 = pred2018))
addmargins(xtabs(~pred2018,data=mpred))

#-------------------------------------------------------------------------------------
# 2019
m <- fread(paste0(datapath,"AlphaEarth_Italy_2019_master.csv"))
pred <- mod$predict_newdata(m)   # new_df = solo feature A00..A63 (+eventuali extra)
pred$set_threshold(th_mlr3)
mpred$pred2019 <- pred$response

addmargins(xtabs(~pred2018+pred2019,data=mpred))

#-------------------------------------------------------------------------------------
# 2020
m <- fread(paste0(datapath,"AlphaEarth_Italy_2020_master.csv"))
pred <- mod$predict_newdata(m)   # new_df = solo feature A00..A63 (+eventuali extra)
pred$set_threshold(th_mlr3)
mpred$pred2020 <- pred$response

addmargins(xtabs(~pred2019+pred2020,data=mpred))

#-------------------------------------------------------------------------------------
# 2021
m <- fread(paste0(datapath,"AlphaEarth_Italy_2021_master.csv"))
pred <- mod$predict_newdata(m)   # new_df = solo feature A00..A63 (+eventuali extra)
pred$set_threshold(th_mlr3)
mpred$pred2021 <- pred$response

addmargins(xtabs(~pred2020+pred2021,data=mpred))

#-------------------------------------------------------------------------------------
# 2022
m <- fread(paste0(datapath,"AlphaEarth_Italy_2022_master.csv"))
pred <- mod$predict_newdata(m)   # new_df = solo feature A00..A63 (+eventuali extra)
pred$set_threshold(th_mlr3)
mpred$pred2022 <- pred$response

write.table(mpred,file=out,sep=",",quote=F,row.names=F)
