setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
library(caret)
library(randomForest)
library(data.table)
datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/models\\"
#-------------------------------------------------------------------------------------
# Load models
load(paste0(datapath,"rf_model_A.RData"))
load(paste0(datapath,"rf_model_B.RData"))
load(paste0(datapath,"rf_model_C.RData"))
load(paste0(datapath,"rf_model_D.RData"))
load(paste0(datapath,"rf_model_E.RData"))
load(paste0(datapath,"rf_model_F.RData"))
load(paste0(datapath,"rf_model_G.RData"))
load(paste0(datapath,"rf_model_H.RData"))
load(paste0(datapath,"ensemble_model.RData"))

#-------------------------------------------------------------------------------------
# 2018
m <- fread("S2_S1_Italy_2018_trajectories_master.csv")
#Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)
# Predict A
m$probs_A <- predict(rf_model_A, newdata=m, type = "prob")[, "1"]
mean(m$probs_A)
# Predict B
m$probs_B <- predict(rf_model_B, newdata=m, type = "prob")[, "1"]
mean(m$probs_B)
# Predict C
m$probs_C <- predict(rf_model_C, newdata=m, type = "prob")[, "1"]
mean(m$probs_C)
# Predict D
m$probs_D <- predict(rf_model_D, newdata=m, type = "prob")[, "1"]
mean(m$probs_D)
# Predict E
m$probs_E <- predict(rf_model_E, newdata=m, type = "prob")[, "1"]
mean(m$probs_E)
# Predict F
m$probs_F <- predict(rf_model_F, newdata=m, type = "prob")[, "1"]
mean(m$probs_F)
# Predict G
m$probs_G <- predict(rf_model_G, newdata=m, type = "prob")[, "1"] 
mean(m$probs_G)
# Predict H
m$probs_H <- predict(rf_model_H, newdata=m, type = "prob")[, "1"] 
mean(m$probs_H)

mean(m$probs_A)+mean(m$probs_B)+mean(m$probs_C)+mean(m$probs_D)+mean(m$probs_E)+mean(m$probs_F)+mean(m$probs_G)+mean(m$probs_H)

summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])
for (k in LETTERS[1:8]) {
  eval(parse(text=paste0("m$probs_",k," <- m$probs_",k," * (1/max(m$probs_",k,"))")))
}
summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])

pred2018 <- predict(rf_model,m)

mpred <- as.data.frame(list(POINT_ID = m$POINT_ID,
                            pred2018 = pred2018))
xtabs(~pred2018,data=mpred)

#-------------------------------------------------------------------------------------
# 2019
m <- fread("S2_S1_Italy_2019_trajectories_master.csv")
#Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)
# Predict A
m$probs_A <- predict(rf_model_A, newdata=m, type = "prob")[, "1"]
mean(m$probs_A)
# Predict B
m$probs_B <- predict(rf_model_B, newdata=m, type = "prob")[, "1"]
mean(m$probs_B)
# Predict C
m$probs_C <- predict(rf_model_C, newdata=m, type = "prob")[, "1"]
mean(m$probs_C)
# Predict D
m$probs_D <- predict(rf_model_D, newdata=m, type = "prob")[, "1"]
mean(m$probs_D)
# Predict E
m$probs_E <- predict(rf_model_E, newdata=m, type = "prob")[, "1"]
mean(m$probs_E)
# Predict F
m$probs_F <- predict(rf_model_F, newdata=m, type = "prob")[, "1"]
mean(m$probs_F)
# Predict G
m$probs_G <- predict(rf_model_G, newdata=m, type = "prob")[, "1"] 
mean(m$probs_G)
# Predict H
m$probs_H <- predict(rf_model_H, newdata=m, type = "prob")[, "1"] 
mean(m$probs_H)

mean(m$probs_A)+mean(m$probs_B)+mean(m$probs_C)+mean(m$probs_D)+mean(m$probs_E)+mean(m$probs_F)+mean(m$probs_G)+mean(m$probs_H)

summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])
for (k in LETTERS[1:8]) {
  eval(parse(text=paste0("m$probs_",k," <- m$probs_",k," * (1/max(m$probs_",k,"))")))
}
summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])


mpred$pred2019 <- predict(rf_model,m)

addmargins(xtabs(~pred2018+pred2019,data=mpred))

#-------------------------------------------------------------------------------------
# 2020
m <- fread("S2_S1_Italy_2020_trajectories_master.csv")
#Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)
# Predict A
m$probs_A <- predict(rf_model_A, newdata=m, type = "prob")[, "1"]
mean(m$probs_A)
# Predict B
m$probs_B <- predict(rf_model_B, newdata=m, type = "prob")[, "1"]
mean(m$probs_B)
# Predict C
m$probs_C <- predict(rf_model_C, newdata=m, type = "prob")[, "1"]
mean(m$probs_C)
# Predict D
m$probs_D <- predict(rf_model_D, newdata=m, type = "prob")[, "1"]
mean(m$probs_D)
# Predict E
m$probs_E <- predict(rf_model_E, newdata=m, type = "prob")[, "1"]
mean(m$probs_E)
# Predict F
m$probs_F <- predict(rf_model_F, newdata=m, type = "prob")[, "1"]
mean(m$probs_F)
# Predict G
m$probs_G <- predict(rf_model_G, newdata=m, type = "prob")[, "1"] 
mean(m$probs_G)
# Predict H
m$probs_H <- predict(rf_model_H, newdata=m, type = "prob")[, "1"] 
mean(m$probs_H)

mean(m$probs_A)+mean(m$probs_B)+mean(m$probs_C)+mean(m$probs_D)+mean(m$probs_E)+mean(m$probs_F)+mean(m$probs_G)+mean(m$probs_H)

summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])
for (k in LETTERS[1:8]) {
  eval(parse(text=paste0("m$probs_",k," <- m$probs_",k," * (1/max(m$probs_",k,"))")))
}
summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])

mpred$pred2020 <- predict(rf_model,m)

addmargins(xtabs(~pred2018+pred2020,data=mpred))

#-------------------------------------------------------------------------------------
# 2021
m <- fread("S2_S1_Italy_2021_trajectories_master.csv")
#Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)
# Predict A
m$probs_A <- predict(rf_model_A, newdata=m, type = "prob")[, "1"]
mean(m$probs_A)
# Predict B
m$probs_B <- predict(rf_model_B, newdata=m, type = "prob")[, "1"]
mean(m$probs_B)
# Predict C
m$probs_C <- predict(rf_model_C, newdata=m, type = "prob")[, "1"]
mean(m$probs_C)
# Predict D
m$probs_D <- predict(rf_model_D, newdata=m, type = "prob")[, "1"]
mean(m$probs_D)
# Predict E
m$probs_E <- predict(rf_model_E, newdata=m, type = "prob")[, "1"]
mean(m$probs_E)
# Predict F
m$probs_F <- predict(rf_model_F, newdata=m, type = "prob")[, "1"]
mean(m$probs_F)
# Predict G
m$probs_G <- predict(rf_model_G, newdata=m, type = "prob")[, "1"] 
mean(m$probs_G)
# Predict H
m$probs_H <- predict(rf_model_H, newdata=m, type = "prob")[, "1"] 
mean(m$probs_H)

mean(m$probs_A)+mean(m$probs_B)+mean(m$probs_C)+mean(m$probs_D)+mean(m$probs_E)+mean(m$probs_F)+mean(m$probs_G)+mean(m$probs_H)

summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])
for (k in LETTERS[1:8]) {
  eval(parse(text=paste0("m$probs_",k," <- m$probs_",k," * (1/max(m$probs_",k,"))")))
}
summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])

mpred$pred2021 <- predict(rf_model,m)

addmargins(xtabs(~pred2018+pred2021,data=mpred))

#-------------------------------------------------------------------------------------
# 2022
m <- fread("S2_S1_Italy_2022_trajectories_master.csv")
#Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)
# Predict A
m$probs_A <- predict(rf_model_A, newdata=m, type = "prob")[, "1"]
mean(m$probs_A)
# Predict B
m$probs_B <- predict(rf_model_B, newdata=m, type = "prob")[, "1"]
mean(m$probs_B)
# Predict C
m$probs_C <- predict(rf_model_C, newdata=m, type = "prob")[, "1"]
mean(m$probs_C)
# Predict D
m$probs_D <- predict(rf_model_D, newdata=m, type = "prob")[, "1"]
mean(m$probs_D)
# Predict E
m$probs_E <- predict(rf_model_E, newdata=m, type = "prob")[, "1"]
mean(m$probs_E)
# Predict F
m$probs_F <- predict(rf_model_F, newdata=m, type = "prob")[, "1"]
mean(m$probs_F)
# Predict G
m$probs_G <- predict(rf_model_G, newdata=m, type = "prob")[, "1"] 
mean(m$probs_G)
# Predict H
m$probs_H <- predict(rf_model_H, newdata=m, type = "prob")[, "1"] 
mean(m$probs_H)

mean(m$probs_A)+mean(m$probs_B)+mean(m$probs_C)+mean(m$probs_D)+mean(m$probs_E)+mean(m$probs_F)+mean(m$probs_G)+mean(m$probs_H)

summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])
for (k in LETTERS[1:8]) {
  eval(parse(text=paste0("m$probs_",k," <- m$probs_",k," * (1/max(m$probs_",k,"))")))
}
summary(m[,c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")])

mpred$pred2022 <- predict(rf_model,m)

addmargins(xtabs(~pred2018+pred2022,data=mpred))

write.table(mpred,file="master_predicted.csv",sep=",",quote=F,row.names=F)
