#-------------------------------
# Predictions on panel 2018-2022
#-------------------------------
library(caret)
library(randomForest)
library(data.table)
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/4.Predictions")
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
load(paste0(datapath,"rf_model_A_reduced.RData"))
load(paste0(datapath,"rf_model_B_reduced.RData"))
load(paste0(datapath,"rf_model_C_reduced.RData"))
load(paste0(datapath,"rf_model_D_reduced.RData"))
load(paste0(datapath,"rf_model_E_reduced.RData"))
load(paste0(datapath,"rf_model_F_reduced.RData"))
load(paste0(datapath,"rf_model_G_reduced.RData"))
load(paste0(datapath,"rf_model_H_reduced.RData"))
# 
s18 <-fread(paste0(datapath,"Italy_sample_2018_reduced.csv"))
s22 <-fread(paste0(datapath,"Italy_sample_2022_reduced.csv"))
s22$LC1 <- as.factor(s22$LC1)
table(s22$LC1)

panel22 <- s22[s22$POINT_ID %in% s18$POINT_ID,]
panel22$LC1 <- as.factor(panel22$LC1)
preProc_vals <- preProcess(
  x      = panel22,
  method = "medianImpute"
)
panel22 <- predict(preProc_vals, panel22)
anyNA(panel22)              # TRUE se ci sono NA
any(is.nan(as.matrix(panel22)))  # TRUE se ci sono NaN
any(is.infinite(as.matrix(panel22)))  # TRUE se ci sono Inf o -Inf

panel22$probs_A <- predict(rf_model_A, newdata=panel22, type = "prob")[, "1"]
panel22$probs_B <- predict(rf_model_B, newdata=panel22, type = "prob")[, "1"]
panel22$probs_C <- predict(rf_model_C, newdata=panel22, type = "prob")[, "1"]
panel22$probs_D <- predict(rf_model_D, newdata=panel22, type = "prob")[, "1"]
panel22$probs_E <- predict(rf_model_E, newdata=panel22, type = "prob")[, "1"]
panel22$probs_F <- predict(rf_model_F, newdata=panel22, type = "prob")[, "1"]
panel22$probs_G <- predict(rf_model_G, newdata=panel22, type = "prob")[, "1"]
panel22$probs_H <- predict(rf_model_H, newdata=panel22, type = "prob")[, "1"]

panel22_2 <- panel22[,c("POINT_ID","LC1","probs_A","probs_B","probs_C","probs_D",
                   "probs_E","probs_F","probs_G","probs_H")]

# set.seed(1234)
# rf_model_panel <- randomForest(LC1 ~ ., 
#                                  data=panel2, 
#                                  importance=TRUE,
#                                  # mtry=15,
#                                  # nodesize=5,
#                                  ntree=500,
#                                  do.trace=TRUE)
load(paste0(datapath,"ensemble_model_reduced.RData"))

rf_predictions <- predict(rf_model,panel22_2)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, panel22_2$LC1)
cm
t <- addmargins(cm$table)
t 

panel22_2 <- merge(panel22_2,s18[,c("POINT_ID","LC1")],by="POINT_ID")
colnames(panel22_2)[c(2,11)] <- c("LC1","LC1_18")
panel22_2$LC1_18 <- as.factor(panel22_2$LC1_18)

addmargins(xtabs(~LC1+LC1_18,data=panel22_2))

cm2 <- confusionMatrix(rf_predictions, panel22_2$LC1_18)
cm2
t2 <- addmargins(cm2$table)
t2

panel3 <- panel22_2[panel22_2$LC1 == panel22_2$LC1_18,]
panel3$LC_predicted <- predict(rf_model,panel3)
# Evaluate models on test data
cm <- confusionMatrix(panel3$LC_predicted, panel3$LC1)
cm
t <- addmargins(cm$table)
t

panel18 <- s18[s18$POINT_ID %in% s22$POINT_ID,]
panel18$LC1 <- as.factor(panel18$LC1)

panel <- merge(panel18,panel22,by="POINT_ID")

c <- cor(x=panel[,c(2:32)],y=panel[,c(34:64)])
d <- as.data.frame(list(vars = names(panel)[2:32],
                        correlation = diag(c)))
d
