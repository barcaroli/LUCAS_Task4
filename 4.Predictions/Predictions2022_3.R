library(caret)
library(randomForest)
library(data.table)
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/4.Predictions")
#-------------------------------------------------------------------------------------
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
# Load models
load(paste0(datapath,"rf_model_A.RData"))
load(paste0(datapath,"rf_model_B.RData"))
load(paste0(datapath,"rf_model_C.RData"))
load(paste0(datapath,"rf_model_D.RData"))
load(paste0(datapath,"rf_model_E.RData"))
load(paste0(datapath,"rf_model_F.RData"))
load(paste0(datapath,"rf_model_G.RData"))
load(paste0(datapath,"rf_model_H.RData"))
#-------------------------------------------------------------------------------------
# Read master
m <- fread(paste0(datapath,"Italy_master_2022.csv"))
summary(m)
# Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)

# anyNA(m)              # TRUE se ci sono NA
# any(is.nan(as.matrix(m)))  # TRUE se ci sono NaN
#any(is.infinite(as.matrix(m)))  # TRUE se ci sono Inf o -Inf
# m[is.infinite(as.matrix(m))]

# m$PSSR[is.infinite(m$PSSR)] <- median(m$PSSR)
# m$IRECI[is.infinite(m$IRECI)] <- median(m$IRECI)

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


load(paste0(datapath,"ensemble_model.RData"))

correction_factors <- read.csv(paste0(datapath,"correction_factors.csv"))


m$LC_predicted <- predict(rf_model,m)

table(m$LC_predicted,useNA = "ifany")/nrow(m)

t1 <- as.data.frame(round(prop.table(xtabs(~LC_predicted,data=m)),4)*100)
t1$Estimates <- t1$Freq * correction_factors$correction
t1

# write.table(mpred,paste0(datapath,"Italy_master_2022_with_predictions_3.csv"),sep=",",quote=F,row.names=F)
library(data.table())
s2 <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s2 <- s2[s2$POINT_NUTS0 == "IT",]
s2 <- merge(s2,m[,c("POINT_ID","LC_predicted")],by="POINT_ID")
s2$LC1 <- substr(s2$SURVEY_LC1,1,1)
t2 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC_predicted,data=s2)),4)*100)
t2

t3 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC1,data=s2)),4)*100)
t3

# t1$LC_predicted <- factor(t1$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t1 <- t1[order(t1$LC_predicted), ]
# t2$LC_predicted <- factor(t2$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t2 <- t2[order(t2$LC_predicted), ]
mat <- rbind(MasterPredictedCorr = t1$Estimates, SurveyPredicted = t3$Freq, SurveyObserved = t2$Freq)
mat

# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange","darkgreen"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
        ylab = "Percentage",
        main = "Observed vs predicted 2022")

legend("topright", legend = c("Master Predicted", "Survey Predicted", "Survey Observed"), 
       fill = c("skyblue", "orange", "darkgreen"))

mat <- as.data.frame(mat)

colnames(mat) <-c("A", "B", "C", "D", "E", "F", "G", "H")
mat$reference <- c("Master Predicted 2022", "Survey Predicted 2022", "Survey Observed 2022")
write.table(mat,paste0(datapath,"Italy_estimates_predictions_2022_3.csv"),sep=",",quote=F,row.names=F)
