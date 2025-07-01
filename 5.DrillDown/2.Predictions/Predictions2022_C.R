library(caret)
library(randomForest)
library(data.table)
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/4.Predictions")
#-------------------------------------------------------------------------------------
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
# Load models
load(paste0(datapath,"rf_model_C1.RData"))
load(paste0(datapath,"rf_model_C2.RData"))
load(paste0(datapath,"rf_model_C3.RData"))
#-------------------------------------------------------------------------------------
# Read master
m <- fread(paste0(datapath,"Italy_master_2022_with_LC1_predictions.csv"))
m <- m[,-c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")]
table(m$LC_predicted)
m <- m[m$LC_predicted == "C",]
m$LC_predicted <- factor(m$LC_predicted)
summary(m)
# Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)

# anyNA(m)              # TRUE se ci sono NA
# any(is.nan(as.matrix(m)))  # TRUE se ci sono NaN
# any(is.infinite(as.matrix(m)))  # TRUE se ci sono Inf o -Inf
# m[is.infinite(as.matrix(m))]

# m$PSSR[is.infinite(m$PSSR)] <- median(m$PSSR)
# m$IRECI[is.infinite(m$IRECI)] <- median(m$IRECI)

# Predict C1
m$probs_C1 <- predict(rf_model_C1, newdata=m, type = "prob")[, "1"]
mean(m$probs_C1)

# Predict C2
m$probs_C2 <- predict(rf_model_C2, newdata=m, type = "prob")[, "1"]
mean(m$probs_C2)

# Predict C3
m$probs_C3 <- predict(rf_model_C3, newdata=m, type = "prob")[, "1"]
mean(m$probs_C3)

s <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s <- s[s$POINT_NUTS0 == "IT",]
s <- s[substr(s$SURVEY_LC1,1,1) == "C",]
s$LC2 <- as.factor(substr(s$SURVEY_LC1,1,2))
s <- merge(m[,c("POINT_ID",c("probs_C1","probs_C2","probs_C3"))],
           s[,c("POINT_ID","LC2")],by="POINT_ID")
s$LC2 <- as.factor(s$LC2)
s$POINT_ID <- s$LC1 <- NULL

load(paste0(datapath,"ensemble_model_C.RData"))

m$LC_predicted <- predict(rf_model,m)

table(m$LC_predicted,useNA = "ifany")/nrow(m)

t1 <- as.data.frame(round(prop.table(xtabs(~LC_predicted,data=m)),4)*100)
# t1$Estimates <- t1$Freq * correction_factors$correction
t1

# write.table(mpred,paste0(datapath,"Italy_master_2018_with_predictions_A.csv"),sep=",",quote=F,row.names=F)

s2 <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s2 <- s2[s2$POINT_NUTS0 == "IT",]
s2 <- s2[substr(s2$SURVEY_LC1,1,1) == "C",]
s2 <- merge(s2,m[,c("POINT_ID","LC_predicted")],by="POINT_ID")
s2$LC2 <- as.factor(substr(s2$SURVEY_LC1,1,2))
t2 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC_predicted,data=s2)),4)*100)
t2

t3 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC2,data=s2)),4)*100)
t3 <- t3[-1,]
t3

# t1$LC_predicted <- factor(t1$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t1 <- t1[order(t1$LC_predicted), ]
# t2$LC_predicted <- factor(t2$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t2 <- t2[order(t2$LC_predicted), ]
mat <- rbind(MasterPredicted = t1$Freq, SurveyPredicted = t2$Freq, SurveyObserved = t3$Freq)
mat <- as.matrix(mat)
mat

# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange","darkgreen"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("C1","C2","C3"),
        ylab = "Percentage",
        main = "Observed vs predicted 2018")

legend("topright", legend = c("Master Predicted", "Survey Predicted", "Survey Observed"), 
       fill = c("skyblue", "orange", "darkgreen"))

mat <- as.data.frame(mat)

colnames(mat) <-c("C1", "C2", "C3")
mat$reference <- c("Master Predicted 2022", "Survey Predicted 2022", "Survey Observed 2022")
write.table(mat,paste0(datapath,"Italy_estimates_predictions_2022_C.csv"),sep=",",quote=F,row.names=F)
