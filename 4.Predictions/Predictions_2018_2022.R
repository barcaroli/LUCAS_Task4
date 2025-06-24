library(caret)
library(randomForest)
library(data.table)

#-------------------------------------------------------------------------------------
# Load models
load("D:/Google Drive/LUCAS Copernicus/EarthEngine/3.MachineLearning/rf_model_ABCE.RData")
rf_model_ABCE <- rf_model
load("D:/Google Drive/LUCAS Copernicus/EarthEngine/3.MachineLearning/rf_model_D.RData")
rf_model_D <- rf_model
load("D:/Google Drive/LUCAS Copernicus/EarthEngine/3.MachineLearning/rf_model_F.RData")
rf_model_F <- rf_model
load("D:/Google Drive/LUCAS Copernicus/EarthEngine/3.MachineLearning/rf_model_G.RData")
rf_model_G <- rf_model
load("D:/Google Drive/LUCAS Copernicus/EarthEngine/3.MachineLearning/rf_model_H.RData")
rf_model_H <- rf_model
#-------------------------------------------------------------------------------------
# Read master
m <- fread("D:/Google Drive/LUCAS Copernicus/EarthEngine/3.MachineLearning/Italy_master_2022.csv")
summary(m)
# Impute missing values
preProc_vals <- preProcess(
  x      = m,
  method = "medianImpute"
)
m <- predict(preProc_vals, m)

# anyNA(m)              # TRUE se ci sono NA
# any(is.nan(as.matrix(m)))  # TRUE se ci sono NaN
any(is.infinite(as.matrix(m)))  # TRUE se ci sono Inf o -Inf
m[is.infinite(as.matrix(m))]

m$PSSR[is.infinite(m$PSSR)] <- median(m$PSSR)
m$IRECI[is.infinite(m$IRECI)] <- median(m$IRECI)

# Predict H
probs <- predict(rf_model_H, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.14, 1, 0))  
table(m$LC_predicted,useNA = "ifany")
m_H <- m[m$LC_predicted==1,]
m_H$LC_predicted <- "H"

# Predict G
m <- m[!m$POINT_ID %in% m_H$POINT_ID,]
probs <- predict(rf_model_G, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.16, 1, 0))  
table(m$LC_predicted,useNA = "ifany")
m_G <- m[m$LC_predicted==1,]
m_G$LC_predicted <- "G"

# Predict F
m <- m[!m$POINT_ID %in% m_G$POINT_ID,]
probs <- predict(rf_model_F, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.13, 1, 0))  
table(m$LC_predicted,useNA = "ifany")
m_F <- m[m$LC_predicted==1,]
m_F$LC_predicted <- "F"

# Predict D
m <- m[!m$POINT_ID %in% m_F$POINT_ID,]
probs <- predict(rf_model_D, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.19, 1, 0))  
table(m$LC_predicted,useNA = "ifany")
m_D <- m[m$LC_predicted==1,]
m_D$LC_predicted <- "D"

# Predict A, B, C, E
m <- m[!m$POINT_ID %in% m_D$POINT_ID,]
m$LC_predicted <- as.factor(predict(rf_model_ABCE, newdata=m))
table(m$LC_predicted,useNA = "ifany")

mpred <- rbind(m,m_D,m_F,m_G,m_H)
mpred$LC_predicted <- factor(mpred$LC_predicted)
table(mpred$LC_predicted,useNA = "ifany")
t1 <- as.data.frame(round(prop.table(xtabs(~LC_predicted,data=mpred)),4)*100)
t1

write.table(mpred,"Italy_master_2022_with_predictions.csv")

s <- fread("D:/Google Drive/LUCAS Copernicus/EarthEngine/data/Survey_2022_cal_wgt_2nd_phase.txt")

s <- s[s$POINT_NUTS0 == "IT",]
s <- merge(s,mpred[,c("POINT_ID","LC_predicted")],by="POINT_ID")
s$LC1 <- substr(s$SURVEY_LC1,1,1)
t2 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC1,data=s)),4)*100)
t2

t3 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC_predicted,data=s)),4)*100)
t3

s2 <- fread("D:/Google Drive/LUCAS Copernicus/EarthEngine/data/Survey_2018_cal_wgt.txt")
s2 <- merge(s2,mpred[,c("POINT_ID","LC_predicted")],by="POINT_ID")
t4 <- as.data.frame(round(prop.table(xtabs(cal_wgt~LC_predicted,data=s2)),4)*100)
t4

t1$LC_predicted <- factor(t1$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
t1 <- t1[order(t1$LC_predicted), ]
t3$LC_predicted <- factor(t3$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
t3 <- t3[order(t3$LC_predicted), ]
t4$LC_predicted <- factor(t4$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
t4 <- t4[order(t4$LC_predicted), ]
mat <- rbind(MasterPredicted = t1$Freq, SurveyPredicted2022 = t3$Freq, 
             SurveyPredicted2018 = t4$Freq, SurveyObserved = t2$Freq)

# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange","darkgreen","red"),
        ylim = c(0, max(mat) + 5),
        names.arg = levels(t1$LC_predicted)[order(levels(t1$LC_predicted))],
        ylab = "Percentage",
        main = "Observed vs predicted 2022")

legend("topright", legend = c("Master Predicted", "Survey Predicted 2022", "Survey Predicted 2018", "Survey Observed"), 
       fill = c("skyblue", "orange", "darkgreen", "red"))
