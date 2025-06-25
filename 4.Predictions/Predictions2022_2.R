library(caret)
library(randomForest)
library(data.table)

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
# any(is.infinite(as.matrix(m)))  # TRUE se ci sono Inf o -Inf
# m[is.infinite(as.matrix(m))]

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

# Predict A
m <- m[!m$POINT_ID %in% m_D$POINT_ID,]
probs <- predict(rf_model_A, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.33, 1, 0)) 
table(m$LC_predicted,useNA = "ifany")
m_A <- m[m$LC_predicted==1,]
m_A$LC_predicted <- "A"

# Predict E
m <- m[!m$POINT_ID %in% m_A$POINT_ID,]
probs <- predict(rf_model_E, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.33, 1, 0)) 
table(m$LC_predicted,useNA = "ifany")
m_E <- m[m$LC_predicted==1,]
m_E$LC_predicted <- "E"

# Predict B
m <- m[!m$POINT_ID %in% m_E$POINT_ID,]
probs <- predict(rf_model_B, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.41, 1, 0)) 
table(m$LC_predicted,useNA = "ifany")
m_B <- m[m$LC_predicted==1,]
m_B$LC_predicted <- "B"

# Predict C
m <- m[!m$POINT_ID %in% m_B$POINT_ID,]
probs <- predict(rf_model_C, newdata=m, type = "prob")[, "1"]
m$LC_predicted <- as.factor(ifelse(probs > 0.46, 1, 0)) 
table(m$LC_predicted,useNA = "ifany")
m_C <- m[m$LC_predicted==1,]
m_C$LC_predicted <- "C"

mpred <- rbind(m_A,m_B,m_C,m_D,m_E,m_F,m_G,m_H)
mpred$LC_predicted <- factor(mpred$LC_predicted)
table(mpred$LC_predicted,useNA = "ifany")

t1 <- as.data.frame(round(prop.table(xtabs(~LC_predicted,data=mpred)),4)*100)
t1

write.table(mpred,paste0(datapath,"Italy_master_2022_with_predictions_2.csv"),sep=",",quote=F,row.names=F)
library(data.table())
s <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s <- s[s$POINT_NUTS0 == "IT",]
s <- merge(s,mpred[,c("POINT_ID","LC_predicted")],by="POINT_ID")
s$LC1 <- substr(s$SURVEY_LC1,1,1)
t2 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC1,data=s)),4)*100)
t2

t3 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC_predicted,data=s)),4)*100)
t3

# t1$LC_predicted <- factor(t1$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t1 <- t1[order(t1$LC_predicted), ]
# t2$LC_predicted <- factor(t2$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t2 <- t2[order(t2$LC_predicted), ]
mat <- rbind(MasterPredicted = t1$Freq, SurveyPredicted = t3$Freq, SurveyObserved = t2$Freq)

# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange","darkgreen"),
        ylim = c(0, max(mat) + 5),
        names.arg = levels(t1$LC_predicted)[order(levels(t1$LC_predicted))],
        ylab = "Percentage",
        main = "Observed vs predicted 2022")

legend("topright", legend = c("Master Predicted", "Survey Predicted", "Survey Observed"), 
       fill = c("skyblue", "orange", "darkgreen"))

mat <- as.data.frame(mat)
colnames(mat) <-c("A", "B", "C", "D", "E", "F", "G", "H")

write.table(mat,paste0(datapath,"Italy_estimates_predictions_2022_2.csv"),sep=",",quote=F,row.names=F)
