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
# load(paste0(datapath,"rf_model_A_reduced.RData"))
# load(paste0(datapath,"rf_model_B_reduced.RData"))
# load(paste0(datapath,"rf_model_C_reduced.RData"))
# load(paste0(datapath,"rf_model_D_reduced.RData"))
# load(paste0(datapath,"rf_model_E_reduced.RData"))
# load(paste0(datapath,"rf_model_F_reduced.RData"))
# load(paste0(datapath,"rf_model_G_reduced.RData"))
# load(paste0(datapath,"rf_model_H_reduced.RData"))
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


s22 <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s22 <- s22[s22$POINT_NUTS0 == "IT",]
s22$LC1 <- as.factor(substr(s22$SURVEY_LC1,1,1))
table(s22$LC1,useNA="ifany")
s22 <- merge(m[,c("POINT_ID",c("probs_A","probs_B","probs_C","probs_D",
                             "probs_E","probs_F","probs_G","probs_H"))],
           s22[,c("POINT_ID","LC1","wgt_2nd_phase")],by="POINT_ID")
table(s22$LC1)
# table(s22$SURVEY_OBS_TYPE)
# s22a$POINT_ID <- s22a$wgt_2nd_phase <- NULL
set.seed(1234)
rf_model <- randomForest(LC1 ~ . -POINT_ID - wgt_2nd_phase,
                               data=s22,
                               importance=TRUE,
                               # mtry=15,
                               # nodesize=5,
                               ntree=200,
                               do.trace=TRUE)
s22$LC_predicted <- predict(rf_model,s22)
# Evaluate model
cm22 <- confusionMatrix(s22$LC_predicted, s22$LC1)
cm22
t <- cm22$table
addmargins(t)
addmargins(round(prop.table(t),4))*100

s22 <- s22[!duplicated(s22$POINT_ID),]


# s18 <- fread(paste0(datapath,"Survey_2018_cal_wgt.txt"))
# s18 <- s18[s18$NUTS0_16 == "IT",]
# s18$LC1 <- as.factor(substr(s18$land_cover,1,1))
# table(s18$LC1,useNA="ifany")
# s18 <- merge(m[,c("POINT_ID",c("probs_A","probs_B","probs_C","probs_D",
#                              "probs_E","probs_F","probs_G","probs_H"))],
#            s18[,c("POINT_ID","LC1","cal_wgt")],by="POINT_ID")
# table(s18$LC1)
# 
# 
# rf_predictions <- predict(rf_model,s18)
# # Evaluate model
# cm18 <- confusionMatrix(rf_predictions, s18$LC1)
# cm18
# t18 <- addmargins(cm18$table)
# t18 
# 
# correction_factors <- read.csv(paste0(datapath,"correction_factors.csv"))

load(paste0(datapath,"ensemble_model_reduced.RData"))
m$LC_predicted <- predict(rf_model,m)

table(m$LC_predicted,useNA = "ifany")/nrow(m)

t1 <- as.data.frame(round(prop.table(xtabs(~LC_predicted,data=m)),4)*100)
# t1$Estimates <- t1$Freq * correction_factors$correction
t1

write.table(m,paste0(datapath,"Italy_master_2022_with_LC1_predictions.csv"),sep=",",quote=F,row.names=F)

# s18 <- merge(s18,m[,c("POINT_ID","LC_predicted")],by="POINT_ID")
# s22 <- merge(s22,m[,c("POINT_ID","LC_predicted")],by="POINT_ID")

t2 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC_predicted,data=s22)),4)*100)
t2

t3 <- as.data.frame(round(prop.table(xtabs(wgt_2nd_phase~LC1,data=s22)),4)*100)
t3



# t1$LC_predicted <- factor(t1$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t1 <- t1[order(t1$LC_predicted), ]
# t2$LC_predicted <- factor(t2$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t2 <- t2[order(t2$LC_predicted), ]
mat <- rbind(MasterPredicted = t1$Freq, 
             Survey2018predicted = t2$Freq, 
             Survey2022Observed = t3$Freq)
mat

# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue","orange","darkgreen"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
        ylab = "Percentage",
        main = "Observed vs predicted 2022")

legend("topright", legend = c("Master Predicted", 
                              "Survey 2022 Predicted",
                            "Survey 2022 Observed"), 
       fill = c("skyblue","orange", "darkgreen"))

mat <- as.data.frame(mat)

colnames(mat) <-c("A", "B", "C", "D", "E", "F", "G", "H")
mat$reference <- c("Master Predicted", 
                   "Survey 2022 Predicted", 
                   "Survey 2022 Observed")
write.table(mat,paste0(datapath,"Italy_estimates_predictions_2022_3.csv"),sep=",",quote=F,row.names=F)
