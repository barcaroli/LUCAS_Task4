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
m <- fread(paste0(datapath,"Italy_master_2018.csv"))
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

s18 <-fread(paste0(datapath,"Italy_sample_2018.csv"))
table(s18$LC1)
s18 <- merge(m[,c("POINT_ID",c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H"))],
           s18[,c("POINT_ID","LC1")],by="POINT_ID")
s18$LC1 <- as.factor(s18$LC1)


set.seed(42)
training_rows <- createDataPartition(s18$LC1, p = 0.75, list = FALSE)
train_data <- s18[training_rows, ]
test_data <- s18[-training_rows, ]


set.seed(1234)
rf_model <- randomForest(LC1 ~ ., 
                         data=train_data, 
                         importance=TRUE,
                         # mtry=15,
                         # nodesize=5,
                         ntree=500,
                         do.trace=TRUE)
save(rf_model,file=paste0(datapath,"ensemble_model_reduced.RData"))
rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$LC1)
cm
t <- addmargins(cm$table)
t 


#---------------------------------

categorie <- names(t[9,])[1:8]
marg_riga <- as.numeric(t[9, 1:8])
marg_col <- as.numeric(t[1:8, 9])
marginals_mat <- cbind(Riga = marg_riga, Colonna = marg_col)
rownames(marginals_mat) <- categorie
barplot(
  t(marginals_mat), 
  beside = TRUE, 
  col = c("skyblue", "orange"),
  legend.text = c("Reference", "Prediction"),
  args.legend = list(x = "topright"),
  main = "RandomForest",
  ylab = "Frequency",
  names.arg = categorie
)

# Compute correction factors
correction_factors <- as.data.frame(marginals_mat[,1] /  marginals_mat[,2])
correction_factors$LC1 <- LETTERS[1:8]
colnames(correction_factors)[1] <- "correction"
correction_factors
write.table(correction_factors,paste0(datapath,"correction_factors.csv"),sep=",",quote=F,row.names = F)


m$LC_predicted <- predict(rf_model,m)

table(m$LC_predicted,useNA = "ifany")/nrow(m)

t1 <- as.data.frame(round(prop.table(xtabs(~LC_predicted,data=m)),4)*100)
t1$Estimates <- t1$Freq * correction_factors$correction
t1

write.table(m,paste0(datapath,"Italy_master_2018_with_LC1_predictions.csv"),sep=",",quote=F,row.names=F)
library(data.table())
s2 <- fread(paste0(datapath,"Survey_2018_cal_wgt.txt"))
s2 <- s2[s2$NUTS0_16 == "IT",]
s2 <- merge(s2,m[,c("POINT_ID","LC_predicted")],by="POINT_ID")
s2$LC1 <- substr(s2$land_cover,1,1)
t2 <- as.data.frame(round(prop.table(xtabs(cal_wgt~LC_predicted,data=s2)),4)*100)
t2

t3 <- as.data.frame(round(prop.table(xtabs(cal_wgt~LC1,data=s2)),4)*100)
t3

# t1$LC_predicted <- factor(t1$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t1 <- t1[order(t1$LC_predicted), ]
# t2$LC_predicted <- factor(t2$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t2 <- t2[order(t2$LC_predicted), ]
mat <- rbind(MasterPredicted = t1$Freq, SurveyPredicted = t2$Freq, SurveyObserved = t3$Freq)
mat

# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange","darkgreen"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
        ylab = "Percentage",
        main = "Observed vs predicted 2018")

legend("topright", legend = c("Master Predicted", "Survey Predicted", "Survey Observed"), 
       fill = c("skyblue", "orange", "darkgreen"))

mat <- as.data.frame(mat)

colnames(mat) <-c("A", "B", "C", "D", "E", "F", "G", "H")
mat$reference <- c("Master Predicted 2018", "Survey Predicted 2018", "Survey Observed 2018")
write.table(mat,paste0(datapath,"Italy_estimates_predictions_2018_3.csv"),sep=",",quote=F,row.names=F)

# Compute correction factors
corr <- mat[2,c(1:8)] /  mat[1,c(1:8)]
correction_factors <- as.data.frame(list(LC1 = LETTERS[1:8], correction = t(corr)))
colnames(correction_factors)[2] <- "correction"
correction_factors
write.table(correction_factors,paste0(datapath,"correction_factors.csv"),sep=",",quote=F,row.names = F)


