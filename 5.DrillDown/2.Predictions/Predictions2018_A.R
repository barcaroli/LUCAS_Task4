library(caret)
library(randomForest)
library(data.table)
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/4.Predictions")
#-------------------------------------------------------------------------------------
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
# Load models
load(paste0(datapath,"rf_model_A1.RData"))
load(paste0(datapath,"rf_model_A2.RData"))
load(paste0(datapath,"rf_model_A3.RData"))
#-------------------------------------------------------------------------------------
# Read master
m <- fread(paste0(datapath,"Italy_master_2018_with_LC1_predictions.csv"))
m <- m[,-c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H")]
table(m$LC_predicted)
m <- m[m$LC_predicted == "A",]
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

# Predict A1
m$probs_A1 <- predict(rf_model_A1, newdata=m, type = "prob")[, "1"]
mean(m$probs_A)

# Predict A2
m$probs_A2 <- predict(rf_model_A2, newdata=m, type = "prob")[, "1"]
mean(m$probs_A2)

# Predict A3
m$probs_A3 <- predict(rf_model_A3, newdata=m, type = "prob")[, "1"]
mean(m$probs_A3)

s <- fread(paste0(datapath,"Survey_2018_cal_wgt.txt"))
s <- s[s$NUTS0_16 == "IT",]
s <- s[substr(s$land_cover,1,1) == "A",]
s$LC2 <- as.factor(substr(s$land_cover,1,2))
s <- merge(m[,c("POINT_ID",c("probs_A1","probs_A2","probs_A3"))],
           s[,c("POINT_ID","LC2")],by="POINT_ID")
s$LC2 <- as.factor(s$LC2)
s$POINT_ID <- s$LC1 <- NULL

set.seed(42)
training_rows <- createDataPartition(s$LC2, p = 0.75, list = FALSE)
train_data <- s[training_rows, ]
test_data <- s[-training_rows, ]
train_data$POINT_ID <- NULL
test_data$POINT_ID <- NULL

set.seed(1234)
rf_model <- randomForest(LC2 ~ ., 
                         data=train_data, 
                         importance=TRUE,
                         # mtry=15,
                         # nodesize=5,
                         ntree=500,
                         do.trace=TRUE)
save(rf_model,file=paste0(datapath,"ensemble_model_A.RData"))
rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$LC2)
cm
t <- addmargins(cm$table)
t 

categorie <- names(t[4,])[1:3]
marg_riga <- as.numeric(t[4, 1:3])
marg_col <- as.numeric(t[1:3, 4])
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
# correction_factors <- as.data.frame(marginals_mat[,1] /  marginals_mat[,2])
# correction_factors$LC1 <- LETTERS[1:8]
# colnames(correction_factors)[1] <- "correction"
# correction_factors
# write.table(correction_factors,paste0(datapath,"correction_factors.csv"),sep=",",quote=F,row.names = F)


m$LC_predicted <- predict(rf_model,m)

table(m$LC_predicted,useNA = "ifany")/nrow(m)

t1 <- as.data.frame(round(prop.table(xtabs(~LC_predicted,data=m)),4)*100)
# t1$Estimates <- t1$Freq * correction_factors$correction
t1

# write.table(mpred,paste0(datapath,"Italy_master_2018_with_predictions_3.csv"),sep=",",quote=F,row.names=F)

s2 <- fread(paste0(datapath,"Survey_2018_cal_wgt.txt"))
s2 <- s2[s2$NUTS0_16 == "IT",]
s2 <- s2[substr(s2$land_cover,1,1) == "A",]
s2 <- merge(s2,m[,c("POINT_ID","LC_predicted")],by="POINT_ID")
s2$LC2 <- as.factor(substr(s2$land_cover,1,2))
t2 <- as.data.frame(round(prop.table(xtabs(cal_wgt~LC_predicted,data=s2)),4)*100)
t2

t3 <- as.data.frame(round(prop.table(xtabs(cal_wgt~LC2,data=s2)),4)*100)
t3

# t1$LC_predicted <- factor(t1$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t1 <- t1[order(t1$LC_predicted), ]
# t2$LC_predicted <- factor(t2$LC_predicted, levels = c("A", "B", "C", "D", "E", "F", "G", "H"))
# t2 <- t2[order(t2$LC_predicted), ]
mat <- rbind(MasterPredicted = t1$Freq, SurveyPredicted = t3$Freq, SurveyObserved = t2$Freq)
mat

# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange","darkgreen"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A1","A2","A3"),
        ylab = "Percentage",
        main = "Observed vs predicted 2018")

legend("topright", legend = c("Master Predicted", "Survey Predicted", "Survey Observed"), 
       fill = c("skyblue", "orange", "darkgreen"))

mat <- as.data.frame(mat)

colnames(mat) <-c("A1", "A2", "A3")
mat$reference <- c("Master Predicted 2018", "Survey Predicted 2018", "Survey Observed 2018")
write.table(mat,paste0(datapath,"Italy_estimates_predictions_2018_A.csv"),sep=",",quote=F,row.names=F)
