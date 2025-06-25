#------------------------------------------------------------------------------
# 2.randomForest.R
#
# Script to estimate a random forest model to predict "land cover" using EE data
# Input: 1. LUCAS survey data of a given year / country augmented with EE data 
#        2. master dataset (processed by Ballin) for a given country augmented with EE data
# Output: 1. LUCAS survey data of a given year / country with predicted land cover 
#         2. master dataset (processed by Ballin) with predicted land cover 
#         3. random forest model
#------------------------------------------------------------------------------
library(caret)
# library(ranger)
library(randomForest)
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
#------------------------------------------------------------------------------
# Input: 1. LUCAS survey data of a given year / country augmented with EE data 
data <- read.csv(paste0(datapath,"Italy_sample_2018.csv"))
data$target <- as.factor(ifelse(data$LC1 == "D",1,0))
table(data$target,useNA="ifany")
data$LC1 <- NULL

preProc_vals <- preProcess(
  x      = data,
  method = "medianImpute"
)

# 2.2 Applica l’imputazione sia su train che su test
data <- predict(preProc_vals, data)

# all_feats <- c(paste0("JM_NDVI_",LETTERS[1:8]),
#                paste0("JM_VV_",LETTERS[1:8]),
#                paste0("JM_VH_",LETTERS[1:8]))

# data <- data[,c("target",all_feats)]

# Splitting the data
set.seed(1234)
training_rows <- createDataPartition(data$target, p = 0.8, list = FALSE)
train_data <- data[training_rows, ]
test_data <- data[-training_rows, ]
train_data$POINT_ID <- NULL
test_data$POINT_ID <- NULL


# Random Forest
set.seed(1234)
rf_model_D <- randomForest(target ~ ., 
                         data=train_data, 
                         importance=TRUE,
                         mtry=15,
                         nodesize=5,
                         ntree=500,
                         do.trace=TRUE)
save(rf_model_D,file=paste0(datapath,"rf_model_D.RData"))
rf_model <- rf_model_D
plot(rf_model)
imp_df <- as.data.frame(rf_model$importance)
# imp_df <- importance(rf_model)
# imp_df <- data.frame(Variable = rownames(imp),
#                      MeanDecreaseGini = imp[, "MeanDecreaseGini"])
imp_df$Variable <- row.names(imp_df)
imp_df[order(imp_df$MeanDecreaseGini, decreasing = TRUE), c("Variable", "MeanDecreaseGini")]

probs <- predict(rf_model, newdata=test_data, type = "prob")[, "1"]
hist(probs)
rf_predictions <- as.factor(ifelse(probs > 0.19, 1, 0))  # abbassa la soglia
# rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$target)
cm
t <- addmargins(cm$table)
t 
# Predict land cover on whole data
# data$predicted_LC <- predict(rf_model,data) 
# cm <- confusionMatrix(data$predicted_LC, data$target)
# cm
# t <- addmargins(cm$table)
# t 
# Plot
# categorie <- names(t[9,])[1:8]
# marg_riga <- as.numeric(t[9, 1:8])
# marg_col <- as.numeric(t[1:8, 9])
# marginals_mat <- cbind(Riga = marg_riga, Colonna = marg_col)
# rownames(marginals_mat) <- categorie
# barplot(
#   t(marginals_mat), 
#   beside = TRUE, 
#   col = c("skyblue", "orange"),
#   legend.text = c("Reference", "Prediction"),
#   args.legend = list(x = "topright"),
#   main = "RandomForest",
#   ylab = "Frequency",
#   names.arg = categorie
# )
# # Compute correction factors
# correction_factors <- as.data.frame(marginals_mat[,1] /  marginals_mat[,2])
# correction_factors$LC1 <- LETTERS[1:8]
# colnames(correction_factors)[1] <- "correction"
# correction_factors
# write.table(correction_factors,"correction_factors.csv",sep=",",quote=F,row.names = F)

#------------------------------------------------------------------------------
# Output: 1. LUCAS survey data of a given year / country with predicted land cover 
# write.table(data,"Italy_sample_2018_preds.csv",sep=",",quote=F,row.names=F)

# Prediction on whole master
#------------------------------------------------------------------------------
# Input 2. master dataset (processed by Ballin) for a given country augmented with EE data
# data2 <- read.csv("data_Italy_master_2018.csv")
# summary(data2)
# data2$predicted_LC <- predict(rf_model,data2)
#------------------------------------------------------------------------------
# Output 2. master dataset (processed by Ballin) for a given country with predicted land cover
# write.table(data2,"Italy_master_2018_preds.csv",sep=",",quote=F,row.names=F)

# CODE TO BE USED FOR 2022 MASTER
# data3 <- read.csv("data_Italy_master_2022.csv")
# summary(data3)
# # Ottieni una matrice TRUE/FALSE: TRUE se il valore è NA/NaN/Inf
# mask_bad <- sapply(data3, function(x) is.na(x) | is.nan(x) | is.infinite(x))
# # Per ogni riga: TRUE se almeno un valore è "cattivo"
# rows_to_remove <- apply(mask_bad, 1, any)
# # Tieni solo le righe "buone"
# data3_clean <- data3[!rows_to_remove, ]
# summary(data3_clean)
# data3_clean$predicted_LC <- predict(rf_model,data3_clean) 
# xtabs(~predicted_LC,data=data3_clean)
# round(prop.table(xtabs(~predicted_LC,data=data3_clean)),4)
# write.table(data3_clean,"Italy_master_2022_preds.csv",sep=",",quote=F,row.names=F)

#------------------------------------------------------------------------------
# Output 3. random forest model
# save(rf_model,file="random_forest_2018.RData")
