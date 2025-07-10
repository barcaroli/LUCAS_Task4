library(caret)
# library(ranger)
library(randomForest)
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
#------------------------------------------------------------------------------
# Input: 1. LUCAS survey data of a given year / country augmented with EE data 
data <- read.csv(paste0(datapath,"Italy_sample_2018_reduced_exp.csv"))
data$target <- as.factor(ifelse(data$LC1 == "F",1,0))
table(data$target,useNA="ifany")
data$LC1 <- NULL

preProc_vals <- preProcess(
  x      = data,
  method = "medianImpute"
)
data <- predict(preProc_vals, data)


# Splitting the data
set.seed(1234)
training_rows <- createDataPartition(data$target, p = 0.8, list = FALSE)
train_data <- data[training_rows, ]
test_data <- data[-training_rows, ]
train_data$POINT_ID <- NULL
test_data$POINT_ID <- NULL



# Random Forest
set.seed(1234)
rf_model_F <- randomForest(target ~ ., 
                         data=train_data, 
                         # classwt = c("0" = 1, "1" = 100),
                         importance=TRUE,
                         mtry=15,
                         nodesize=5,
                         ntree=200,
                         do.trace=TRUE)
save(rf_model_F,file=paste0(datapath,"rf_model_F.RData"))
rf_model <- rf_model_F
plot(rf_model)
imp_df <- as.data.frame(rf_model$importance)

imp_df$Variable <- row.names(imp_df)
imp_df[order(imp_df$MeanDecreaseGini, decreasing = TRUE), c("Variable", "MeanDecreaseGini")]

# probs <- predict(rf_model, newdata=test_data, type = "prob")[, "1"]
# hist(probs)
# rf_predictions <- as.factor(ifelse(probs > 0.16, 1, 0))  # abbassa la soglia
rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$target)
cm
t <- addmargins(cm$table)
t 
# Reference
# Prediction    0    1  Sum
# 0   5554   27 5581
# 1     31   21   52
# Sum 5585   48 5633
# #------------------------------------------------------------------------------
# # Input: 2. LUCAS survey data of a given year / country augmented with EE data 
# # reduced eliminating the followin variables
# # c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12",
# #   "EVI","MCARI","VH_lin_corr","VV_lin_corr","MCARI","PSSR","IRECI")
# data <- read.csv(paste0(datapath,"Italy_sample_2018_reduced.csv"))
# data$target <- as.factor(ifelse(data$LC1 == "G",1,0))
# table(data$target,useNA="ifany")
# data$LC1 <- NULL
# 
# preProc_vals <- preProcess(
#   x      = data,
#   method = "medianImpute"
# )
# data <- predict(preProc_vals, data)
# 
# 
# # Splitting the data
# set.seed(1234)
# training_rows <- createDataPartition(data$target, p = 0.8, list = FALSE)
# train_data <- data[training_rows, ]
# test_data <- data[-training_rows, ]
# train_data$POINT_ID <- NULL
# test_data$POINT_ID <- NULL
# 
# 
# 
# # Random Forest
# set.seed(1234)
# rf_model_G <- randomForest(target ~ ., 
#                            data=train_data, 
#                            # classwt = c("0" = 1, "1" = 100),
#                            importance=TRUE,
#                            mtry=15,
#                            nodesize=5,
#                            ntree=200,
#                            do.trace=TRUE)
# save(rf_model_G,file=paste0(datapath,"rf_model_G_reduced.RData"))
# rf_model <- rf_model_G
# plot(rf_model)
# imp_df <- as.data.frame(rf_model$importance)
# # imp_df <- importance(rf_model)
# # imp_df <- data.frame(Variable = rownames(imp),
# #                      MeanDecreaseGini = imp[, "MeanDecreaseGini"])
# imp_df$Variable <- row.names(imp_df)
# imp_df[order(imp_df$MeanDecreaseGini, decreasing = TRUE), c("Variable", "MeanDecreaseGini")]
# 
# probs <- predict(rf_model, newdata=test_data, type = "prob")[, "1"]
# hist(probs)
# rf_predictions <- as.factor(ifelse(probs > 0.16, 1, 0))  # abbassa la soglia
# # rf_predictions <- predict(rf_model,test_data)
# # Evaluate models on test data
# cm <- confusionMatrix(rf_predictions, test_data$target)
# cm
# t <- addmargins(cm$table)
# t 
# # Reference
# # Prediction    0    1  Sum
# # 0   5554   27 5581
# # 1     31   21   52
# # Sum 5585   48 5633