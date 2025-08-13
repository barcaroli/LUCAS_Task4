library(caret)
library(randomForest)
setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/models/"
#------------------------------------------------------------------------------

data <- read.csv("S2_S1_Italy_2018_trajectories_sample.csv")
data$target <- as.factor(ifelse(data$LC1 == "F",1,0))
table(data$target,useNA="ifany")
data$LC1 <- NULL

preProc_vals <- preProcess(
  x      = data,
  method = "medianImpute"
)
data <- predict(preProc_vals, data)
# library(randomForest)
# data <- rfImpute(y ~ ., data = data, iter = 5)


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
                         importance=TRUE,
                         mtry=5,
                         nodesize=3,
                         ntree=500,
                         do.trace=TRUE)
save(rf_model_F,file=paste0(datapath,"rf_model_F.RData"))
rf_model <- rf_model_F
plot(rf_model)
imp_df <- as.data.frame(rf_model$importance)
imp_df$Variable <- row.names(imp_df)
imp_df[order(imp_df$MeanDecreaseGini, decreasing = TRUE), c("Variable", "MeanDecreaseGini")]

# probs <- predict(rf_model, newdata=test_data, type = "prob")[, "1"]
# hist(probs)
# rf_predictions <- as.factor(ifelse(probs > 0.33, 1, 0))  # abbassa la soglia
rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$target)
cm
t <- addmargins(cm$table)
t 
#               Reference
# Prediction    0    1  Sum
# 0   14696    41 14737
# 1       1   294   295
# Sum 14697   335 15032

