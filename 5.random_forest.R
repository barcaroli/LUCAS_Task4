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
setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
library(caret)
library(randomForest)
library(data.table)
datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/models\\"
#-------------------------------------------------------------------------------------
# Load models
load(paste0(datapath,"rf_model_A.RData"))
load(paste0(datapath,"rf_model_B.RData"))
load(paste0(datapath,"rf_model_C.RData"))
load(paste0(datapath,"rf_model_D.RData"))
load(paste0(datapath,"rf_model_E.RData"))
load(paste0(datapath,"rf_model_F.RData"))
load(paste0(datapath,"rf_model_G.RData"))
load(paste0(datapath,"rf_model_H.RData"))

m <- fread("S2_S1_Italy_2018_trajectories_sample.csv")
summary(m)
#Impute missing values
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

s18 <-fread("S2_S1_Italy_2018_trajectories_sample.csv")
s18 <- s18[!duplicated(s18$POINT_ID),]
# s18 <- s18[!duplicated(s18$POINT_ID),]
table(s18$LC1)
s18 <- merge(m[,c("POINT_ID",c("probs_A","probs_B","probs_C","probs_D","probs_E","probs_F","probs_G","probs_H"))],
             s18[,c("POINT_ID","LC1")],by="POINT_ID")
s18$LC1 <- as.factor(s18$LC1)
summary(s18)

# s18$POINT_ID <- NULL
set.seed(42)
training_rows <- createDataPartition(s18$LC1, p = 0.8, list = FALSE)
train_data <- s18[training_rows, ]
test_data <- s18[-training_rows, ]

summary(train_data)


set.seed(1234)
rf_model <- randomForest(LC1 ~ . - POINT_ID , 
                         data=train_data, 
                         importance=TRUE,
                         # mtry=15,
                         # nodesize=5,
                         ntree=200,
                         do.trace=TRUE)
save(rf_model,file=paste0(datapath,"ensemble_model.RData"))
# load(paste0(datapath,"ensemble_model.RData"))
rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$LC1)
cm
t <- cm$table
addmargins(t)
# Reference
# Prediction     A     B     C     D     E     F     G     H   Sum
# A    1201     4     2     0     3     1     3     0  1214
# B       3  6038     3     2     6     3     3     0  6058
# C       4     8  6540     2     5     0     0     0  6559
# D       2     0     2  1119     0     0     0     0  1123
# E       3     8    10     0  3039     1     0     0  3061
# F       0     0     0     0     1   414     0     0   415
# G       1     0     0     0     0     0   327     0   328
# H       0     0     0     0     0     0     0    31    31
# Sum  1214  6058  6557  1123  3054   419   333    31 18789
