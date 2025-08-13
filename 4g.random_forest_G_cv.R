library(caret)
# library(randomForest)
setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/models/"
#------------------------------------------------------------------------------

df <- read.csv("S2_S1_Italy_2018_trajectories_sample.csv")
df$target <- as.factor(ifelse(df$LC1 == "G",1,0))

lvls_old <- levels(df$target)
lvls_new <- make.names(lvls_old)   # "0" -> "X0", "classe 1" -> "classe.1", ecc.
map_back <- setNames(lvls_old, lvls_new)  # per tornare ai nomi originali se vuoi
levels(df$target) <- lvls_new

table(df$target,useNA="ifany")
df$LC1 <- NULL

preProc_vals <- preProcess(
  x      = df,
  method = "medianImpute"
)
df <- predict(preProc_vals, df)


library(caret)
library(ranger)
library(MLmetrics)
set.seed(123)

# --- 1) Dati ---
# df: data.frame con variabili predittive + target

# --- 2) Train/Test split (stratificato) ---
idx   <- createDataPartition(df$target, p = 0.80, list = FALSE)
train <- df[idx, ]
test  <- df[-idx, ]

# (Opzione) pesi di classe per sbilanciamento (inverso della frequenza nel train)
class_w <- 1 / as.numeric(table(train$target))
names(class_w) <- levels(train$target)

# (Opzione) sampling dentro la CV: "up", "down" o "smote"
sampling_method <- NULL  # es. "smote" se molto sbilanciato

# --- 3) Controllo della CV (5x3) e metriche ---
ctrl <- trainControl(
  method = "repeatedcv",
  number = 5,
  repeats = 3,
  classProbs = TRUE,
  summaryFunction = multiClassSummary,  # richiede ModelMetrics
  savePredictions = "final",
  verboseIter = TRUE,
  sampling = sampling_method
)

# --- 4) Griglia di tuning ---
p <- ncol(train) - 1L  # num. predittori
grid_ranger <- expand.grid(
  mtry          = unique(pmax(1, round(seq(sqrt(p)/2, sqrt(p)*2, length.out = 6)))),
  splitrule     = c("gini", "extratrees"),
  min.node.size = c(1, 3, 5, 10, 15)
)

# --- 5) Training con CV + tuning ---
fit <- train(
  target ~ . -POINT_ID, 
  data = train,
  method = "ranger",
  trControl = ctrl,
  tuneGrid  = grid_ranger,
  num.trees = 500,
  importance = "impurity",          # o "permutation"
  metric = "Accuracy",              # se vuoi ottimizzare la BA: "Mean_Balanced_Accuracy"
  class.weights = class_w,         # rimuovi se non vuoi pesi di classe
  verbose =TRUE
)

print(fit)
print(fit$bestTune)
# plot(fit)  # curva di tuning

# --- 6) Valutazione sul test set ---
pred_class <- predict(fit, newdata = test)
pred_prob  <- predict(fit, newdata = test, type = "prob")

cm <- confusionMatrix(pred_class, test$target)
cm

# Balanced Accuracy macro dal confusion matrix
ba_per_class <- (cm$byClass[, "Sensitivity"] + cm$byClass[, "Specificity"]) / 2
BA_test <- mean(ba_per_class, na.rm = TRUE)
BA_test

# --- 7) Uso del modello e salvataggio ---
# predict(fit, newdata = nuovi_dati)
# predict(fit, newdata = nuovi_dati, type = "prob")
rf_model_G <- fit
save(rf_model_G, file=paste0(datapath,"rf_caret_ranger_G.rds"))
