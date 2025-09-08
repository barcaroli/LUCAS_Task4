#--------------------------------------------------------------------------------
# Script to predict "land cover" using rf and xgboost models
# Input
#   Trained classifier (rf_learner_mod.rds or xgb_learner_mod.rds).
#   AlphaEarth dataset (AlphaEarth_Italy_2018_sample.csv: bands A00–A63, LC1, POINT_ID).
#   Helper: threshold_equalize_marginals.R.
#   Settings: data paths, 5-fold CV.
# Processing
#   Load model/data; drop duplicate POINT_ID; coerce LC1 to factor; print class distribution.
#   Build mlr3 TaskClassif on the dataset.
#   Perform 5-fold cross-validation with the chosen model; collect out-of-fold predictions.
#   Compute OOF confusion matrix.
#   Estimate class thresholds via threshold_equalize_marginals(p_cv), then normalize to [0,1].
#   Apply thresholds to predictions with PredictionClassif$set_threshold() to rebalance decisions.
#   Recompute confusion matrix and summary metrics via caret::confusionMatrix().
#   Save thresholds (saveRDS) and the full session image (save.image).
# Output
#   OOF confusion matrices (before/after thresholding) with margins.
#   caret confusion-matrix report (accuracy, Kappa, per-class stats).
#   Normalized thresholds file: th_mlr3_rf.rds (or th_mlr3_xgb.rds).
#   Workspace snapshot: run_predict_RF.RData (or run_predict_XGB.RData).
#------------------------------------------------------------------------------
library(data.table)
library(caret)
source("threshold_equalize_marginals.R")
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow")
suppressPackageStartupMessages({
  library(data.table)
  library(mlr3verse)   # per predict_newdata
})
# datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/ALPHAEARTH/data/"
datapath <- "D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow/data/"

#--------------------------------
# Scelta del modello rf
mod <- readRDS(paste0(datapath,"rf_learner.rds"))
out <- paste0(datapath,"th_mlr3_rf.rds")
out2 <- paste0(datapath,"run_predict_RF.RData")

#--------------------------------
# Scelta del modello xgb
# mod <- readRDS(paste0(datapath,"xgb_learner.rds"))
# out <- paste0(datapath,"th_mlr3_xgb.rds")
# out2 <- paste0(datapath,"run_predict_XGB.RData")

mod

# test <- readRDS(paste0(datapath,"test_data.rds"))
sample <- fread(paste0(datapath,"AlphaEarth_Italy_2018_sample.csv"))
test_data <- sample[!duplicated((sample$POINT_ID)),]
test_data$LC1 <- as.factor(test_data$LC1)
addmargins(xtabs(~LC1,data=test_data))


# Prepara il task su cui valutare 
task <- TaskClassif$new("lc1_eval", backend = test_data, target = "LC1")  # X = tuo data.frame
# 

# CM out-of-fold 
rs <- rsmp("cv", folds = 5)
rr <- resample(task, mod, rs)        # store_models=FALSE va bene
p_cv <- rr$prediction()
cm_cv <- table(p_cv$truth, p_cv$response)
print(addmargins(t(cm_cv)))

# Equalizzazione su OOF:
eq_cv <- threshold_equalize_marginals(p_cv)
th_mlr3 <- eq_cv$thresholds / max(eq_cv$thresholds)   # scala in [0,1], max=1
th_mlr3
p_cv2 <- p_cv$clone(deep = TRUE)
p_cv2$set_threshold(th_mlr3)
print(addmargins(t(table(p_cv2$truth, p_cv2$response))))

saveRDS(th_mlr3,file=out)

cm <- confusionMatrix(p_cv2$truth, p_cv2$response)
cm
addmargins(cm$tab)

save.image(file=out2)




