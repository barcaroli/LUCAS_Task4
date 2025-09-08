# ============================================================
# AlphaEarth A00–A63 → LC1 (A..H)
# Input
#   AlphaEarth feature dataset (A00–A63 bands, optional terrain covariates).
#   User-defined parameters: number of folds, favored classes (D/F/G/H), recall weights, thresholds.
# Processing
#   Load and clean data (remove duplicates, NAs, enforce factors A–H).
#   Define an mlr3 classification task with stratified or custom CV.
#   Create a custom weighted recall metric that prioritizes favored classes.
#   Configure and tune two learners: Random Forest (ranger) with class weights, and XGBoost with oversampling.
#   Hyperparameter search via random search with evaluation limits (n_evals).
#   Benchmark learners with cross-validation; compute accuracy, balanced accuracy, weighted recall.
#   Extract confusion matrices, normalized confusion matrices, per-class recall, and pooled macro-F1.
#   Apply post-hoc threshold tuning (<1 for favored classes) to increase their detection rate.
#   Visualize normalized confusion matrices with ggplot heatmaps.
#   Save tuned AutoTuner objects, final learners, and session image.
# Output
#   Performance metrics (accuracy, balanced accuracy, weighted recall, macro-F1).
#   Confusion matrices before/after threshold tuning.
#   Plots of normalized confusion matrices.
#   Serialized models (.Rds) and saved workspace (.RData).
# ============================================================
n_evals_rf = 30
n_evals_xgb = 40

req <- c("mlr3verse","mlr3learners","mlr3pipelines","mlr3tuning",
         "paradox","data.table","ggplot2","ranger","xgboost","R6")  # [MOD] + R6
to_install <- setdiff(req, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install)
suppressPackageStartupMessages({
  library(data.table)
  library(mlr3verse)
  library(mlr3learners)
  library(mlr3pipelines)
  library(mlr3tuning)
  library(paradox)
  library(ggplot2)
  library(R6)  # [MOD]
})

# ---------------------------
# 0) Parametri da personalizzare
# ---------------------------
# datapath    <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/ALPHAEARTH/data"
datapath    <- "D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow/data/"
pattern     <- "\\AlphaEarth_Italy_2018_sample.csv$"
target_col  <- "LC1"
bands       <- sprintf("A%02d", 0:63)
id_col      <- "POINT_ID"               # opzionale
fold_col    <- "cv_fold"                # opzionale (interi 1..K). Se assente → CV stratificata
nfolds      <- 5
seed        <- 42

# [MOD] Classi da “favorire” (maggior recall)
favored <- c("D","F","G","H")
# [MOD] Peso maggiore in tuning per il recall delle classi favored
w_recall_vec <- setNames(rep(1, 8), LETTERS[1:8])
w_recall_vec[favored] <- 3

# [MOD] Soglie post-hoc per aumentare la detection delle classi favored (<1 = più facile predirle)
thresh_vec <- setNames(rep(1, 8), LETTERS[1:8])
thresh_vec[favored] <- 0.7   # puoi provare 0.6–0.8

# ---------------------------
# 1) Caricamento & pulizia
# ---------------------------
fs <- list.files(datapath, pattern = pattern, full.names = TRUE)
stopifnot("Nessun file CSV trovato" = length(fs) > 0)
X  <- rbindlist(lapply(fs, fread), use.names = TRUE, fill = TRUE)

if ("POINT_ID" %in% names(X)) X <- X[!duplicated(X$POINT_ID),]

need_cols <- c(bands, target_col)
stopifnot("Mancano colonne A00–A63 o LC1" = all(need_cols %in% names(X)))

if (is.numeric(X[[target_col]])) {
  map_letters <- setNames(LETTERS[1:8], as.character(1:8))
  X[[target_col]] <- factor(map_letters[as.character(X[[target_col]])], levels = LETTERS[1:8])
} else {
  X[[target_col]] <- toupper(trimws(as.character(X[[target_col]])))
  X <- X[X[[target_col]] %in% LETTERS[1:8]]
  X[[target_col]] <- factor(X[[target_col]], levels = LETTERS[1:8])
}

X <- X[!is.na(X[[target_col]])]
X <- X[complete.cases(X[, ..bands])]

extra_cov <- intersect(c("ELEV_DEM", "aspect_slope", "aspect_exposure"), names(X))
feat_cols <- c(bands, extra_cov)

keep <- unique(c(feat_cols, target_col, intersect(c(id_col, fold_col), names(X))))
X <- X[, ..keep]

cat("\n--- Distribuzione LC1 ---\n")
print(round(prop.table(table(X[[target_col]])), 4))

# ---------------------------
# 2) Task & Resampling
# ---------------------------
set.seed(seed)
task <- TaskClassif$new("lc1_ae", backend = X, target = target_col)

if (fold_col %in% names(X)) {
  folds <- as.integer(X[[fold_col]])
  K     <- max(folds, na.rm = TRUE)
  idx   <- split(seq_len(nrow(X)), folds)
  train_sets <- lapply(seq_len(K), function(k) unlist(idx[setdiff(seq_len(K), k)]))
  test_sets  <- lapply(seq_len(K), function(k) unlist(idx[k]))
  rsmp_use <- rsmp("custom")
  rsmp_use$instantiate(task, train = train_sets, test = test_sets)
  message(sprintf("Usata CV custom (%d fold) da '%s'.", K, fold_col))
} else {
  rsmp_use <- rsmp("cv", folds = nfolds)
  rsmp_use$instantiate(task)
  message(sprintf("Usata CV stratificata a %d fold.", nfolds))
}

# --- Metrica custom: Weighted Recall (usa .score) ---
MeasureClassifWRecall <- R6::R6Class("MeasureClassifWRecall",
                                     inherit = mlr3::MeasureClassif,
                                     public = list(
                                       weights = NULL,
                                       initialize = function(weights) {
                                         self$weights = weights
                                         super$initialize(
                                           id = "classif.wrecall",
                                           range = c(0, 1),
                                           minimize = FALSE,
                                           properties = character(0),        # non richiede model/task/learner
                                           predict_type = "response"         # lavora su etichette (non prob)
                                         )
                                       }
                                     ),
                                     private = list(
                                       .score = function(prediction, ...) { # <-- questo è il punto chiave
                                         cm  <- table(prediction$truth, prediction$response)
                                         rec <- diag(cm) / pmax(rowSums(cm), 1)
                                         
                                         cls <- rownames(cm)
                                         w   <- self$weights[cls]
                                         w[is.na(w)] <- 1
                                         
                                         sum(rec * w) / sum(w)
                                       }
                                     )
)

msr_wrecall <- MeasureClassifWRecall$new(w_recall_vec)

# ---------------------------
# 4) Learners + Spazi di tuning (mirati al recall D/F/G/H)
# ---------------------------

# ---- Random Forest (ranger) ----
tab   <- table(task$truth())
p     <- as.numeric(tab) / sum(tab)

alpha <- 1.6    # [MOD] più aggressivo di 1.0
base_w  <- (1 / p) ^ alpha
names(base_w) <- names(tab)
boost <- setNames(rep(1, length(tab)), names(tab))
boost[favored] <- 1.5  # [MOD] spinta extra per D/F/G/H
w_rf <- base_w * boost
w_rf <- w_rf / mean(w_rf)  # normalizz. (facoltativa)

lrn_rf <- lrn("classif.ranger",
              num.trees = 600,                      # [MOD] leggermente più alberi
              importance = "impurity",
              class.weights = w_rf,                 # [MOD]
              respect.unordered.factors = "order",
              seed = seed,
              predict_type = "prob"                 # [MOD] abilita threshold tuning
)

space_rf <- ps(
  mtry          = p_int(8, 48),
  min.node.size = p_int(1, 10),
  splitrule     = p_fct(c("gini", "extratrees"))
)

at_rf <- AutoTuner$new(
  learner      = lrn_rf,
  resampling   = rsmp("holdout"),
  measure      = msr_wrecall,                # [MOD] tuning su Weighted Recall
  search_space = space_rf,
  terminator   = trm("evals", n_evals = n_evals_rf),
  tuner        = tnr("random_search")
)

# ---- XGBoost (con oversampling più spinto) ----
po_cb <- po("classbalancing",
            id = "cb",
            adjust    = "minor",
            reference = "major",
            ratio     = 1.3)                  # [MOD] > 1 per favorire le minoritarie

set.seed(seed)

lrn_xgb <- lrn("classif.xgboost",
               objective        = "multi:softprob",
               eval_metric      = "mlogloss",
               nthread          = max(1L, parallel::detectCores() - 1L),
               tree_method      = "hist",
               booster          = "gbtree",
               predict_type     = "prob"       # [MOD] abilita threshold tuning
)

graph_xgb <- po_cb %>>% lrn_xgb
gxl       <- as_learner(graph_xgb)

space_xgb <- ps(
  classif.xgboost.eta              = p_dbl(0.03, 0.30),
  classif.xgboost.max_depth        = p_int(4, 12),
  classif.xgboost.min_child_weight = p_int(1, 8),
  classif.xgboost.subsample        = p_dbl(0.6, 1.0),
  classif.xgboost.colsample_bytree = p_dbl(0.6, 1.0),
  classif.xgboost.nrounds          = p_int(200, 1200)
)

at_xgb <- AutoTuner$new(
  learner      = gxl,
  resampling   = rsmp("holdout"),
  measure      = msr_wrecall,               # [MOD] tuning su Weighted Recall
  search_space = space_xgb,
  terminator   = trm("evals", n_evals = n_evals_xgb),
  tuner        = tnr("random_search")
)

# ---------------------------
# 5) Benchmark con CV finale (media ± sd)
# ---------------------------
design <- benchmark_grid(
  tasks = task,
  learners = list(at_rf, at_xgb),
  resamplings = rsmp_use
)
bmr <- benchmark(design)

cat("\n--- Metriche CV (media ± sd) ---\n")
measures <- list(
  msr("classif.acc"),
  msr("classif.bacc"),
  msr_wrecall                                # [MOD] riporta anche la metrica di tuning
)
print(bmr$aggregate(measures))

save.image(file="temp.RData")

# ------------------------------------------------------------
# Predizioni CV → Confusion matrix (usando bmr$resample_results)
# ------------------------------------------------------------
get_cm_from_bmr <- function(bmr, learner) {
  learner_id <- if (inherits(learner, "Learner")) learner$id else as.character(learner)
  dt <- data.table::as.data.table(bmr)
  uh <- unique(dt[learner_id == learner_id, uhash])
  if (length(uh) == 0L) {
    avail <- unique(dt$learner_id)
    stop(sprintf("Nessuna riga per learner_id='%s'. Learner disponibili: %s",
                 learner_id, paste(avail, collapse = ", ")))
  }
  rr_tab <- bmr$resample_results
  rr_row <- rr_tab[uhash == uh[1L]]
  if (nrow(rr_row) == 0L) stop("ResampleResult non trovato per uhash: ", uh[1L])
  rr <- rr_row$resample_result[[1L]]
  pred <- rr$prediction()
  cm  <- table(pred$truth, pred$response, useNA = "no")
  cmn <- sweep(cm, 1, pmax(rowSums(cm), 1), "/")
  list(cm = cm, cmn = cmn, pred = pred)
}

cm_rf  <- get_cm_from_bmr(bmr, "classif.ranger.tuned")
cm_xgb <- get_cm_from_bmr(bmr, "cb.classif.xgboost.tuned")

cat("\n-- RF CM (prima del threshold tuning) --\n");  print(addmargins(cm_rf$cm))
cat("\n-- XGB CM (prima del threshold tuning) --\n"); print(addmargins(cm_xgb$cm))

# ---------------------------
# Heatmap ggplot della CM normalizzata
# ---------------------------
plot_cm <- function(cmn, title = "Confusion Matrix (normalized)") {
  stopifnot(is.matrix(cmn))
  if (is.null(rownames(cmn))) rownames(cmn) <- sprintf("R%d", seq_len(nrow(cmn)))
  if (is.null(colnames(cmn))) colnames(cmn) <- sprintf("C%d", seq_len(ncol(cmn)))
  Truth <- rep(rownames(cmn), times = ncol(cmn))
  Pred  <- rep(colnames(cmn), each  = nrow(cmn))
  Val   <- as.numeric(cmn)
  long <- data.frame(Truth = factor(Truth, levels = rownames(cmn)),
                     Pred  = factor(Pred,  levels = colnames(cmn)),
                     Val   = Val,
                     check.names = FALSE)
  ggplot2::ggplot(long, ggplot2::aes(x = Pred, y = Truth, fill = Val)) +
    ggplot2::geom_tile() +
    ggplot2::geom_text(ggplot2::aes(label = sprintf("%.2f", Val)), size = 3) +
    ggplot2::scale_fill_gradient(low = "white", high = "steelblue") +
    ggplot2::labs(title = title, x = "Predicted", y = "True") +
    ggplot2::theme_minimal()
}

print(plot_cm(cm_rf$cmn,  "classif.ranger.tuned — CM normalizzata"))
print(plot_cm(cm_xgb$cmn, "cb.classif.xgboost.tuned — CM normalizzata"))

# ---------------------------
# Macro-F1 "pooled"
# ---------------------------
macro_f1_manual <- function(pred) {
  cm <- table(pred$truth, pred$response)
  k  <- nrow(cm)
  f1 <- numeric(k)
  for (i in seq_len(k)) {
    tp   <- cm[i, i]
    fp   <- sum(cm[-i, i])
    fn   <- sum(cm[i, -i])
    prec <- if ((tp + fp) == 0) NA else tp / (tp + fp)
    rec  <- if ((tp + fn) == 0) NA else tp / (tp + fn)
    f1[i] <- if (is.na(prec) || is.na(rec) || (prec + rec) == 0) NA else 2 * prec * rec / (prec + rec)
  }
  mean(f1, na.rm = TRUE)
}

rf_macro_f1  <- macro_f1_manual(cm_rf$pred)
xgb_macro_f1 <- macro_f1_manual(cm_xgb$pred)
cat("\nMacro-F1 (pooled): RF =", rf_macro_f1, " | XGB =", xgb_macro_f1, "\n")

per_class_recall <- function(cmn) data.table(classe = rownames(cmn), recall = round(diag(cmn), 3))
ba_rf_per_class  <- per_class_recall(cm_rf$cmn)
ba_xgb_per_class <- per_class_recall(cm_xgb$cmn)
print(ba_rf_per_class)
print(ba_xgb_per_class)

# ---------------------------
# [MOD] Threshold tuning post-hoc su D/F/G/H
# ---------------------------
threshold_tune <- function(pred, th_vec) {
  stopifnot("prob" %in% pred$predict_types)  # servono le probabilità
  pr2 <- pred$clone(deep = TRUE)
  pr2$set_threshold(th_vec)   # soglie per classe: <1 favorisce la classe
  cm  <- table(pr2$truth, pr2$response)
  cmn <- sweep(cm, 1, pmax(rowSums(cm), 1), "/")
  list(pred = pr2, cm = cm, cmn = cmn)
}

rf_th  <- threshold_tune(cm_rf$pred,  thresh_vec)
xgb_th <- threshold_tune(cm_xgb$pred, thresh_vec)

cat("\n-- RF CM (dopo threshold tuning) --\n");  print(addmargins(rf_th$cm))
cat("\n-- XGB CM (dopo threshold tuning) --\n"); print(addmargins(xgb_th$cm))

cat("\n-- Recall per classe (RF dopo soglie) --\n");  print(per_class_recall(rf_th$cmn))
cat("\n-- Recall per classe (XGB dopo soglie) --\n"); print(per_class_recall(xgb_th$cmn))

# ---------------------------
# 9) Salvataggi vari
# ---------------------------
saveRDS(at_rf,  file = paste0(datapath,"rf.Rds"))
saveRDS(at_xgb, file = paste0(datapath,"xgb.Rds"))

at_rf$train(task)
at_xgb$train(task)
saveRDS(at_rf$learner,  file = paste0(datapath,"rf_learner.Rds"))
saveRDS(at_xgb$learner, file = paste0(datapath,"xgb_learner.Rds"))

save.image(file = paste0(datapath,"run_60_80_wrecall_thresh.RData"))  # [MOD]

# Confusion matrix stile caret (opzionale)
library(caret)
confusionMatrix(cm_rf$cm)
addmargins(cm_rf$cm)
confusionMatrix(cm_xgb$cm)
addmargins(cm_xgb$cm)
