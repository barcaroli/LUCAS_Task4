# ============================================================
# Sentinel data (221 variables) models fitting — FIXED
# ============================================================
n_evals_rf  <- 2
n_evals_xgb <- 3

req <- c("mlr3verse","mlr3learners","mlr3pipelines","mlr3tuning",
         "paradox","data.table","ggplot2","ranger","xgboost","R6")
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
  library(R6)
})

# ---------------------------
# 0) Parametri
# ---------------------------
datapath    <- "D:/Google Drive/LUCAS Copernicus/EarthEngine/SENTINEL/workflow/data/"
pattern     <- "\\Italy_sample_2018_reduced_aux_completo_rain_fourier.csv"
target_col  <- "LC1"
id_col      <- "POINT_ID"
fold_col    <- "cv_fold"
nfolds      <- 5
seed        <- 42

# Classi da favorire
favored <- c("D","F","G","H")

# Pesi recall (tuning)
w_recall_vec <- setNames(rep(1, 8), LETTERS[1:8]); w_recall_vec[favored] <- 3

# Soglie post-hoc “semplici” (<=1 favoriscono la classe)
thresh_vec <- setNames(rep(1, 8), LETTERS[1:8]); thresh_vec[favored] <- 0.7

# ---------------------------
# 1) Caricamento & pulizia
# ---------------------------
fs <- list.files(datapath, pattern = pattern, full.names = TRUE)
stopifnot("Nessun file CSV trovato" = length(fs) > 0)
X  <- rbindlist(lapply(fs, fread), use.names = TRUE, fill = TRUE)

# LC1 come fattore con livelli A..H
X[[target_col]] <- factor(X[[target_col]], levels = LETTERS[1:8])

fa <- names(X)[vapply(X, is.factor, logical(1))]
ch <- names(X)[vapply(X, is.character, logical(1))]
for (cc in ch) X[[cc]] <- factor(X[[cc]])

# de-dup su ID se presente
if (id_col %in% names(X)) X <- X[!duplicated(X[[id_col]]), ]

# Definisci colonne non-feature (le teniamo in X per eventuale CV custom)
ID_COLS      <- c("POINT_ID")
LOC_COLS     <- c("NUTS2_16")             # tipicamente character
W            <- c("cal_wgt", "cal_wgt.x", "cal_wgt.y", "wgt_2nd_phase")
COORD        <- c("lon","lat")
FOLD         <- c("cv_fold")

NON_FEATURE  <- unique(c(ID_COLS, W, COORD, FOLD, target_col))

# Feature = tutto tranne target e NON_FEATURE
feat_cols <- setdiff(names(X), NON_FEATURE)

cat("\n--- Distribuzione LC1 ---\n")
print(round(prop.table(table(X[[target_col]])), 4))

# ---------------------------
# 2) Task & Resampling
# ---------------------------
set.seed(seed)
task <- TaskClassif$new("lc1_ae", backend = X, target = target_col)

# seleziona solo le feature per il modello
task$select(feat_cols)

if (fold_col %in% names(X)) {
  folds <- as.integer(X[[fold_col]])
  K     <- max(folds, na.rm = TRUE)
  idx   <- split(seq_len(nrow(X)), folds)
  train_sets <- lapply(seq_len(K), function(k) unlist(idx[setdiff(seq_len(K), k)]))
  test_sets  <- lapply(seq_len(K), function(k) unlist(idx[k]))
  rsmp_use <- rsmp("custom"); rsmp_use$instantiate(task, train = train_sets, test = test_sets)
  message(sprintf("Usata CV custom (%d fold) da '%s'.", K, fold_col))
} else {
  rsmp_use <- rsmp("cv", folds = nfolds); rsmp_use$instantiate(task)
  message(sprintf("Usata CV stratificata a %d fold.", nfolds))
}

# ---------------------------
# 3) Metrica custom: Weighted Recall (con .score)
# ---------------------------
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
                                           properties = character(0),
                                           predict_type = "response"
                                         )
                                       }
                                     ),
                                     private = list(
                                       .score = function(prediction, ...) {
                                         cm  <- table(prediction$truth, prediction$response)
                                         rec <- diag(cm) / pmax(rowSums(cm), 1)
                                         cls <- rownames(cm)
                                         w   <- self$weights[cls]; w[is.na(w)] <- 1
                                         sum(rec * w) / sum(w)
                                       }
                                     )
)
msr_wrecall <- MeasureClassifWRecall$new(w_recall_vec)

# ---------------------------
# 4) Learners + Tuning space
# ---------------------------

# ---- RF (ranger) con class weights ----
tab <- table(task$truth()); p <- as.numeric(tab) / sum(tab)
alpha <- 1.6
base_w <- (1 / p) ^ alpha; names(base_w) <- names(tab)
boost  <- setNames(rep(1, length(tab)), names(tab)); boost[favored] <- 1.5
w_rf   <- (base_w * boost) / mean(base_w * boost)

lrn_rf <- lrn("classif.ranger",
              num.trees = 600,
              importance = "impurity",
              class.weights = w_rf,
              respect.unordered.factors = "order",
              seed = seed,
              predict_type = "prob"
)

space_rf <- ps(
  mtry          = p_int(8, 48),
  min.node.size = p_int(1, 10),
  splitrule     = p_fct(c("gini", "extratrees"))
)

# Terminator esplicito
term_rf  <- bbotk::trm("evals"); term_rf$param_set$values$n_evals  <- as.integer(n_evals_rf)

at_rf <- AutoTuner$new(
  learner      = lrn_rf,
  resampling   = rsmp("holdout"),
  measure      = msr_wrecall,
  search_space = space_rf,
  terminator   = term_rf,
  tuner        = tnr("random_search")
)

# ---- XGBoost con oversampling + ENCODER ----
# Encoder one-hot (selezione tipi come vettore; fallback se 'character' non supportato)
po_enc <- po("encode", method = "one-hot")
ok <- TRUE
tryCatch({
  po_enc$param_set$values$affect_columns <- selector_type(c("factor","character","ordered"))
}, error = function(e) { ok <<- FALSE })
if (!ok || is.null(po_enc$param_set$values$affect_columns)) {
  po_enc$param_set$values$affect_columns <- selector_type(c("factor","ordered"))
}

po_cb  <- po("classbalancing", adjust = "minor", reference = "major", ratio = 1.3)

lrn_xgb <- lrn("classif.xgboost",
               objective   = "multi:softprob",
               eval_metric = "mlogloss",
               nthread     = max(1L, parallel::detectCores() - 1L),
               tree_method = "hist",
               booster     = "gbtree",
               predict_type = "prob"
)

graph_xgb <- po_enc %>>% po_cb %>>% lrn_xgb
gxl <- as_learner(graph_xgb)

space_xgb <- ps(
  classif.xgboost.eta              = p_dbl(0.03, 0.30),
  classif.xgboost.max_depth        = p_int(4, 12),
  classif.xgboost.min_child_weight = p_int(1, 8),
  classif.xgboost.subsample        = p_dbl(0.6, 1.0),
  classif.xgboost.colsample_bytree = p_dbl(0.6, 1.0),
  classif.xgboost.nrounds          = p_int(200, 1200)
)

term_xgb <- bbotk::trm("evals"); term_xgb$param_set$values$n_evals <- as.integer(n_evals_xgb)

at_xgb <- AutoTuner$new(
  learner      = gxl,
  resampling   = rsmp("holdout"),
  measure      = msr_wrecall,
  search_space = space_xgb,
  terminator   = term_xgb,
  tuner        = tnr("random_search")
)

# ---------------------------
# 5) Benchmark con CV finale
# ---------------------------
design <- benchmark_grid(
  tasks = task,
  learners = list(at_rf, at_xgb),
  resamplings = rsmp_use
)
bmr <- benchmark(design)

cat("\n--- Metriche CV (media ± sd) ---\n")
measures <- list(msr("classif.acc"), msr("classif.bacc"), msr_wrecall)
print(bmr$aggregate(measures))

save.image(file = file.path(datapath, "temp.RData"))

# ---------------------------
# 6) Predizioni CV → CM
# ---------------------------
get_cm_from_bmr <- function(bmr, learner_or_id) {
  desired_id <- if (inherits(learner_or_id, "Learner")) learner_or_id$id else as.character(learner_or_id)
  dt <- data.table::as.data.table(bmr)
  uh <- unique(dt[learner_id == desired_id, uhash])
  if (length(uh) == 0L) {
    avail <- unique(dt$learner_id)
    stop(sprintf("Nessuna riga per learner_id='%s'. Learner disponibili: %s",
                 desired_id, paste(avail, collapse = ", ")))
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

# Usa gli oggetti AutoTuner per evitare mismatch dell'id
cm_rf  <- get_cm_from_bmr(bmr, at_rf)
cm_xgb <- get_cm_from_bmr(bmr, at_xgb)

cat("\n-- RF CM (prima del threshold tuning) --\n");  print(addmargins(cm_rf$cm))
cat("\n-- XGB CM (prima del threshold tuning) --\n"); print(addmargins(cm_xgb$cm))

# ---------------------------
# 7) Heatmap ggplot della CM normalizzata
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
  ggplot(long, aes(x = Pred, y = Truth, fill = Val)) +
    geom_tile() +
    geom_text(aes(label = sprintf("%.2f", Val)), size = 3) +
    scale_fill_gradient(low = "white", high = "steelblue") +
    labs(title = title, x = "Predicted", y = "True") +
    theme_minimal()
}
print(plot_cm(cm_rf$cmn,  paste0(at_rf$id,  " — CM normalizzata")))
print(plot_cm(cm_xgb$cmn, paste0(at_xgb$id, " — CM normalizzata")))

# ---------------------------
# 8) Macro-F1 "pooled" + recall per classe
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
# 9) Threshold tuning post-hoc (semplice, con riscalatura in [0,1])
# ---------------------------
rescale_thresholds01 <- function(th) {
  d <- max(1, max(th, na.rm = TRUE))
  th / d
}
threshold_tune <- function(pred, th_vec) {
  if (is.null(pred$prob)) stop("Servono probabilità (predict_type='prob').")
  pr2 <- pred$clone(deep = TRUE)
  pr2$set_threshold(rescale_thresholds01(th_vec))
  cm  <- table(pr2$truth, pr2$response)
  cmn <- sweep(cm, 1, pmax(rowSums(cm), 1), "/")
  list(pred = pr2, cm = cm, cmn = cmn)
}
rf_th  <- threshold_tune(cm_rf$pred,  thresh_vec)
xgb_th <- threshold_tune(cm_xgb$pred, thresh_vec)

cat("\n-- RF CM (dopo soglie semplici) --\n");  print(addmargins(rf_th$cm))
cat("\n-- XGB CM (dopo soglie semplici) --\n"); print(addmargins(xgb_th$cm))
cat("\n-- Recall per classe (RF dopo soglie) --\n");  print(per_class_recall(rf_th$cmn))
cat("\n-- Recall per classe (XGB dopo soglie) --\n"); print(per_class_recall(xgb_th$cmn))

# ---------------------------
# 10) Equalizzazione dei margini (soglie “pronte mlr3”)
# ---------------------------
threshold_equalize_marginals <- function(pred,
                                         target = c("truth","custom"),
                                         target_vec = NULL,
                                         eta = 0.5, tol = 0.01, max_iter = 50,
                                         min_w = 1e-3, max_w = 1e3) {
  if (is.null(pred$prob)) stop("Servono probabilità (predict_type='prob').")
  probs   <- as.matrix(pred$prob)
  classes <- colnames(probs)
  truth   <- factor(pred$truth, levels = classes)
  N <- nrow(probs); K <- ncol(probs); eps <- 1e-9
  
  target <- match.arg(target)
  if (target == "truth") {
    T_counts <- as.numeric(table(truth)); names(T_counts) <- classes
  } else {
    stopifnot(!is.null(target_vec), length(target_vec) == K)
    if (abs(sum(target_vec) - 1) < 1e-6) T_counts <- as.numeric(target_vec) * N else T_counts <- as.numeric(target_vec)
    names(T_counts) <- classes
  }
  T_counts[T_counts < 1] <- 1
  
  w <- setNames(rep(1, K), classes)
  predict_with_w <- function(P, w) {
    scores <- sweep(P, 2, w, "*")
    idx <- max.col(scores, ties.method = "first")
    factor(classes[idx], levels = classes)
  }
  
  iters <- 0L
  repeat {
    iters <- iters + 1L
    pred_lab <- predict_with_w(probs, w)
    C_counts <- as.numeric(table(factor(pred_lab, levels = classes)))
    names(C_counts) <- classes
    rel_gap <- max(abs(C_counts - T_counts) / pmax(T_counts, 1))
    if (rel_gap <= tol || iters >= max_iter) break
    ratio <- (T_counts + eps) / (C_counts + eps)
    w <- pmin(pmax(w * (ratio ^ eta), min_w), max_w)
  }
  
  final_pred <- predict_with_w(probs, w)
  cm  <- table(truth, final_pred)
  cmn <- sweep(cm, 1, pmax(rowSums(cm), 1), "/")
  
  th_raw  <- setNames(as.numeric(mean(w) / w), classes)
  scale_den <- max(1, max(th_raw, na.rm = TRUE))
  th_mlr3 <- th_raw / scale_den
  th_mlr3[!is.finite(th_mlr3)] <- 1e-6
  
  list(weights = w,
       thresholds_raw  = th_raw,
       thresholds_mlr3 = th_mlr3,
       iterations = iters,
       response = final_pred,
       cm = cm, cmn = cmn)
}

eq_rf  <- threshold_equalize_marginals(cm_rf$pred,  target = "truth", eta = 0.6, tol = 0.01)
eq_xgb <- threshold_equalize_marginals(cm_xgb$pred, target = "truth", eta = 0.6, tol = 0.01)

p_rf_eq  <- cm_rf$pred$clone(deep=TRUE);  p_rf_eq$set_threshold(eq_rf$thresholds_mlr3)
p_xgb_eq <- cm_xgb$pred$clone(deep=TRUE); p_xgb_eq$set_threshold(eq_xgb$thresholds_mlr3)

cat("\n-- RF CM (dopo equalizzazione) --\n");  print(addmargins(table(p_rf_eq$truth,  p_rf_eq$response)))
cat("\n-- XGB CM (dopo equalizzazione) --\n"); print(addmargins(table(p_xgb_eq$truth, p_xgb_eq$response)))

# ---------------------------
# 11) Salvataggi
# ---------------------------
saveRDS(at_rf,  file = file.path(datapath, "rf.Rds"))
saveRDS(at_xgb, file = file.path(datapath, "xgb.Rds"))

at_rf$train(task)
at_xgb$train(task)
saveRDS(at_rf$learner,  file = file.path(datapath, "rf_learner.Rds"))
saveRDS(at_xgb$learner, file = file.path(datapath, "xgb_learner.Rds"))

save.image(file = file.path(datapath, "run_10_10_wrecall_thresh.RData"))

# Confusion matrix stile caret (opzionale)
suppressPackageStartupMessages(library(caret))
confusionMatrix(cm_rf$cm);  addmargins(cm_rf$cm)
confusionMatrix(cm_xgb$cm); addmargins(cm_xgb$cm)

# ---------------------------
# 12) Helpers per predire con soglie su nuovi dati
# ---------------------------
predict_with_thresholds <- function(learner, new_df, thresholds,
                                    return = c("labels", "prob", "both")) {
  return <- match.arg(return)
  learner$predict_type <- "prob"
  p <- learner$predict_newdata(new_df)
  cls_prob <- colnames(p$prob)
  if (!all(cls_prob %in% names(thresholds))) {
    stop("Le soglie non coprono tutte le classi di predizione: ",
         paste(setdiff(cls_prob, names(thresholds)), collapse = ", "))
  }
  th <- thresholds[cls_prob]
  d  <- max(1, max(th, na.rm = TRUE))
  th <- th / d
  th[!is.finite(th)] <- 1e-6
  p$set_threshold(th)
  if (return == "labels") return(p$response)
  if (return == "prob")   return(p$prob)
  list(labels = p$response, prob = p$prob)
}

make_thresholded_learner <- function(learner, thresholds, id = "th") {
  g  <- mlr3pipelines::as_graph(learner)$clone(deep = TRUE)
  g2 <- g %>>% po("threshold", id = id)
  l2 <- as_learner(g2)
  th <- thresholds
  d  <- max(1, max(th, na.rm = TRUE))
  th <- th / d
  th[!is.finite(th)] <- 1e-6
  l2$param_set$values[[sprintf("%s.threshold", id)]] <- th
  l2$predict_type <- "prob"
  l2
}

# Esempio (commentato):
# mod_xgb <- at_xgb$learner
# pred_labels_new <- predict_with_thresholds(mod_xgb, new_df, eq_xgb$thresholds_mlr3)
# mod_xgb_th <- make_thresholded_learner(mod_xgb, eq_xgb$thresholds_mlr3)
# pred_labels_new2 <- predict_with_thresholds(mod_xgb_th, new_df,
#                                             thresholds = setNames(rep(1, 8), LETTERS[1:8]))
