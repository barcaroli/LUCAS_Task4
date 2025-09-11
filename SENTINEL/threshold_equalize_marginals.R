# Equalizza le frequenze marginali (colonne) alla distribuzione target (righe)
# pred: PredictionClassif mlr3 con pred$prob disponibile
# target: "truth" (default) usa le prevalenze osservate; oppure vettore custom (proporzioni o conteggi) nominato con le classi
# eta: fattore di smorzamento negli aggiornamenti (0<eta<=1)
# tol: tolleranza relativa sulle differenze tra conteggi target e ottenuti
# max_iter: iterazioni massime
threshold_equalize_marginals <- function(pred,
                                         target = c("truth", "custom"),
                                         target_vec = NULL,
                                         eta = 0.5,
                                         tol = 0.01,
                                         max_iter = 50,
                                         min_w = 1e-3,
                                         max_w = 1e3) {
  stopifnot(!is.null(pred$prob))
  probs   <- as.matrix(pred$prob)
  classes <- colnames(probs)
  truth   <- factor(pred$truth, levels = classes)
  N       <- nrow(probs); K <- ncol(probs)
  eps     <- 1e-9
  
  # --- target dei margini predetti (conteggi per classe) ---
  target <- match.arg(target)
  if (target == "truth") {
    T_counts <- as.numeric(table(truth))
    names(T_counts) <- classes
  } else {
    stopifnot(!is.null(target_vec), length(target_vec) == K)
    names(target_vec) <- if (is.null(names(target_vec))) classes else names(target_vec)
    # accetta proporzioni o conteggi
    if (abs(sum(target_vec) - 1) < 1e-6) {
      T_counts <- as.numeric(target_vec) * N
    } else {
      T_counts <- as.numeric(target_vec)
    }
    names(T_counts) <- classes
  }
  
  # evita target zero (classe assente) che bloccherebbe gli update
  T_counts[T_counts < 1] <- 1
  
  # --- pesi / moltiplicatori per classe (1 = neutro) ---
  w <- setNames(rep(1, K), classes)
  
  # helper per predire con pesi w (argmax dei punteggi pesati)
  predict_with_w <- function(P, w) {
    scores <- sweep(P, 2, w, "*")                  # moltiplica colonne per w_k
    idx    <- max.col(scores, ties.method = "first")
    factor(classes[idx], levels = classes)
  }
  
  iters <- 0L
  repeat {
    iters <- iters + 1L
    pred_lab <- predict_with_w(probs, w)
    C_counts <- as.numeric(table(factor(pred_lab, levels = classes)))
    names(C_counts) <- classes
    
    # differenza relativa massimo scostamento
    rel_gap <- max(abs(C_counts - T_counts) / pmax(T_counts, 1))
    if (rel_gap <= tol || iters >= max_iter) break
    
    # update moltiplicatori (se C<T aumenta w; se C>T diminuisce w)
    ratio <- (T_counts + eps) / (C_counts + eps)
    w <- w * (ratio ^ eta)
    # clip per stabilità numerica
    w <- pmin(pmax(w, min_w), max_w)
  }
  
  # predizione finale e confusion matrix
  final_pred <- predict_with_w(probs, w)
  cm  <- table(truth, final_pred)
  cmn <- sweep(cm, 1, pmax(rowSums(cm), 1), "/")
  
  # Soglie “equivalenti” per mlr3::PredictionClassif$set_threshold():
  # in molte implementazioni soglia più bassa ⇒ classe favorita.
  # Convertiamo i pesi in soglie normalizzate attorno a 1.
  th_vec <- setNames(as.numeric(mean(w) / w), classes)
  
  list(
    weights      = w,           # moltiplicatori per classe usati sulle probabilità
    thresholds   = th_vec,      # da passare a set_threshold (se vuoi usare l’API mlr3)
    iterations   = iters,
    target_counts   = T_counts,
    achieved_counts = C_counts,
    response     = final_pred,  # etichette predette equalizzate
    cm = cm, cmn = cmn
  )
}
