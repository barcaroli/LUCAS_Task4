#--------------------------------------------------------------------------------
# Script to produce land cover estimates graphs
# Input: 1. land cover estimates
# Output: 1. A–H graphs: sample vs master (2018–2022) + obs 2018/2022
#------------------------------------------------------------------------------
setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/ALPHAEARTH/workflow")

library(tidyverse)
library(scales)

#-------------------------------------
# Master predictions with rf
# df <- read.csv("estimates_rf.csv")
# out <- "Estimates 2018-2022 (rf).pdf"
#-------------------------------------
# Master predictions with xgb
df <- read.csv("estimates_xgb.csv")
out <- "Estimates 2018-2022 (xgb).pdf"
#-------------------------------------

# DW_estimates <- read.csv("DW_estimates.csv")
# df <- rbind(df,DW_estimates)
rownames(df) <- df$estimate
df$estimate <- NULL
df

# --- Parametri ---
years        <- 2018:2022
classes      <- intersect(colnames(df), LETTERS[1:8])   # usa solo A..H
y0_cutoff    <- 0.05   # se tutti i valori > 5%, l'asse Y non parte da 0
min_abs_pad  <- 0.0005 # padding minimo assoluto = 0.05%
rel_pad_frac <- 0.15   # padding relativo = 15% dell'intervallo dei dati

make_plot <- function(df, classe) {
  # assicura righe attese (se mancano → NA)
  req_rows <- c(paste0("sample_pred", years),
                paste0("master_pred", years), "obs2018", "obs2022")
  missing_rows <- setdiff(req_rows, rownames(df))
  if (length(missing_rows) > 0) {
    add <- matrix(NA_real_, nrow = length(missing_rows), ncol = ncol(df),
                  dimnames = list(missing_rows, colnames(df)))
    df <- rbind(df, add)
  }
  
  sample_vals <- sapply(years, function(y) as.numeric(df[paste0("sample_pred", y), classe]))
  master_vals <- sapply(years, function(y) as.numeric(df[paste0("master_pred", y), classe]))
  obs18 <- as.numeric(df["obs2018", classe])
  obs22 <- as.numeric(df["obs2022", classe])
  
  if (all(is.na(c(sample_vals, master_vals, obs18, obs22)))) return(NULL)
  
  ts_df <- tibble(
    year   = rep(years, 2),
    value  = c(sample_vals, master_vals),
    series = factor(rep(c("Sample (2018–2022)", "Master (2018–2022)"), each = length(years)),
                    levels = c("Sample (2018–2022)", "Master (2018–2022)", "Obs 2018", "Obs 2022"))
  )
  obs_df <- tibble(
    series = factor(c("Obs 2018", "Obs 2022"), levels = levels(ts_df$series)),
    y = c(obs18, obs22)
  )
  
  # --- Limiti Y dinamici (lower non da 0 se i valori sono "alti") ---
  # --- Limiti Y dinamici: sempre vicino al minimo, mai sotto 0 ---
  y_all <- c(sample_vals, master_vals, obs18, obs22)
  y_all <- y_all[is.finite(y_all)]
  span  <- diff(range(y_all))
  base  <- ifelse(span > 0, span, max(y_all, 0))
  pad   <- max(min_abs_pad, rel_pad_frac * base)
  # y_lower <- 0 
  y_lower <- max(0, min(y_all) - 10 * pad)   # niente più soglia che riporta a 0
  y_upper <- min(1, max(y_all) + pad)
  y_lim   <- c(y_lower, y_upper)
  
  
  col_map <- c(
    "Sample (2018–2022)" = "blue",
    "Master (2018–2022)" = "red",
    "Obs 2018"           = "black",
    "Obs 2022"           = "magenta"
  )
  lty_map <- c(
    "Sample (2018–2022)" = "solid",
    "Master (2018–2022)" = "solid",
    "Obs 2018"           = "dashed",
    "Obs 2022"           = "dashed"
  )
  
  ggplot() +
    geom_hline(data = obs_df,
               aes(yintercept = y, color = series, linetype = series),
               linewidth = 0.9, na.rm = TRUE) +
    geom_line(data = ts_df,
              aes(x = year, y = value, color = series, linetype = series),
              linewidth = 1.2, na.rm = TRUE) +
    geom_point(data = ts_df,
               aes(x = year, y = value, color = series),
               size = 2.2, na.rm = TRUE) +
    # Etichette con 2 decimali
    geom_text(data = ts_df,
              aes(x = year, y = value,
                  label = percent(value, accuracy = 0.01), color = series),
              vjust = -0.8, size = 3, show.legend = FALSE, na.rm = TRUE) +
    scale_x_continuous(breaks = years, minor_breaks = NULL) +
    # Asse Y con 2 decimali
    scale_y_continuous(labels = percent_format(accuracy = 0.01), limits = y_lim) +
    scale_color_manual(values = col_map, drop = FALSE) +
    scale_linetype_manual(values = lty_map, drop = FALSE) +
    labs(
      title = paste0("Class: ", classe, " — Sample vs Master (2018–2022) + Obs 2018/2022"),
      # Sottotitolo con 2 decimali
      subtitle = paste0(
        "Obs2018 = ", ifelse(is.na(obs18), "NA", percent(obs18, 0.01)),
        "   •   Obs2022 = ", ifelse(is.na(obs22), "NA", percent(obs22, 0.01))
      ),
      x = "Year", y = "Percentage", color = NULL, linetype = NULL
    ) +
    theme_minimal(base_size = 13) +
    theme(legend.position = "bottom")
}

# --- stampa a video (A → H) ---
for (cl in classes) {
  p <- make_plot(df, cl)
  if (!is.null(p)) print(p)
}

# --- (Opzionale) PDF multipagina ---
pdf(out, width = 8.5, height = 5.6)
for (cl in classes) {
  p <- make_plot(df, cl)
  if (!is.null(p)) print(p)
}
dev.off()
