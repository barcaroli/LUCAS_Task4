library(tidyverse)
df <- read.csv("S2_S1_Italy_2018_trajectories_sample.csv",
               stringsAsFactors = FALSE)
# library(caret)
# preProc_vals <- preProcess(
#   x      = df[, all_feats],
#   method = "medianImpute"
# )
# df <- predict(preProc_vals, df[, c("POINT_ID","LC1",all_feats)])
# df <- df[df$LC1 == "H",]

# 2) Definizione delle colonne NDVI
ndvi_cols <- sprintf("NDVI_%02d", 1:12)

# 3) Calcola le traiettorie medie mensili di NDVI per ciascuna classe LC1
means_by_lc1 <- df %>%
  select(LC1, all_of(ndvi_cols)) %>%
  group_by(LC1) %>%
  summarize(across(all_of(ndvi_cols), ~ mean(.x, na.rm = TRUE))) %>%
  ungroup()

means_by_lc1[means_by_lc1$LC1=="H",]

# H <- df[df$LC1=="C",]

jm <- function(x, m) {
  v <- !is.na(x)
  x <- x[v]
  m <- m[v]
  B <- sum((x - m)^2) / 8
  sqrt(2 * (1 - exp(-B)))
}
df_new <- NULL
for (j in c(1:nrow(df))) {
  cat("\n ",j)
  x <- df[j,grep("NDVI",names(df))]
  jm_res <- rep(NA,8)
  for (i in c(1:8)) {
    m <- means_by_lc1[i,c(2:13)]
    jm_res[i] <- jm(x,m)
  }
  df_new <- rbind(df_new,cbind(df[j,],t(jm_res)))
}

jm_cols <- paste0("JM_NDVI_",LETTERS[1:8])
colnames(df_new)[39:46] <- jm_cols

df <- df_new

df_long <- df %>%
  pivot_longer(
    cols      = all_of(jm_cols),
    names_to  = "target_class",
    values_to = "JM"
  ) %>%
  mutate(
    # pulisco il nome, da "JM_LC1_A" a "A"
    target_class = str_remove(target_class, "^JM_LC1_")
  )

# 3) Recupero i livelli di LC1 presenti
lc1_levels <- sort(unique(df_long$LC1))

# 4) Loop sui livelli e stampa un plot per ciascuno
for (lc in lc1_levels) {
  p <- df_long %>%
    filter(LC1 == lc) %>%
    ggplot(aes(x = target_class, y = JM)) +
    geom_boxplot() +
    labs(
      title = paste0("Distribuzione JM (classi target) per LC1 = ", lc),
      x     = "Classe di destinazione",
      y     = "Jeffries–Matusita distance"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  print(p)
}

write.table(df,"jm_NDVI.csv",sep=",",quote=F,row.names=F)
