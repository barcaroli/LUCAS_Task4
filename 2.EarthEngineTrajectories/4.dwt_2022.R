# library(tidyverse)
library(caret)
library(data.table)
library(dtw)
library(tidyr)
library(dplyr)
library(ggplot2)

datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"

df <- fread(paste0(datapath,"S2_S1_Italy_2022_trajectories_master.csv"))
preProc_vals <- preProcess(
  x      = df,
  method = "medianImpute"
)
df <- predict(preProc_vals, df)

# traj <- read.csv(paste0(datapath,"trajectories_2022.csv"))
traj <- read.csv(paste0(datapath,"trajectories_2018.csv"))
NDVI_mean_A <- traj$mean_ndvi[traj$LC1=="A"]
NDVI_mean_B <- traj$mean_ndvi[traj$LC1=="B"]
NDVI_mean_C <- traj$mean_ndvi[traj$LC1=="C"]
NDVI_mean_D <- traj$mean_ndvi[traj$LC1=="D"]
NDVI_mean_E <- traj$mean_ndvi[traj$LC1=="E"]
NDVI_mean_F <- traj$mean_ndvi[traj$LC1=="F"]
NDVI_mean_G <- traj$mean_ndvi[traj$LC1=="G"]
NDVI_mean_H <- traj$mean_ndvi[traj$LC1=="H"]
VV_mean_A <- traj$vv.mean_vv[traj$LC1=="A"]
VV_mean_B <- traj$vv.mean_vv[traj$LC1=="B"]
VV_mean_C <- traj$vv.mean_vv[traj$LC1=="C"]
VV_mean_D <- traj$vv.mean_vv[traj$LC1=="D"]
VV_mean_E <- traj$vv.mean_vv[traj$LC1=="E"]
VV_mean_F <- traj$vv.mean_vv[traj$LC1=="F"]
VV_mean_G <- traj$vv.mean_vv[traj$LC1=="G"]
VV_mean_H <- traj$vv.mean_vv[traj$LC1=="H"]
VH_mean_A <- traj$vh.mean_vh[traj$LC1=="A"]
VH_mean_B <- traj$vh.mean_vh[traj$LC1=="B"]
VH_mean_C <- traj$vh.mean_vh[traj$LC1=="C"]
VH_mean_D <- traj$vh.mean_vh[traj$LC1=="D"]
VH_mean_E <- traj$vh.mean_vh[traj$LC1=="E"]
VH_mean_F <- traj$vh.mean_vh[traj$LC1=="F"]
VH_mean_G <- traj$vh.mean_vh[traj$LC1=="G"]
VH_mean_H <- traj$vh.mean_vh[traj$LC1=="H"]



dtw <- as.data.frame(list(POINT_ID=df$POINT_ID,
                               dtw_NDVI_A=rep(NA,nrow(df)),
                               dtw_NDVI_B=rep(NA,nrow(df)),
                               dtw_NDVI_C=rep(NA,nrow(df)),
                               dtw_NDVI_D=rep(NA,nrow(df)),
                               dtw_NDVI_E=rep(NA,nrow(df)),
                               dtw_NDVI_F=rep(NA,nrow(df)),
                               dtw_NDVI_G=rep(NA,nrow(df)),
                               dtw_NDVI_H=rep(NA,nrow(df)),
                               dtw_VV_A=rep(NA,nrow(df)),
                               dtw_VV_B=rep(NA,nrow(df)),
                               dtw_VV_C=rep(NA,nrow(df)),
                               dtw_VV_D=rep(NA,nrow(df)),
                               dtw_VV_E=rep(NA,nrow(df)),
                               dtw_VV_F=rep(NA,nrow(df)),
                               dtw_VV_G=rep(NA,nrow(df)),
                               dtw_VV_H=rep(NA,nrow(df)),
                               dtw_VH_A=rep(NA,nrow(df)),
                               dtw_VH_B=rep(NA,nrow(df)),
                               dtw_VH_C=rep(NA,nrow(df)),
                               dtw_VH_D=rep(NA,nrow(df)),
                               dtw_VH_E=rep(NA,nrow(df)),
                               dtw_VH_F=rep(NA,nrow(df)),
                               dtw_VH_G=rep(NA,nrow(df)),
                               dtw_VH_H=rep(NA,nrow(df))))
for (i in c(1:nrow(df))) {
  cat("\n",i)
  ndvi <- as.numeric(df[i,c("NDVI_01","NDVI_02","NDVI_03","NDVI_04","NDVI_05","NDVI_06",
                 "NDVI_07","NDVI_08","NDVI_09","NDVI_10","NDVI_11","NDVI_12")])
  VV <- as.numeric(df[i,c("VV_01","VV_02","VV_03","VV_04","VV_05","VV_06",
                            "VV_07","VV_08","VV_09","VV_10","VV_11","VV_12")])
  VH <- as.numeric(df[i,c("VH_01","VH_02","VH_03","VH_04","VH_05","VH_06",
                          "VH_07","VH_08","VH_09","VH_10","VH_11","VH_12")])
  dtw$dtw_NDVI_A[i] <- dtw(ndvi, NDVI_mean_A)$distance
  dtw$dtw_NDVI_B[i] <- dtw(ndvi, NDVI_mean_B)$distance
  dtw$dtw_NDVI_C[i] <- dtw(ndvi, NDVI_mean_C)$distance
  dtw$dtw_NDVI_D[i] <- dtw(ndvi, NDVI_mean_D)$distance
  dtw$dtw_NDVI_E[i] <- dtw(ndvi, NDVI_mean_E)$distance
  dtw$dtw_NDVI_F[i] <- dtw(ndvi, NDVI_mean_F)$distance
  dtw$dtw_NDVI_G[i] <- dtw(ndvi, NDVI_mean_G)$distance
  dtw$dtw_NDVI_H[i] <- dtw(ndvi, NDVI_mean_H)$distance
  dtw$dtw_VV_A[i] <- dtw(VV, VV_mean_A)$distance
  dtw$dtw_VV_B[i] <- dtw(VV, VV_mean_B)$distance
  dtw$dtw_VV_C[i] <- dtw(VV, VV_mean_C)$distance
  dtw$dtw_VV_D[i] <- dtw(VV, VV_mean_D)$distance
  dtw$dtw_VV_E[i] <- dtw(VV, VV_mean_E)$distance
  dtw$dtw_VV_F[i] <- dtw(VV, VV_mean_F)$distance
  dtw$dtw_VV_G[i] <- dtw(VV, VV_mean_G)$distance
  dtw$dtw_VV_H[i] <- dtw(VV, VV_mean_H)$distance
  dtw$dtw_VH_A[i] <- dtw(VH, VH_mean_A)$distance
  dtw$dtw_VH_B[i] <- dtw(VH, VH_mean_B)$distance
  dtw$dtw_VH_C[i] <- dtw(VH, VH_mean_C)$distance
  dtw$dtw_VH_D[i] <- dtw(VH, VH_mean_D)$distance
  dtw$dtw_VH_E[i] <- dtw(VH, VH_mean_E)$distance
  dtw$dtw_VH_F[i] <- dtw(VH, VH_mean_F)$distance
  dtw$dtw_VH_G[i] <- dtw(VH, VH_mean_G)$distance
  dtw$dtw_VH_H[i] <- dtw(VH, VH_mean_H)$distance
}

a <- merge(df,dtw,by="POINT_ID")
# write.table(a,paste0(datapath,"S2_S1_Italy_2022_trajectories_dwt_master.csv"),sep=",",quote=F,row.names=F)
write.table(a,paste0(datapath,"S2_S1_Italy_2022_trajectories_dwt_master.csv"),sep=",",quote=F,row.names=F)


s <- fread(paste0(datapath,"S2_S1_Italy_2022_trajectories_sample.csv"))
b <- merge(df,dtw,by="POINT_ID")
s2022 <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
b <- merge(b,s2022[,c("POINT_ID","SURVEY_LC1")],by="POINT_ID")
b$LC1 <- substr(b$SURVEY_LC1,1,1)
b$SURVEY_LC1 <- NULL
write.table(b,paste0(datapath,"S2_S1_Italy_2022_trajectories_dwt_sample_means2018.csv"),sep=",",quote=F,row.names=F)

df_long <- b %>%
  pivot_longer(
    cols = starts_with("dtw_NDVI_"),
    names_to = "classe_riferimento",
    values_to = "distanza"
  ) %>%
  mutate(
    classe_riferimento = gsub("dtw_NDVI_", "", classe_riferimento),
    classe_riferimento = factor(classe_riferimento, levels = LETTERS[1:8])
  )

# Boxplot
ggplot(df_long, aes(x = classe_riferimento, y = distanza)) +
  geom_boxplot() +
  labs(
    title = "Distribuzione delle distanze DTW rispetto alle traiettorie medie NDVI",
    x = "Classe di riferimento",
    y = "Distanza DTW"
  ) +
  theme_minimal()

df_long <- b %>%
  pivot_longer(
    cols = starts_with("dtw_VV_"),
    names_to = "classe_riferimento",
    values_to = "distanza"
  ) %>%
  mutate(
    classe_riferimento = gsub("dtw_VV_", "", classe_riferimento),
    classe_riferimento = factor(classe_riferimento, levels = LETTERS[1:8])
  )

# Boxplot
ggplot(df_long, aes(x = classe_riferimento, y = distanza)) +
  geom_boxplot() +
  labs(
    title = "Distribuzione delle distanze DTW rispetto alle traiettorie medie VV",
    x = "Classe di riferimento",
    y = "Distanza DTW"
  ) +
  theme_minimal()

df_long <- b %>%
  pivot_longer(
    cols = starts_with("dtw_VH_"),
    names_to = "classe_riferimento",
    values_to = "distanza"
  ) %>%
  mutate(
    classe_riferimento = gsub("dtw_VH_", "", classe_riferimento),
    classe_riferimento = factor(classe_riferimento, levels = LETTERS[1:8])
  )

# Boxplot
ggplot(df_long, aes(x = classe_riferimento, y = distanza)) +
  geom_boxplot() +
  labs(
    title = "Distribuzione delle distanze DTW rispetto alle traiettorie medie VH",
    x = "Classe di riferimento",
    y = "Distanza DTW"
  ) +
  theme_minimal()

