# library(tidyverse)
library(caret)
library(data.table)
library(dtw)
library(tidyr)
library(dplyr)
library(ggplot2)

datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"

df <- fread(paste0(datapath,"S2_S1_Italy_2018_trajectories_master.csv"))
preProc_vals <- preProcess(
  x      = df,
  method = "medianImpute"
)
df <- predict(preProc_vals, df)
ndvi_cols <- c("NDVI_01","NDVI_02","NDVI_03","NDVI_04","NDVI_05","NDVI_06",
               "NDVI_07","NDVI_08","NDVI_09","NDVI_10","NDVI_11","NDVI_12")

traj <- read.csv("trajectories_2018.csv")
mean_A <- traj$mean_ndvi[traj$LC1=="A"]
mean_B <- traj$mean_ndvi[traj$LC1=="B"]
mean_C <- traj$mean_ndvi[traj$LC1=="C"]
mean_D <- traj$mean_ndvi[traj$LC1=="D"]
mean_E <- traj$mean_ndvi[traj$LC1=="E"]
mean_F <- traj$mean_ndvi[traj$LC1=="F"]
mean_G <- traj$mean_ndvi[traj$LC1=="G"]
mean_H <- traj$mean_ndvi[traj$LC1=="H"]


dtw_NDVI <- as.data.frame(list(POINT_ID=df$POINT_ID,
                               dtw_NDVI_A=rep(NA,nrow(df)),
                               dtw_NDVI_B=rep(NA,nrow(df)),
                               dtw_NDVI_C=rep(NA,nrow(df)),
                               dtw_NDVI_D=rep(NA,nrow(df)),
                               dtw_NDVI_E=rep(NA,nrow(df)),
                               dtw_NDVI_F=rep(NA,nrow(df)),
                               dtw_NDVI_G=rep(NA,nrow(df)),
                               dtw_NDVI_H=rep(NA,nrow(df))))
for (i in c(1:nrow(df))) {
  cat("\n",i)
  ndvi <- as.numeric(df[i,c("NDVI_01","NDVI_02","NDVI_03","NDVI_04","NDVI_05","NDVI_06",
                 "NDVI_07","NDVI_08","NDVI_09","NDVI_10","NDVI_11","NDVI_12")])
  dtw_NDVI$dtw_NDVI_A[i] <- dtw(ndvi, mean_A)$distance
  dtw_NDVI$dtw_NDVI_B[i] <- dtw(ndvi, mean_B)$distance
  dtw_NDVI$dtw_NDVI_C[i] <- dtw(ndvi, mean_C)$distance
  dtw_NDVI$dtw_NDVI_D[i] <- dtw(ndvi, mean_D)$distance
  dtw_NDVI$dtw_NDVI_E[i] <- dtw(ndvi, mean_E)$distance
  dtw_NDVI$dtw_NDVI_F[i] <- dtw(ndvi, mean_F)$distance
  dtw_NDVI$dtw_NDVI_G[i] <- dtw(ndvi, mean_G)$distance
  dtw_NDVI$dtw_NDVI_H[i] <- dtw(ndvi, mean_H)$distance
}

a <- merge(df,dtw_NDVI,by="POINT_ID")
write.table(a,paste0(datapath,"S2_S1_Italy_2018_trajectories_dwt_master.csv"),sep=",",quote=F,row.names=F)


s <- fread(paste0(datapath,"S2_S1_Italy_2018_trajectories_sample.csv"))
b <- merge(df,dtw_NDVI,by="POINT_ID")
s <- fread(paste0(datapath,"D:/Google Drive/LUCAS Copernicus/EarthEngine/data/Survey_2018_cal_wgt.txt"))
b <- merge(b,s[,c("POINT_ID","land_cover")],by="POINT_ID")
b$LC1 <- substr(b$land_cover,1,1)
b$land_cover <- NULL
write.table(b,paste0(datapath,"S2_S1_Italy_2018_trajectories_dwt_sample.csv"),sep=",",quote=F,row.names=F)

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

