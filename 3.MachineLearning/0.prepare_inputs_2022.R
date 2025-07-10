datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
# Prepare master 
library(data.table)
load(paste0(datapath,"Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP.Rdata"))
m <- Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP[Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP$NUTS0_21=="IT",]
rm(Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP)
# Read 2022 sample
s <- fread(paste0(datapath,"Survey_2022_cal_wgt_2nd_phase.txt"))
s <- s[s$POINT_NUTS0 == "IT",]
s$LC1 <- as.factor(substr(s$SURVEY_LC1,1,1))
# s$LC1 <- as.factor(substr(s$SURVEY_LC1,1,1)) # Only 2022
# Read first download with CLCM, VV, VH
glcm <- read.csv(paste0(datapath,"S2_S1_GLCM_Italy_2022.csv"))
glcm$NDVI <- (glcm$B8-glcm$B4) / (glcm$B8+glcm$B4)
glcm$NDWI <- (glcm$B8-glcm$B3) / (glcm$B8+glcm$B3)
glcm$EVI <- (glcm$B8-glcm$B4) / 2.5 * (glcm$B8 + 6*glcm$B4 - 7.5*glcm$B2 + 0.5)
glcm$SAVI <- (glcm$B8-glcm$B4) / (glcm$B8 + glcm$B4 + 0.5)
glcm$NDI45 <- (glcm$B5-glcm$B4) / (glcm$B5+glcm$B4)
glcm$MCARI <- ((glcm$B5 - glcm$B4) - 0.2*(glcm$B5 - glcm$B3)) * (glcm$B5-glcm$B4)
glcm$GNDVI <- (glcm$B3-glcm$B4) / (glcm$B3+glcm$B4)
# glcm$MSAVI <- ?
glcm$PSSR <- glcm$B7 / glcm$B4
glcm$IRECI <- (glcm$B7-glcm$B4) / (glcm$B5/glcm$B6)
# Read second download with trajectories
traj <- read.csv(paste0(datapath,"S2_S1_Italy_2022_trajectories_dwt_master.csv"))
# traj <- read.csv(paste0(datapath,"S2_S1_Italy_2022_trajectories_dwt_master_means2018.csv"))

italy <- m[,c("POINT_ID","ELEV_DEM","aspect_slope","aspect_exposure")]
italy <- merge(italy,glcm,by="POINT_ID")
italy <- merge(italy,traj,by="POINT_ID")

write.table(italy,paste0(datapath,"Italy_master_2022.csv"),sep=",",quote=F,row.names=F)

sample <- merge(italy,s[,c("POINT_ID","LC1")])
write.table(sample,paste0(datapath,"Italy_sample_2022.csv"),sep=",",quote=F,row.names=F)

# exclude <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12",
#              "EVI","MCARI","VH_lin_corr","VV_lin_corr","MCARI","PSSR","IRECI")

include <- c("POINT_ID","ELEV_DEM","aspect_slope","aspect_exposure",
             "dtw_NDVI_A","dtw_NDVI_B","dtw_NDVI_C","dtw_NDVI_D","dtw_NDVI_E","dtw_NDVI_F","dtw_NDVI_G","dtw_NDVI_H",
             "dtw_VV_A","dtw_VV_B","dtw_VV_C","dtw_VV_D","dtw_VV_E","dtw_VV_F","dtw_VV_G","dtw_VV_H",  
             "dtw_VH_A","dtw_VH_B","dtw_VH_C","dtw_VH_D","dtw_VH_E","dtw_VH_F","dtw_VH_G","dtw_VH_H",
             "VH_lin_savg","VH_lin_var","VV_lin_savg","VV_lin_var")

# italy_reduced <- italy[,!names(italy) %in% exclude]
italy_reduced <- italy[,names(italy) %in% include]

write.table(italy_reduced,paste0(datapath,"Italy_master_2022_reduced.csv"),sep=",",quote=F,row.names=F)

sample_reduced <- merge(italy_reduced,s[,c("POINT_ID","LC1")],by="POINT_ID")
write.table(sample_reduced,paste0(datapath,"Italy_sample_2022_reduced.csv"),sep=",",quote=F,row.names=F)

# Expand of the sample to the whole master
sample_reduced <- merge(sample_reduced,s[,c("POINT_ID","wgt_2nd_phase","point_area")],by="POINT_ID")
sum(sample_reduced$wgt_2nd_phase/sample_reduced$point_area)
sample_reduced_exp <- sample_reduced[rep(seq_len(nrow(sample_reduced)), round(sample_reduced$wgt_2nd_phase/sample_reduced$point_area)),]
nrow(sample_reduced_exp)
round(table(sample$LC1)/nrow(sample),4)*100
round(table(sample_reduced_exp$LC1)/nrow(sample_reduced_exp),4)*100
sample_reduced_exp$wgt_2nd_phase <- sample_reduced_exp$point_area <- NULL
write.table(sample_reduced_exp,paste0(datapath,"Italy_sample_2022_reduced_exp.csv"),sep=",",quote=F,row.names=F)

