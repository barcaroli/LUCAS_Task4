datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
# Prepare master 
load(paste0(datapath,"Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP.Rdata"))
m <- Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP[Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP$NUTS0_21=="IT",]
rm(Master_HRL_CLC_NUTS21_N2K_DEH_BCK_DEM_EXP)
# Read 2018 sample
s <- read.delim(paste0(datapath,"Survey_2018_cal_wgt.txt"))
s <- s[s$NUTS0_16 == "IT",]
s$LC1 <- as.factor(substr(s$land_cover,1,1))
# s$LC1 <- as.factor(substr(s$SURVEY_LC1,1,1)) # Only 2022
# Read first download with CLCM, VV, VH
glcm <- read.csv(paste0(datapath,"S2_S1_GLCM_Italy_2018.csv"))
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
traj <- read.csv(paste0(datapath,"S2_S1_Italy_2018_trajectories_dwt_master.csv"))

italy <- m[,c("POINT_ID","ELEV_DEM","aspect_slope","aspect_exposure")]
italy <- merge(italy,glcm,by="POINT_ID")
italy <- merge(italy,traj,by="POINT_ID")

write.table(italy,paste0(datapath,"Italy_master_2018.csv"),sep=",",quote=F,row.names=F)

sample <- merge(italy,s[,c("POINT_ID","LC1")],by="POINT_ID")
write.table(sample,paste0(datapath,"Italy_sample_2018.csv"),sep=",",quote=F,row.names=F)

# exclude <- c("B1","B2","B3","B4","B5","B6","B7","B8","B8A","B9","B11","B12",
#              "EVI","MCARI","VH_lin_corr","VV_lin_corr","MCARI","PSSR","IRECI")

include <- c("POINT_ID","ELEV_DEM","aspect_slope","aspect_exposure",
             "dtw_NDVI_A","dtw_NDVI_B","dtw_NDVI_C","dtw_NDVI_D","dtw_NDVI_E","dtw_NDVI_F","dtw_NDVI_G","dtw_NDVI_H",
         "dtw_VV_A","dtw_VV_B","dtw_VV_C","dtw_VV_D","dtw_VV_E","dtw_VV_F","dtw_VV_G","dtw_VV_H",  
             "dtw_VH_A","dtw_VH_B","dtw_VH_C","dtw_VH_D","dtw_VH_E","dtw_VH_F","dtw_VH_G","dtw_VH_H",
         "VH_lin_savg","VH_lin_var","VV_lin_savg","VV_lin_var")

# italy_reduced <- italy[,!names(italy) %in% exclude]
italy_reduced <- italy[,names(italy) %in% include]

write.table(italy_reduced,paste0(datapath,"Italy_master_2018_reduced.csv"),sep=",",quote=F,row.names=F)

sample_reduced <- merge(italy_reduced,s[,c("POINT_ID","LC1")],by="POINT_ID")
write.table(sample_reduced,paste0(datapath,"Italy_sample_2018_reduced.csv"),sep=",",quote=F,row.names=F)
