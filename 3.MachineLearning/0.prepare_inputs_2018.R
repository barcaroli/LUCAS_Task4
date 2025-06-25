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

sample <- merge(italy,s[,c("POINT_ID","LC1")])
write.table(sample,paste0(datapath,"Italy_sample_2018.csv"),sep=",",quote=F,row.names=F)
