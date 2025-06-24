setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/")
## ----setup, include=FALSE--------------------------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)
library(ReGenesees)
library(ggplot2)
library(reshape)
library(openxlsx)

lc_values <- c('water', 'trees', 'grass', 'flooded_vegetation', 'crops',
               'shrub_and_scrub', 'built', 'bare', 'snow_and_ice')
## --------------------------------------------------------------------------------------------
cop3 <- read.xlsx(".\\Italy2018\\Italy_2018-12-31_3.xlsx")
colnames(cop3)[2:10] <- lc_values
Italy3 <- aggregate(cbind(water,trees,grass,flooded_vegetation,crops,shrub_and_scrub,built,bare,snow_and_ice)
                   ~startDate,data=cop3,FUN=mean)
Italy3 <- colSums(Italy3[,c(2:10)])
Italy3 / sum(Italy3)

cop9 <- read.xlsx(".\\Italy2018\\Italy_2018-12-31_9.xlsx")
colnames(cop9)[2:10] <- lc_values
Italy9 <- aggregate(cbind(water,trees,grass,flooded_vegetation,crops,shrub_and_scrub,built,bare,snow_and_ice)
                    ~startDate,data=cop9,FUN=mean)
Italy9 <- colSums(Italy9[,c(2:10)])
Italy9 / sum(Italy9)

cop12 <- read.xlsx(".\\Italy2018\\Italy_2018-12-31_12.xlsx")
colnames(cop12)[2:10] <- lc_values
Italy12 <- aggregate(cbind(water,trees,grass,flooded_vegetation,crops,shrub_and_scrub,built,bare,snow_and_ice)
                    ~startDate,data=cop12,FUN=mean)
Italy12 <- colSums(Italy12[,c(2:10)])
Italy12 / sum(Italy12)

cop1 <- read.xlsx(".\\Italy2018\\Italy_2018-12-31_1.xlsx")
colnames(cop1)[2:10] <- lc_values
Italy1 <- aggregate(cbind(water,trees,grass,flooded_vegetation,crops,shrub_and_scrub,built,bare,snow_and_ice)
                     ~startDate,data=cop1,FUN=mean)
Italy1 <- colSums(Italy1[,c(2:10)])
Italy1 / sum(Italy1)

## --------------------------------------------------------------------------------------------
cop18 <- cop[1,]
copern <- NULL
copern$land_cover_cop <- lc_values
copern$counts <- as.numeric(cop18[,c(2:10)])
copern$freq <- round(copern$counts / sum(copern$counts),4)
copern <- as.data.frame(copern)
copern2 <- copern[c(7,5,2,6,3,8,1,4),]
copern2


## --------------------------------------------------------------------------------------------
load("LUCAS2018_sample.RData")
LUCAS_lazio <- LUCAS[LUCAS$NUTS2_16 ==  "ITI4",]
LUCAS_lazio$STRATUM_LABEL <- as.factor(LUCAS_lazio$STRATUM_LABEL)
LUCAS_lazio$LC <- as.factor(substr(LUCAS_lazio$land_cover,1,1))


## ----warning=FALSE---------------------------------------------------------------------------
des <- e.svydesign(LUCAS_lazio, ids = ~ POINT_ID, strata = ~ STRATUM_LABEL, 
                   weights = ~ cal_wgt, check.data = TRUE)
ls <- find.lon.strata(des)
if (!is.null(ls)) des <- collapse.strata(des)


## --------------------------------------------------------------------------------------------
est <- svystatTM(des, y = ~ LC,estimator="Mean",conf.int=TRUE)
row.names(est) <- c("A-Artificial","B-Cropland","C-Woodland",
              "D-Shrubland","E-Grassland","F-Bareland",
              "G-Water","H-Wetlands")
est


## --------------------------------------------------------------------------------------------
estimates <- as.data.frame(list(LUCAS_landcover=c("A-Artificial","B-Cropland","C-Woodland",
              "D-Shrubland","E-Grassland","F-Bareland",
              "G-Water","H-Wetlands"),
                                LUCAS_estimates=est$Mean,
                                Copernicus_landcover=copern2$land_cover_cop,
                                Copernicus_freqs=copern2$freq))
estimates[,c(1,3)]
#   LUCAS_landcover Copernicus_landcover
# 1    A-Artificial                built
# 2      B-Cropland                crops
# 3      C-Woodland                trees
# 4     D-Shrubland      shrub_and_scrub
# 5     E-Grassland                grass
# 6      F-Bareland                 bare
# 7         G-Water                water
# 8      H-Wetlands   flooded_vegetation
estimates


## --------------------------------------------------------------------------------------------
df <- melt(estimates[c(1,2,4)], id.vars = "LUCAS_landcover")
ggplot(df, aes(x = LUCAS_landcover, y = value, fill = variable)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Comparison of freq_ESA and freq_LUCAS",
       x = "Land Cover Types",
       y = "Means") +
  theme_minimal()


## --------------------------------------------------------------------------------------------
par(mfrow=c(1,1))
ReGenesees:::PlotCI(est, ty = "o", pch = 19, lty = 2)
lines(copern2$freq,add=TRUE,col="red")


## ----echo=FALSE------------------------------------------------------------------------------
cop2 <- cop
for (i in c(1:5)) {
  tot_obs <- sum(cop2[i,c(2:10)])
    cop2[i,c(2:10)] <- cop2[i,c(2:10)] / tot_obs
}

par(mfrow=c(1,2))
plot(cop2$built,type="b",ylim=c(min(cop2$built,est$Mean[1])*0.5,max(cop2$built,est$Mean[1])*1.2),
     ylab = "Built %",xlab="Year",xaxt="n",col="blue")
title("Built-up")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[1],b=0,col="red",lty=2)
abline(a=est$Mean[1],b=0,col="red")
abline(a=est$`CI.u(95%)`[1],b=0,col="red",lty=2)
legend("bottomright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)

plot(cop2$crops,type="b",ylim=c(min(cop2$built,est$Mean[2])*0.5,max(cop2$crops,est$Mean[2])*1.2),
     ylab = "Crops % ",xlab="Year",xaxt="n",col="blue")
title("Crops")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[2],b=0,col="red",lty=2)
abline(a=est$Mean[2],b=0,col="red")
abline(a=est$`CI.u(95%)`[2],b=0,col="red",lty=2)
legend("bottomright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)

plot(cop2$trees,type="b",ylim=c(min(cop2$trees,est$Mean[3])*0.5,max(cop2$trees,est$Mean[3])*1.2),
     ylab = "Trees % ",xlab="Year",xaxt="n",col="blue")
title("Trees")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[3],b=0,col="red",lty=2)
abline(a=est$Mean[3],b=0,col="red")
abline(a=est$`CI.u(95%)`[3],b=0,col="red",lty=2)
legend("bottomright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)

plot(cop2$shrub_and_scrub,type="b",ylim=c(min(cop2$shrub_and_scrub,est$Mean[4])*0.5,max(cop2$shrub_and_scrub,est$Mean[4])*1.2),
     ylab = "Trees % ",xlab="Year",xaxt="n",col="blue")
title("Shrub")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[4],b=0,col="red",lty=2)
abline(a=est$Mean[4],b=0,col="red")
abline(a=est$`CI.u(95%)`[4],b=0,col="red",lty=2)
legend("bottomright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)


plot(cop2$grass,type="b",ylim=c(min(cop2$grass,est$Mean[5])*0.5,max(cop2$grass,est$Mean[5])*1.2),
     ylab = "Grass %",xlab="Year",xaxt="n",col="blue")
title("Grass")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[5],b=0,col="red",lty=2)
abline(a=est$Mean[5],b=0,col="red")
abline(a=est$`CI.u(95%)`[5],b=0,col="red",lty=2)
legend("topright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)

plot(cop2$bare,type="b",ylim=c(0,max(cop2$bare,est$Mean[6])*3),
     ylab = "Bareland % ",xlab="Year",xaxt="n",col="blue")
title("Bareland")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[6],b=0,col="red",lty=2)
abline(a=est$Mean[6],b=0,col="red")
abline(a=est$`CI.u(95%)`[6],b=0,col="red",lty=2)
legend("topright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)

plot(cop2$water,type="b",ylim=c(min(cop2$water,est$Mean[7])*0.5,max(cop2$water,est$Mean[7])*1.2),
     ylab = "Water % ",xlab="Year",xaxt="n",col="blue")
title("Water")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[7],b=0,col="red",lty=2)
abline(a=est$Mean[7],b=0,col="red")
abline(a=est$`CI.u(95%)`[7],b=0,col="red",lty=2)
legend("bottomright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)

plot(cop2$flooded_vegetation,type="b",ylim=c(min(cop2$flooded_vegetation,est$`CI.l(95%)`[8])*0.5,max(cop2$flooded_vegetation,est$`CI.u(95%)`[8])*1.2),
     ylab = "Flooded vegetation % ",xlab="Year",xaxt="n",col="blue")
title("Flooded vegetation")
axis(1, at=c(1:5), lab=c("2018","2019","2020","2021","2022"))
abline(a=est$`CI.l(95%)`[8],b=0,col="red",lty=2)
abline(a=est$Mean[8],b=0,col="red")
abline(a=est$`CI.u(95%)`[8],b=0,col="red",lty=2)
legend("topright", c("LUCAS", "Copernicus"), fill = c("red", "blue"), cex=0.8)

