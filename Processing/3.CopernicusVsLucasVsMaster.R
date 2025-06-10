#------------------------------------------------------------------------------
# 3.CopernicusVsLucasVsMaster.R
#
# Script to compare estimates from Earth Engine, LUCAS and Master (predicted values)
# Input: 1. LUCAS survey data of a given year / country with land cover predicted values 
#        2. master dataset (processed by Ballin) for a given country with land cover predicted values
#        3. estimates from Earth Engines (Copernicus)
# Output: 1. excel dataset with estimates
#         2. plots
#------------------------------------------------------------------------------
# devtools::install_github("DiegoZardetto/ReGenesees")
library(ReGenesees)
library(ggplot2)
library(reshape)
library(openxlsx)

setwd("D:/Google Drive/LUCAS Copernicus/EarthEngine/Processing")
# --------------------------------------------------------------------------------------------
# Input 3. estimates from Earth Engines (Copernicus)
cop <- read.xlsx("Italy_2018-06-30_2.xlsx")
lc_values <- c('water', 'trees', 'grass', 'flooded_vegetation', 'crops',
                  'shrub_and_scrub', 'built', 'bare', 'snow_and_ice')
# colnames(cop)[1:9] <- lc_values
colnames(cop)[2:10] <- lc_values
cop_country <- colSums(cop[,c(2:10)])
copern <- NULL
copern$land_cover_cop <- lc_values
# copern$counts <- as.numeric(cop18[,c(2:10)])
copern$counts <- cop_country
copern$freq <- round(copern$counts / sum(copern$counts),4)
copern <- as.data.frame(copern)
copern
copern2 <- copern[c(7,5,2,6,3,8,1,4),]
copern2
sum(copern2$counts)/10000

# --------------------------------------------------------------------------------------------
# Input 2. master dataset (processed by Ballin) for a given country with land cover predicted values
m <- read.csv("Italy_master_2018_preds.csv")
m$land_cover <- as.factor(m$predicted_LC)
levels(m$land_cover) <- c("A-Artificial","B-Cropland","C-Woodland",
                          "D-Shrubland","E-Grassland","F-Bareland",
                          "G-Water","H-Wetlands")
t <- as.data.frame(table(m$land_cover)/nrow(m))
colnames(t) <- c("LUCAS_landcover","Master_counts")
# t$LUCAS_landcover <- as.character(t$LUCAS_landcover)
# t <- rbind(t,c("F",0))
# t <- rbind(t,c("H",0))
# t <- t[order(t$LUCAS_landcover),]
# t$LUCAS_landcover <- as.factor(t$LUCAS_landcover)
# levels(t$LUCAS_landcover) <- c("A-Artificial","B-Cropland","C-Woodland",
#                           "D-Shrubland","E-Grassland","F-Bareland",
#                           "G-Water","H-Wetlands")
t

## --------------------------------------------------------------------------------------------
# Input 1. LUCAS survey data of a given year / country with land cover predicted values 
s <- read.csv("Italy_sample_2018_preds.csv")
s$land_cover <- as.factor(s$predicted_LC)
levels(s$land_cover) <- c("A-Artificial","B-Cropland","C-Woodland",
                          "D-Shrubland","E-Grassland","F-Bareland",
                          "G-Water","H-Wetlands")
lucas <- read.delim("Survey_2018_cal_wgt.txt")
lucas$LC1 <- as.factor(substr(lucas$land_cover,1,1))
lucas$land_cover <- NULL
lucas$STRATUM_LABEL <- as.factor(lucas$STRATUM_LABEL)
lucas <- merge(lucas,s[,c("POINT_ID","land_cover")],by="POINT_ID")
xtabs(~LC1+land_cover,data=lucas)
des <- e.svydesign(lucas, ids = ~ POINT_ID, strata = ~ STRATUM_LABEL, 
                   weights = ~ cal_wgt, check.data = TRUE)
ls <- find.lon.strata(des)
if (!is.null(ls)) des <- collapse.strata(des)
summary(lucas$land_cover)
## --------------------------------------------------------------------------------------------
est1 <- svystatTM(des, y = ~ LC1,estimator="Mean",
                 conf.int=TRUE,na.rm=TRUE)
est2 <- svystatTM(des, y = ~ land_cover,estimator="Mean",
                 conf.int=TRUE,na.rm=TRUE)
ReGenesees:::PlotCI(est2, ty = "o", pch = 19, lty = 2)

row.names(est1) <- c("A-Artificial","B-Cropland","C-Woodland",
              "D-Shrubland","E-Grassland","F-Bareland",
              "G-Water","H-Wetlands")
row.names(est2) <- c("A-Artificial","B-Cropland","C-Woodland",
                     "D-Shrubland","E-Grassland","F-Bareland",
                     "G-Water","H-Wetlands")
colnames(est1)[c(3:4)] <- c("CI.l","CI.u")
colnames(est2)[c(3:4)] <- c("CI.l","CI.u")
est1
est2
## --------------------------------------------------------------------------------------------
estimates <- as.data.frame(list(LUCAS_landcover=c("A-Artificial","B-Cropland","C-Woodland",
              "D-Shrubland","E-Grassland","F-Bareland",
              "G-Water","H-Wetlands"
              ),
                      LUCAS_survey=round(est1$Mean,4),
              LUCAS_CI.l=round(est1$CI.l,4),
              LUCAS_CI.l=round(est1$CI.u,4),
                      LUCAS_predicted=round(est2$Mean,4),
                      Master_predicted=round(as.numeric(t$Master_counts),4),
                      Copernicus_landcover=copern2$land_cover_cop,
                      Copernicus_freqs=copern2$freq))
estimates

ReGenesees:::PlotCI(est1, ty = "o", pch = 19, lty = 2, las=2)
lines(estimates$Master_predicted,col="red")
lines(estimates$Copernicus_freqs,col="darkgreen")
legend("topright", c("Sample observed","Master predicted","Copernicus EE"), 
       fill = c("red", "blue", "darkgreen"), cex=0.8)
title("2018 Italy estimates")

ReGenesees:::PlotCI(est2, ty = "o", pch = 19, lty = 2, las=2)
lines(estimates$Master_predicted,col="red")
lines(estimates$Copernicus_freqs,col="darkgreen")
legend("topright", c("Sample predicted","Master predicted","Copernicus EE"), 
       fill = c("red", "blue", "darkgreen"), cex=0.8)
title("2018 Italy estimates")


## --------------------------------------------------------------------------------------------
# df <- melt(estimates[c(1,2,4)], id.vars = "LUCAS_landcover")
# ggplot(df, aes(x = LUCAS_landcover, y = value, fill = variable)) +
#   geom_bar(stat = "identity", position = "dodge") +
#   labs(title = "Comparison of freq_ESA and freq_LUCAS",
#        x = "Land Cover Types",
#        y = "Means") +
#   theme_minimal()


## --------------------------------------------------------------------------------------------
par(mfrow=c(1,1))
ReGenesees:::PlotCI(est1, ty = "o", pch = 19, lty = 2)
lines(copern2$freq,add=TRUE,col="red")

library(ggplot2)
library(dplyr)

# 1) Prepara i dati
est <- estimates %>%
  rename(LUCAS_CI.u = LUCAS_CI.l.1) %>%             # rinomina upper bound
  mutate(LUCAS_landcover = factor(
    LUCAS_landcover,
    levels = LUCAS_landcover
  ))

# 2) Graficazione
ggplot(est,
       aes(
         x = LUCAS_landcover,
         y = LUCAS_survey,
         group = 1            # assicura un'unica serie continua
       )) +
  # ribbon: fill e group
  geom_ribbon(aes(
    ymin = LUCAS_CI.l,
    ymax = LUCAS_CI.u,
    fill = "Intervallo di confidenza",
    group = 1
  ),
  alpha = 0.3) +
  # linea LUCAS_survey
  geom_line(aes(
    color = "LUCAS_survey"
  ),
  size = 1,
  group = 1) +
  # linea Master_predicted
  geom_line(aes(
    y     = Master_predicted,    # ridefinisco y
    color = "Master_predicted"
  ),
  linetype = "dashed",
  size     = 1,
  group    = 1) +
  
  # definisci colori e fill manualmente
  scale_color_manual(name   = "Serie",
                     values = c(
                       LUCAS_survey     = "steelblue",
                       Master_predicted = "darkred"
                     )) +
  scale_fill_manual(name   = "",
                    values = c(
                      "Intervallo di confidenza" = "steelblue"
                    )) +
  
  # ordine legenda
  guides(fill  = guide_legend(order = 1),
         color = guide_legend(order = 2)) +
  
  # etichette e tema
  labs(x        = "Land cover",
       y        = "Proporzione stimata",
       title    = "Stime LUCAS con intervallo di confidenza",
       subtitle = "Linea tratteggiata: Master_predicted") +
  theme_minimal() +
  theme(axis.text.x    = element_text(angle = 45, hjust = 1),
        legend.position = "top")




# Preparazione dei dati
est <- estimates
# Rinomina le colonne in modo più comodo
est$lower <- est$LUCAS_CI.l
est$upper <- est$LUCAS_CI.l.1  # ora contiene i valori corretti
est$p      <- est$LUCAS_survey
est$m      <- est$Master_predicted
# Ordine delle categorie e coordinate x
est$x <- seq_along(est$LUCAS_landcover)

# Imposto margini un po' più ampi a sinistra e sotto
old.par <- par(mar = c(5,5,4,2) + 0.1)

# 1) creo il canvas vuoto con range y che includa anche il ribbon
plot(NA,
     xlim = range(est$x),
     ylim = range(c(est$lower, est$upper, est$p, est$m)),
     xaxt = "n",       # salto l'asse x automatico
     xlab = "Land cover",
     ylab = "Estimates",
     main = "Land Cover Italy 2018"
)

# 2) disegno il ribbon con polygon()
#    costruisco i vertici: prima in avanti lower, poi al contrario upper
xs <- c(est$x, rev(est$x))
ys <- c(est$lower, rev(est$upper))
polygon(xs, ys,
        col = adjustcolor("steelblue", alpha.f = 0.2),
        border = NA)

# 3) disegno la linea LUCAS_survey
lines(est$x, est$p,
      type = "b",    # linea + punti
      lwd  = 2,
      pch  = 16,
      col  = "steelblue")

# 4) disegno la linea Master_predicted in tratteggio
lines(est$x, est$m,
      type    = "b",
      lwd     = 2,
      pch     = 17,
      lty     = 2,
      col     = "darkred")

# 5) etichette categorie sull'asse x
axis(1,
     at    = est$x,
     labels = est$LUCAS_landcover,
     las   = 2,   # ruota le etichette
     cex.axis = 0.8)

# 6) legenda
legend("topright",
       legend = c("Intervallo di confidenza",
                  "LUCAS_survey",
                  "Master_predicted"),
       fill   = c(adjustcolor("steelblue", alpha.f = 0.2), NA, NA),
       border = c(NA, NA, NA),
       lty    = c(NA, 1, 2),
       pch    = c(NA, 16, 17),
       col    = c(NA, "steelblue", "darkred"),
       pt.cex = c(1,1,1),
       bty    = "n")

# ripristino i par originali
par(old.par)

write.csv(estimates,"estimates.csv",row.names=F)

