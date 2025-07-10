#################################################################################
# In questo programma si calcolano tante spline quanti sono i punti.            #
# Successivamente:                                                              #
# si calcolano le distanze tra queste e le spline di riferimento (8 distanze)   #
# si riassumono i valori predetti con alcune statistiche:                       #
# - media                                                                       #
# - minimo                                                                      #
# - range                                                                       #
# - mese in cui si registra il minimo                                           # 
# - mese in cui si registra il massimo                                          #
# - numero di mesi tra il minimmo e il massimo (distanza circolare)
#                                                                               #
# i coefficentisono registrati in "verticale" (tante righe quanti sono i        #
#  coefficienti per ciascun punto)                                              #
#                                                                               #
# I risultati sono registrati in:                                               #
# pred_t                                                                        #
# coef_t                                                                        #
# stat_pred_t                                                                   #
# dist_t                                                                        #
# dist_t_oriz                                                                   # 
#################################################################################
library(dtw)
library(tidyverse)
library(mgcv)


casi=1000

datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
# datapath <- "G:\\MEA\\Collaborazioni internazionali\\Progetto stime LUCAS 2022\\pgm\\Processo stima\\data\\"
#------------------------------------------------------------------------------
# leggo le triettorie medie_ndvi
df <- read.csv(paste0(datapath,"S2_S1_Italy_2018_trajectories_sample.csv"),stringsAsFactors = FALSE)

# 
df_ndvi <- df %>%
  select(LC1, starts_with("NDVI_")) %>%        # prendi LC1 e tutte le colonne NDVI_01..12
  pivot_longer(
    cols       = starts_with("NDVI_"),
    names_to   = "month",
    names_prefix = "NDVI_",
    values_to  = "NDVI"
  ) %>%
  mutate(month = as.integer(month)) %>%        # trasforma month in intero 1–12
  group_by(LC1, month) %>%
  summarize(mean_ndvi = mean(NDVI, na.rm = TRUE), .groups = "drop")

# leggo il campione
sample <- read.csv(paste0(datapath,"Italy_sample_2018.csv"))
# casi=NROW(sample)


# 1) stimo i modelli di riferimento
model_A <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="A",])
model_B <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="B",])
model_C <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="C",])
model_D <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="D",])
model_E <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="E",])
model_F <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="F",])
model_G <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="G",])
model_H <- gam(mean_ndvi ~ s(month, bs = "cc", k = 6), data = df_ndvi[df_ndvi$LC1=="H",])

# rappresntazione grafica dei modelli di riferimento
plot(seq(1:12),  model_A$fitted.values, type="l",col = "black", lwd = 2, ylim=c(0,1))
lines(seq(1:12), model_B$fitted.values, col = "red", lwd = 2)
lines(seq(1:12), model_C$fitted.values, col = "green", lwd = 2)
lines(seq(1:12), model_D$fitted.values, col = "yellow", lwd = 2)
lines(seq(1:12), model_E$fitted.values, col = "blue", lwd = 2)
lines(seq(1:12), model_F$fitted.values, col = "magenta", lwd = 2)
lines(seq(1:12), model_G$fitted.values, col = "violet", lwd = 2)
lines(seq(1:12), model_H$fitted.values, col = "darkgreen", lwd = 2)


# 2) calcolo tante spline quanti sono i punti

# su questi dataframe  archivierò i risultati
pred_t<-NULL
coef_t<-NULL
stat_pred_t<-NULL
#inizia il ciclo di for sui punti
for( i in 1:casi){
  cat("\n",i)
#for( i %in% NROW(sample)){
    # seleziono l'unità
    s_i<-sample[i,]
    # organizzo i dati
    s_i<- s_i%>%
      select(c(POINT_ID,LC1), starts_with("NDVI_")) %>%        # prendi LC1 e tutte le colonne NDVI_01..12
      pivot_longer(
        cols       = starts_with("NDVI_"),
        names_to   = "month",
        names_prefix = "NDVI_",
        values_to  = "NDVI"
      )
    s_i$month<-as.integer(s_i$month)
    # faccio il modello (su questocomando si può ragionare)
    model_s <- gam(NDVI ~ s(month, bs = "cc", k = 6), data = s_i)
    # predico i valori
    pred<-data.frame(s_i,model_s$fitted.values)
    
    # calcolo un po di statistiche
    mediana_pred<-median(model_s$fitted.values)
    max_pred<-max(model_s$fitted.values)
    min_pred<-min(model_s$fitted.values)
    month.max_pred<-which.max(model_s$fitted.values)
    month.min_pred<-which.min(model_s$fitted.values)
    range_pred=max_pred-min_pred
    diff_mesi_pred <- (month.max_pred - month.min_pred)
#      diff_mesi_pred<-pmin(d, 12 - d)
    
    stat_pred<-data.frame(s_i[1,c("POINT_ID","LC1")],mediana_pred,max_pred,min_pred,month.max_pred,month.min_pred,range_pred,diff_mesi_pred)

    # metto via le statistiche
    stat_pred_t<-rbind(stat_pred_t,stat_pred)
    # metto via i valori predetti
    pred_t<-rbind(pred_t,pred)
    # metto via i coefficienti
    coef<-cbind(sample[i,c("POINT_ID","LC1")],t(model_s$coefficients))
    coef_t<-rbind(coef_t,coef)
    }
    


# 3) calcolo delle distanze tra singole spline e modelli di referimento 
#    ovvero calcolo 8 distanze per ciascun punto

ID<-unique(sample$POINT_ID) 
dist_t<-NULL
dist_t_oriz<-NULL

for( j in 1:casi){
  cat("\n",j)
  pred_j<-pred_t[pred_t$POINT_ID==ID[j],]

    # per successiva comodità micalcolo due volte la distanza egli assegno due nomi differenti
  alignment <- dtw(model_A$fitted.values, pred_j$model_s.fitted.values)
  dtw_distA <- data.frame(alignment$distance,LC1="A")
  dtw_distA_or <- data.frame("dtw_dist_A"=alignment$distance)
                          
  alignment <- dtw(model_B$fitted.values, pred_j$model_s.fitted.values)
  dtw_distB <- data.frame(alignment$distance,LC1="B")
  dtw_distB_or <- data.frame("dtw_dist_B"=alignment$distance)
  
  alignment <- dtw(model_C$fitted.values, pred_j$model_s.fitted.values)
  dtw_distC <- data.frame(alignment$distance,LC1="C")
  dtw_distC_or <- data.frame("dtw_dist_C"=alignment$distance)
  
  alignment <- dtw(model_D$fitted.values, pred_j$model_s.fitted.values)
  dtw_distD <- data.frame(alignment$distance,LC1="D")
  dtw_distD_or <- data.frame("dtw_dist_D"=alignment$distance)
  
  alignment <- dtw(model_E$fitted.values, pred_j$model_s.fitted.values)
  dtw_distE <- data.frame(alignment$distance,LC1="E")
  dtw_distE_or <- data.frame("dtw_dist_E"=alignment$distance)
  
  alignment <- dtw(model_F$fitted.values, pred_j$model_s.fitted.values)
  dtw_distF <- data.frame(alignment$distance,LC1="F")
  dtw_distF_or <- data.frame("dtw_dist_F"=alignment$distance)
  
  alignment <- dtw(model_G$fitted.values, pred_j$model_s.fitted.values)
  dtw_distG <- data.frame(alignment$distance,LC1="G")
  dtw_distG_or <- data.frame("dtw_dist_G"=alignment$distance)
  
  alignment <- dtw(model_H$fitted.values, pred_j$model_s.fitted.values)
  dtw_distH <- data.frame(alignment$distance,LC1="H")
  dtw_distH_or <- data.frame("dtw_dist_H"=alignment$distance)
  
  # metto via i risultati con due rappresentazioni:
  # - in verticale che risulta comodo per il box plot
  # - in orizzontale che risulta per il successivo merge con i dati by POINT_ID
  # t sta per totale
  dist_j<-NULL
  dist_j<-rbind(dtw_distA,dtw_distB,dtw_distC,dtw_distD,
                dtw_distE,dtw_distF,dtw_distG,dtw_distH)
  colnames(sample)[94]<-"LC"
  dist_j<-cbind(sample[j,c("POINT_ID","LC")],dist_j)
  dist_t<-rbind(dist_t,dist_j)
  
  dist_j_oriz<-NULL
  dist_j_oriz<-cbind(sample[j,c("POINT_ID","LC")], dtw_distA_or,dtw_distB_or,dtw_distC_or,dtw_distD_or,
                                                    dtw_distE_or,dtw_distF_or,dtw_distG_or,dtw_distH_or)
  dist_t_oriz<-rbind(dist_t_oriz,dist_j_oriz)
}
  
boxplot(data=coef_t,coef_t$`(Intercept)`~LC1)
boxplot(data=coef_t,coef_t$`s(month).1`~LC1)
boxplot(data=coef_t,coef_t$`s(month).2`~LC1)
boxplot(data=coef_t,coef_t$`s(month).3`~LC1)
boxplot(data=coef_t,coef_t$`s(month).4`~LC1)

boxplot(data=stat_pred_t,mediana_pred~LC1)
boxplot(data=stat_pred_t,month.max_pred~LC1)
boxplot(data=stat_pred_t,month.min_pred~LC1)
boxplot(data=stat_pred_t,max_pred~LC1)
boxplot(data=stat_pred_t,min_pred~LC1)
boxplot(data=stat_pred_t,range_pred~LC1)
boxplot(data=stat_pred_t,diff_mesi_pred~LC1)
cor(stat_pred_t[,3:9])

boxplot(data=dist_t,alignment.distance~LC1)
cor(dist_t_oriz[,3:10])

cor(dist_t_oriz[,3:10])

par(mfcol=c(9,9))
plot(dist_t_oriz[,3:10],dist_t_oriz[,3:10])  
plot(stat_pred_t[,3:9],stat_pred_t[,3:9])  
  
head(stat_pred_t)  

library(caret)
library(randomForest)
set.seed(1234)
#--------------------------------
# Using spline coefficients
data <- coef_t
colnames(data)[3:7] <- c("Intercept","coeff1","coeff2","coeff3","coeff4")
data$POINT_ID <- NULL
data$LC1 <- as.factor(data$LC1)
training_rows <- createDataPartition(data$LC1, p = 0.8, list = FALSE)
train_data <- data[training_rows, ]
test_data <- data[-training_rows, ]
train_data$POINT_ID <- NULL
test_data$POINT_ID <- NULL  
set.seed(1234)
rf_model <- randomForest(LC1 ~ ., 
                           data=train_data, 
                           importance=TRUE,
                           # mtry=15,
                           # nodesize=5,
                           ntree=200,
                           do.trace=TRUE)  
rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$LC1)
cm
t <- addmargins(cm$table)
t 
#--------------------------------
# Using distances
data <- dist_t_oriz
data$POINT_ID <- NULL
data$LC <- as.factor(data$LC)
training_rows <- createDataPartition(data$LC, p = 0.5, list = FALSE)
train_data <- data[training_rows, ]
test_data <- data[-training_rows, ]
train_data$POINT_ID <- NULL
test_data$POINT_ID <- NULL  
set.seed(1234)
rf_model <- randomForest(LC ~ ., 
                         data=train_data, 
                         importance=TRUE,
                         # mtry=15,
                         # nodesize=5,
                         ntree=200,
                         do.trace=TRUE)  
rf_predictions <- predict(rf_model,test_data)
# Evaluate models on test data
cm <- confusionMatrix(rf_predictions, test_data$LC)
cm
t <- addmargins(cm$table)
t 
