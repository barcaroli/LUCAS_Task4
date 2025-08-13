setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
# Master predictions
m <- read.csv("master_predicted.csv")

df <- m

# LC A
# df$pred2019 <- ifelse(df$pred2018 == "A","A",df$pred2019)
df$pred2020 <- ifelse(df$pred2018 == "A" & df$pred2019 == "A","A",df$pred2020)
df$pred2021 <- ifelse(df$pred2018 == "A" & df$pred2019 == "A" & df$pred2020 == "A","A",df$pred2021)
df$pred2022 <- ifelse(df$pred2018 == "A" & df$pred2019 == "A" & df$pred2020 == "A" & df$pred2021 == "A","A",df$pred2022)

# LC B
# df$pred2020 <- ifelse(df$pred2018 == "B" & df$pred2019 == "B","B",df$pred2020)
# df$pred2021 <- ifelse(df$pred2018 == "B" & df$pred2019 == "B" & df$pred2020 == "B","B",df$pred2021)
# df$pred2022 <- ifelse(df$pred2018 == "B" & df$pred2019 == "B" & df$pred2020 == "B" & df$pred2021 == "G","G",df$pred2022)


# LC G
df$pred2020 <- ifelse(df$pred2018 == "G" & df$pred2019 == "G","G",df$pred2020)
df$pred2021 <- ifelse(df$pred2018 == "G" & df$pred2019 == "G" & df$pred2020 == "G","G",df$pred2021)
df$pred2022 <- ifelse(df$pred2018 == "G" & df$pred2019 == "G" & df$pred2020 == "G" & df$pred2021 == "G","G",df$pred2022)


write.table(df,"master_predicted_treated.csv",sep=",",quote=F,row.names=F)
