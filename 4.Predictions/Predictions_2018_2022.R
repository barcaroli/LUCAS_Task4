datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
mat1 <- read.csv(paste0(datapath,"Italy_estimates_predictions_2018.csv"))
mat2 <- read.csv(paste0(datapath,"Italy_estimates_predictions_2022.csv"))

mat <- rbind(mat1[1,],mat2[1,])
mat <- as.matrix(mat)


# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
        ylab = "Percentage",
        main = "Predicted 2018 and 2022")

legend("topright", legend = c("Master Predicted 2018", "Master predicted 2022"), 
       fill = c("skyblue", "orange"))
