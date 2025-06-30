datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
mat1 <- read.csv(paste0(datapath,"Italy_estimates_predictions_2018_3.csv"),dec=".")
mat2 <- read.csv(paste0(datapath,"Italy_estimates_predictions_2022_3.csv"),dec=".")

mat <- rbind(mat1[1,],mat2[1,],mat1[3,],mat2[3,])
mat <- as.matrix(mat[, 1:8])
mat


# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue", "orange","darkgreen","yellow"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
        ylab = "Percentage",
        main = "Predicted and observed 2018 and 2022")

legend("topright", legend = c("Master Predicted 2018", "Master predicted 2022",
                              "Survey 2018","Survey 2022"), 
       fill = c("skyblue", "orange","darkgreen","yellow"))

mat1 <- mat[c(1,3),]
barplot(mat1,
        beside = TRUE,
        col = c("skyblue","darkgreen"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
        ylab = "Percentage",
        main = "Predicted and observed 2018")

legend("topright", legend = c("Master predicted 2018",
                              "Survey 2018"), 
       fill = c("skyblue","darkgreen"))
       
mat2 <- mat[c(2,4),]
barplot(mat2,
       beside = TRUE,
       col = c("orange","yellow"),
       ylim = c(0, max(mat) + 5),
       names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
       ylab = "Percentage",
       main = "Predicted and observed 2022")

legend("topright", legend = c("Master predicted 2022",
                             "Survey 2022"), 
fill = c("orange","yellow"))

