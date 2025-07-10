datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
mat1 <- read.csv(paste0(datapath,"Italy_estimates_predictions_2018_3.csv"),dec=".")
mat2 <- read.csv(paste0(datapath,"Italy_estimates_predictions_2022_3.csv"),dec=".")

mat <- rbind(mat1[1,],mat2[1,],mat1[3,],mat2[3,])
mat <- as.matrix(mat[, 1:8])
mat


# Crea barplot affiancato
barplot(mat,
        beside = TRUE,
        col = c("skyblue","orange","darkgreen","yellow"),
        ylim = c(0, max(mat) + 5),
        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
        ylab = "Percentage",
        main = "Predicted and observed 2018 and 2022")

legend("topright", legend = c("Master Predicted 2018", 
                              "Master predicted 2022", 
                              "Survey 2018 observed",
                              "Survey 2022 observed"), 
       fill = c("skyblue", "orange","darkgreen","yellow"))

# m1 <- mat[c(1,4),]
# barplot(m1,
#         beside = TRUE,
#         col = c("skyblue","darkgreen"),
#         ylim = c(0, max(mat) + 5),
#         names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
#         ylab = "Percentage",
#         main = "Predicted and observed 2018")
# 
# legend("topright", legend = c("Master predicted 2018",
#                               "Survey 2018"), 
#        fill = c("skyblue","darkgreen"))
#        
# m2 <- mat[c(2,5),]
# barplot(m2,
#        beside = TRUE,
#        col = c("darkgreen","yellow"),
#        ylim = c(0, max(mat) + 5),
#        names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
#        ylab = "Percentage",
#        main = "Predicted and observed 2022")
# 
# legend("topright", legend = c("Survey 2018 predicted (2022)",
#                              "Survey 2022 observed"), 
# fill = c("darkgreen","yellow"))
# 
# 
# m3 <- mat[c(1,2),]
# barplot(m3,
#         beside = TRUE,
#         col = c("skyblue","magenta"),
#         ylim = c(0, max(mat) + 5),
#         names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
#         ylab = "Percentage",
#         main = "Predicted 2018 vs 2022")
# 
# legend("topright", legend = c("Master predicted 2018",
#                               "Master predicted 2022"), 
#        fill = c("skyblue","magenta"))
# 
# 
# m4 <- mat[c(4,5),]
# barplot(m4,
#         beside = TRUE,
#         col = c("darkgreen","yellow"),
#         ylim = c(0, max(mat) + 5),
#         names.arg = c("A", "B", "C", "D", "E", "F", "G", "H"),
#         ylab = "Percentage",
#         main = "Survey observed 2018 vs 2022")
# 
# legend("topright", legend = c("Survey 2018",
#                               "Survey 2022"), 
#        fill = c("magenta","yellow"))
# 
