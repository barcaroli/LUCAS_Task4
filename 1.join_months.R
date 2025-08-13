setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES")
datapath <- "C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES\\data\\2021\\"
library(tidyverse)
months <- sprintf("%02d", 1:12)
files  <- file.path(datapath, paste0("S2_S1_Italy_2021_", months, ".csv"))

# leggi tutti i mesi e unisci per POINT_ID
dfs <- files %>%
  map(~ read_csv(.x, show_col_types = FALSE) %>%
        distinct(POINT_ID, .keep_all = TRUE))   # sicurezza: un record per POINT_ID

final <- reduce(dfs, full_join, by = "POINT_ID")

final <- final[order(final$POINT_ID),]
summary(final)
# Ordina le colonne come richiesto: per ogni mese le 4 variabili (B4,B8,VV,VH)
# cols_order <- c(
#   "POINT_ID",
#   unlist(lapply(months, function(m) paste0(c("B4_","B8_","VV_","VH_"), m)))
# )
# 
# # seleziona solo le colonne attese e nell'ordine desiderato (ignora eventuali extra)
# final <- final %>% select(any_of(cols_order))

write.table(final,paste0(datapath,"S2_S1_Italy_2021_Monthly.csv"),sep=",",quote=F,row.names=F)

