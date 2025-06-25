# install.packages("tidyverse")  # se non lo hai già
library(tidyverse)
datapath <- "D:\\Google Drive\\LUCAS Copernicus\\EarthEngine\\data\\"
# 1. Carica il dataset
df <- read.csv(paste0(datapath,"S2_S1_Italy_2022_trajectories_sample.csv"))

# 2. Prepara i dati per il plot NDVI
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

# 3. Plot NDVI
p_ndvi <- ggplot(df_ndvi, aes(x = month, y = mean_ndvi, color = LC1)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x     = "Mese",
    y     = "NDVI medio",
    color = "Classe LC1",
    title = "Traiettoria media mensile di NDVI per LC1"
  ) +
  theme_minimal()

print(p_ndvi)

ndvi <- p_ndvi$data

# 4. Prepara i dati per il plot VV
df_vv <- df %>%
  select(LC1, starts_with("VV_")) %>%
  pivot_longer(
    cols         = starts_with("VV_"),
    names_to     = "month",
    names_prefix = "VV_",
    values_to    = "VV"
  ) %>%
  mutate(month = as.integer(month)) %>%
  group_by(LC1, month) %>%
  summarize(mean_vv = mean(VV, na.rm = TRUE), .groups = "drop")

# 5. Plot VV
p_vv <- ggplot(df_vv, aes(x = month, y = mean_vv, color = LC1)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x     = "Mese",
    y     = "VV medio (dB)",
    color = "Classe LC1",
    title = "Traiettoria media mensile di VV per LC1"
  ) +
  theme_minimal()

print(p_vv)

vv <- p_vv$data

# 6. Prepara i dati per il plot VH
df_vh <- df %>%
  select(LC1, starts_with("VH_")) %>%
  pivot_longer(
    cols         = starts_with("VH_"),
    names_to     = "month",
    names_prefix = "VH_",
    values_to    = "VH"
  ) %>%
  mutate(month = as.integer(month)) %>%
  group_by(LC1, month) %>%
  summarize(mean_vh = mean(VH, na.rm = TRUE), .groups = "drop")

# 7. Plot VH
p_vh <- ggplot(df_vh, aes(x = month, y = mean_vh, color = LC1)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 1:12) +
  labs(
    x     = "Mese",
    y     = "VH medio (dB)",
    color = "Classe LC1",
    title = "Traiettoria media mensile di VH per LC1"
  ) +
  theme_minimal()

print(p_vh)

vh <- p_vh$data

trajectories <- cbind(ndvi,vv$mean_vv,vh$mean_vh)
write.table(trajectories,paste0(datapath,"trajectories_2022.csv"),sep=",",quote=F,row.names=F)
