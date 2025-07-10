library(tidyverse)
library(broom)

# 1) Leggi i dati
df <- read_csv("samp_trajectories.csv")

# 1) calcola i parametri per ogni POINT_ID come prima
params <- df_long %>%
  group_by(POINT_ID) %>%
  do({
    mdl <- lm(NDVI ~ poly(month, 3, raw = TRUE), data = .)
    co <- coef(mdl)
    tibble(
      b0 = co[1],
      b1 = co[2],
      b2 = co[3],
      b3 = co[4]
    )
  }) %>%
  ungroup()

# 2) genera per ogni riga una tibble di mesi+NDVI interpolato
df_fits <- params %>%
  mutate(
    fitted = pmap(
      # qui passo quattro colonne, tutte lunghe come params
      list(b0, b1, b2, b3),
      # pmap ad ogni riga applica questa funzione anonima
      function(b0, b1, b2, b3) {
        month <- 1:12
        tibble(
          month = month,
          NDVI  = b0 + b1*month + b2*month^2 + b3*month^3
        )
      }
    )
  ) %>%
  # ora “fitted” è una list-column di tibbles, una per POINT_ID
  select(POINT_ID, fitted) %>%
  unnest(cols = fitted)

# 3) (Opzionale) se vuoi riportare anche LC1:
df_fits <- df_fits %>%
  left_join(
    df_long %>% distinct(POINT_ID, LC1),
    by = "POINT_ID"
  )

ggplot(df_fits, aes(
  x     = month,
  y     = NDVI,
  group = POINT_ID,   # ogni linea è un POINT_ID
  color = LC1         # colore in base a LC1
)) +
  geom_line(size = 0.8, alpha = 0.7) +
  geom_point(size = 1.5, alpha = 0.7) +
  scale_x_continuous(breaks = 1:12) +
  scale_color_viridis_d(option = "D") +       # palette discreta adatta alle categorie
  labs(
    x     = "Mese",
    y     = "NDVI interpolato",
    color = "Classe LC1",
    title = "Traiettorie mensili di NDVI, colorate per LC1"
  ) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank()
  )

ggplot(df_fits, aes(x = LC1, y = NDVI, fill = LC1)) +
  geom_boxplot(outlier.alpha = 0.3) +
  scale_fill_viridis_d(option = "D") +       # palette discreta
  labs(
    x     = "Classe LC1",
    y     = "NDVI interpolato",
    title = "Distribuzione di NDVI per Classe LC1"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",                # rimuove legenda ridondante
    axis.text.x     = element_text(angle = 45, hjust = 1)
  )
