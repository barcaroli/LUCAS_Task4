# ------------------------------------------------------------
# Profili di cambiamento più frequenti (2018–2022)
# ------------------------------------------------------------
library(tidyverse)

# 1) Carica i dati
df <- read.csv("master_predicted.csv", stringsAsFactors = FALSE)

# 2) Identifica e ordina le colonne delle predizioni
pred_cols <- paste0("pred", 2018:2022)
stopifnot(all(pred_cols %in% names(df)))  # controllo di sicurezza

# 3) Crea il profilo per riga (es. "C-C-D-C-C")
df <- df %>%
  mutate(across(all_of(pred_cols), as.character)) %>%
  unite("profilo", all_of(pred_cols), sep = "-", remove = FALSE)

# 4) Frequenze assolute e percentuali
profili_freq <- df %>%
  count(profilo, sort = TRUE, name = "frequenza") %>%
  mutate(perc = frequenza / sum(frequenza))

# 5) Stampa i primi 20
print(head(profili_freq, 20), row.names = FALSE)

# 6) (Opzionale) Grafico bar dei top 15 profili
top_n <- 15
profili_freq %>%
  slice_head(n = top_n) %>%
  ggplot(aes(x = reorder(profilo, frequenza), y = frequenza)) +
  geom_col() +
  coord_flip() +
  labs(
    title = paste("Top", top_n, "profili di cambiamento (2018–2022)"),
    x = "Profilo (2018-2019-2020-2021-2022)",
    y = "Frequenza"
  ) +
  theme_minimal()

# ------------------------------------------------------------
# (Opzionale) Solo profili con almeno un cambiamento
# ------------------------------------------------------------
profili_cambiati <- df %>%
  filter(!(pred2018 == pred2019 & pred2019 == pred2020 &
             pred2020 == pred2021 & pred2021 == pred2022)) %>%
  count(profilo, sort = TRUE, name = "frequenza") %>%
  mutate(perc = frequenza / sum(frequenza))

# Vedi i primi 20 profili “non stabili”
print(head(profili_cambiati, 20), row.names = FALSE)
