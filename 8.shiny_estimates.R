# ------------------------------------------------------------
# Shiny: confronto sample/master 2018–2022 + obs2018/obs2022
# ------------------------------------------------------------
setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
library(shiny)
library(tidyverse)
library(scales)

df <- read.csv("estimates.csv")
rownames(df) <- df$estimate
df$series <- NULL
# Se hai già un data frame 'df' con queste righe/colonne, usa quello e commenta i 3 comandi sopra.

years <- 2018:2022

ui <- fluidPage(
  titlePanel("Quote per classe: Sample vs Master (2018–2022) + Obs 2018/2022"),
  sidebarLayout(
    sidebarPanel(
      selectInput("classe", "Modalità (LC1):", choices = colnames(df), selected = "A"),
      helpText("Linea blu: sample_pred; linea rossa: master_pred; nero: obs2018; magenta: obs2022.")
    ),
    mainPanel(
      plotOutput("serie_plot", height = "460px")
    )
  )
)

server <- function(input, output, session) {
  
  # Estrae le due serie temporali e le due osservazioni per la colonna scelta
  dat_reactive <- reactive({
    col <- req(input$classe)
    
    sample_vals <- sapply(years, function(y) as.numeric(df[paste0("sample_pred", y), col]))
    master_vals <- sapply(years, function(y) as.numeric(df[paste0("master_pred", y), col]))
    obs18 <- as.numeric(df["obs2018", col])
    obs22 <- as.numeric(df["obs2022", col])
    
    ts_df <- tibble(
      year   = rep(years, 2),
      value  = c(sample_vals, master_vals),
      series = factor(rep(c("Sample (2018–2022)", "Master (2018–2022)"),
                          each = length(years)),
                      levels = c("Sample (2018–2022)", "Master (2018–2022)",
                                 "Obs 2018", "Obs 2022"))
    )
    
    obs_df <- tibble(
      series = factor(c("Obs 2018", "Obs 2022"),
                      levels = c("Sample (2018–2022)", "Master (2018–2022)",
                                 "Obs 2018", "Obs 2022")),
      y = c(obs18, obs22)
    )
    
    list(ts = ts_df, obs = obs_df)
  })
  
  output$serie_plot <- renderPlot({
    dd <- dat_reactive()
    ts_df <- dd$ts
    obs_df <- dd$obs
    
    # limiti y dinamici (un po' di margine)
    y_max <- max(ts_df$value, obs_df$y, na.rm = TRUE)
    y_min <- min(ts_df$value, obs_df$y, na.rm = TRUE)
    y_pad <- 0.02
    y_lim <- c(max(0, y_min - y_pad), min(1, y_max + y_pad))
    
    col_map <- c(
      "Sample (2018–2022)" = "blue",
      "Master (2018–2022)" = "red",
      "Obs 2018"           = "black",
      "Obs 2022"           = "magenta"
    )
    lty_map <- c(
      "Sample (2018–2022)" = "solid",
      "Master (2018–2022)" = "solid",
      "Obs 2018"           = "dashed",
      "Obs 2022"           = "dashed"
    )
    
    ggplot() +
      # orizzontali per obs2018/obs2022 con legenda
      geom_hline(data = obs_df,
                 aes(yintercept = y, color = series, linetype = series),
                 linewidth = 0.9) +
      # linee serie temporali
      geom_line(data = ts_df,
                aes(x = year, y = value, color = series, linetype = series),
                linewidth = 1.2) +
      geom_point(data = ts_df,
                 aes(x = year, y = value, color = series),
                 size = 2.2) +
      scale_x_continuous(breaks = years, minor_breaks = NULL) +
      scale_y_continuous(labels = percent_format(accuracy = 0.1), limits = y_lim) +
      scale_color_manual(values = col_map, drop = FALSE) +
      scale_linetype_manual(values = lty_map, drop = FALSE) +
      labs(
        title = paste0("Classe selezionata: ", input$classe),
        x = "Anno",
        y = "Quota",
        color = NULL,
        linetype = NULL
      ) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "bottom")
  })
}

shinyApp(ui, server)
