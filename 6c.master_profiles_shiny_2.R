# app.R
# ------------------------------------------------------------
# Profili di cambiamento (2018–2022) con filtro AND e download grafico
# ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)
library(DT)
library(plotly)

ui <- fluidPage(
  titlePanel("Profili di cambiamento (2018–2022) — filtro per valori (AND)"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carica CSV (colonne: POINT_ID, pred2018..pred2022)", accept = ".csv"),
      helpText("Se non carichi un file, viene usato un piccolo dataset di esempio."),
      
      checkboxGroupInput("valori", "Cerca questi valori (A–H):",
                         choices = LETTERS[1:8], selected = "G"),
      numericInput("min_n", "Almeno quante occorrenze per ciascun valore selezionato:",
                   value = 1, min = 1, max = 5, step = 1),
      sliderInput("top_n", "Mostra i Top N profili nel grafico:", min = 5, max = 50, value = 15, step = 1),
      checkboxInput("solo_cambiati", "Escludi profili stabili (AAAAA, BBBBB, ...)", value = FALSE),
      
      hr(),
      strong("Scarica grafico (da ggplot):"),
      div(style = "display:flex; gap:8px; flex-wrap:wrap;",
          downloadButton("dl_plot_png", "PNG"),
          downloadButton("dl_plot_pdf", "PDF"),
          downloadButton("dl_plot_svg", "SVG")
      ),
      hr(),
      downloadButton("dl_profili", "Scarica tabella profili (CSV)")
    ),
    mainPanel(
      h4("Grafico frequenze profili (filtrati)"),
      plotlyOutput("plot_profili", height = "520px"),
      br(),
      h4("Tabella profili (filtrati)"),
      DTOutput("tab_profili"),
      br(),
      h5(textOutput("summary_txt"))
    )
  )
)

server <- function(input, output, session) {
  
  # Dati di esempio (se non carichi file)
  sample_data <- reactive({
    tibble::tribble(
      ~POINT_ID, ~pred2018, ~pred2019, ~pred2020, ~pred2021, ~pred2022,
      40562450,   "F",       "F",       "D",       "F",       "F",
      40582442,   "E",       "E",       "E",       "E",       "E",
      40582444,   "C",       "C",       "D",       "C",       "C",
      40582446,   "C",       "E",       "E",       "E",       "D",
      40582448,   "C",       "C",       "E",       "C",       "C",
      40582450,   "F",       "E",       "D",       "F",       "E",
      40602442,   "C",       "C",       "C",       "C",       "C",
      40602444,   "C",       "C",       "C",       "C",       "C",
      40602446,   "C",       "E",       "D",       "D",       "E",
      40602450,   "C",       "E",       "E",       "E",       "D"
    )
  })
  
  # Lettura dati
  raw_data <- reactive({
    req(length(input$valori) >= 1)
    if (is.null(input$file)) {
      sample_data()
    } else {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
  })
  
  pred_cols <- reactive(paste0("pred", 2018:2022))
  
  # Avviso se mancano colonne
  observe({
    df <- raw_data()
    missing <- setdiff(pred_cols(), names(df))
    if (length(missing) > 0) {
      showNotification(paste("Mancano colonne nel file caricato:", paste(missing, collapse = ", ")),
                       type = "error", duration = 6)
    }
  })
  
  # Prepara dataset filtrato (LOGICA AND)
  prepared <- reactive({
    df <- raw_data()
    cols <- pred_cols()
    validate(need(all(cols %in% names(df)), "Il file deve contenere le colonne pred2018..pred2022."))
    
    df <- df %>% mutate(across(all_of(cols), as.character))
    
    # Matrice caratteri 5 colonne (2018..2022)
    X <- as.matrix(df[cols])
    storage.mode(X) <- "character"
    
    vals  <- input$valori
    min_n <- max(1, as.integer(input$min_n %||% 1))
    
    # Conta per riga le occorrenze di ciascun valore selezionato
    count_mat <- do.call(cbind, lapply(vals, function(v) rowSums(X == v, na.rm = TRUE)))
    if (is.null(dim(count_mat))) count_mat <- cbind(count_mat)  # garantisci matrice anche con 1 valore
    
    colnames(count_mat) <- vals
    # Filtro AND: ogni valore selezionato deve comparire >= min_n volte
    mask <- apply(count_mat >= min_n, 1, all)
    
    df_filt <- df[mask, , drop = FALSE] %>%
      unite("profilo", all_of(cols), sep = "-", remove = FALSE)
    
    if (isTRUE(input$solo_cambiati)) {
      df_filt <- df_filt %>%
        filter(!(pred2018 == pred2019 &
                   pred2019 == pred2020 &
                   pred2020 == pred2021 &
                   pred2021 == pred2022))
    }
    df_filt
  })
  
  # Frequenze profili
  profili_freq <- reactive({
    df_filt <- prepared()
    if (nrow(df_filt) == 0) {
      tibble(profilo = character(), frequenza = integer(), perc = numeric())
    } else {
      df_filt %>%
        count(profilo, sort = TRUE, name = "frequenza") %>%
        mutate(perc = frequenza / sum(frequenza))
    }
  })
  
  # Costruisci il ggplot (usato sia per plotly che per download)
  plot_profili_gg <- reactive({
    tab <- profili_freq()
    validate(need(nrow(tab) > 0, "Nessun profilo soddisfa i criteri selezionati."))
    
    top_n <- min(input$top_n, nrow(tab))
    tab %>%
      slice_head(n = top_n) %>%
      ggplot(aes(x = reorder(profilo, frequenza), y = frequenza)) +
      geom_col() +
      geom_text(aes(label = paste0(comma(frequenza),
                                   " (", percent(perc, accuracy = 0.1), ")")),
                hjust = -0.05, size = 3) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste("Top", top_n, "profili filtrati"),
        subtitle = paste0("Valori (AND): {", paste(input$valori, collapse = ","), "}  •  occorrenze ≥ ", input$min_n),
        x = "Profilo (2018-2019-2020-2021-2022)",
        y = "Frequenza"
      ) +
      theme_minimal(base_size = 12)
  })
  
  # Grafico interattivo (copiabile/scaricabile via toolbar)
  output$plot_profili <- renderPlotly({
    ggplotly(plot_profili_gg()) %>%
      config(displaylogo = FALSE)  # toolbar pulita, include "Download plot as png"
  })
  
  # Download grafico (PNG/PDF/SVG) da ggplot
  output$dl_plot_png <- downloadHandler(
    filename = function() paste0("profili_top_", input$top_n, "_", Sys.Date(), ".png"),
    content  = function(file) {
      ggplot2::ggsave(filename = file, plot = plot_profili_gg(), width = 8, height = 5.5, dpi = 300)
    }
  )
  output$dl_plot_pdf <- downloadHandler(
    filename = function() paste0("profili_top_", input$top_n, "_", Sys.Date(), ".pdf"),
    content  = function(file) {
      ggplot2::ggsave(filename = file, plot = plot_profili_gg(), width = 8, height = 5.5, device = grDevices::pdf)
    }
  )
  output$dl_plot_svg <- downloadHandler(
    filename = function() paste0("profili_top_", input$top_n, "_", Sys.Date(), ".svg"),
    content  = function(file) {
      ggplot2::ggsave(filename = file, plot = plot_profili_gg(), width = 8, height = 5.5, device = "svg")
      # Nota: per SVG potrebbe servire svglite: install.packages("svglite")
    }
  )
  
  # Tabella e download CSV
  output$tab_profili <- renderDT({
    profili_freq() %>%
      mutate(perc = percent(perc, accuracy = 0.01)) %>%
      datatable(rownames = FALSE, options = list(pageLength = 15))
  })
  
  output$summary_txt <- renderText({
    df_filt <- prepared()
    if (nrow(df_filt) == 0) return("Nessuna riga selezionata con i criteri correnti.")
    tot <- nrow(raw_data()); sel <- nrow(df_filt)
    paste0("Righe selezionate: ", sel, " su ", tot, " (", percent(sel / tot, accuracy = 0.01), ").")
  })
  
  output$dl_profili <- downloadHandler(
    filename = function() paste0("profili_filtrati_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(profili_freq(), file, row.names = FALSE)
  )
}

shinyApp(ui, server)
