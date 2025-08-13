# app.R
# ------------------------------------------------------------
# Shiny: profili di cambiamento filtrati per presenza di valori (A–H)
# ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(scales)
library(DT)

ui <- fluidPage(
  titlePanel("Profili di cambiamento (2018–2022) — filtro per valori"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file", "Carica CSV (con colonne: POINT_ID, pred2018..pred2022)",
                accept = c(".csv")),
      helpText("Se non carichi un file, verrà usato un piccolo dataset di esempio."),
      
      checkboxGroupInput("valori", "Cerca questi valori (A–H):",
                         choices = LETTERS[1:8], selected = "G"),
      numericInput("min_n", "Almeno quante occorrenze nella riga:", value = 1, min = 1, max = 5, step = 1),
      
      sliderInput("top_n", "Mostra i Top N profili nel grafico:", min = 5, max = 50, value = 15, step = 1),
      
      checkboxInput("solo_cambiati", "Escludi profili stabili (AAAAA, BBBBB, ...)", value = FALSE),
      
      hr(),
      downloadButton("dl_profili", "Scarica tabella profili (CSV)"),
      strong("Scarica grafico:"),
      downloadButton("dl_plot_png", "PNG"),
      downloadButton("dl_plot_pdf", "PDF"),
      downloadButton("dl_plot_svg", "SVG")
    ),
    mainPanel(
      fluidRow(
        column(12,
               h4("Grafico frequenze profili (filtrati)"),
               plotOutput("plot_profili", height = "520px")
        )
      ),
      br(),
      h4("Tabella profili (filtrati)"),
      DTOutput("tab_profili"),
      br(),
      h5(textOutput("summary_txt"))
    )
  )
)

server <- function(input, output, session) {
  
  # Dati di esempio se non viene caricato nulla
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
    req(input$valori)  # almeno un valore selezionato
    if (is.null(input$file)) {
      sample_data()
    } else {
      read.csv(input$file$datapath, stringsAsFactors = FALSE)
    }
  })
  
  pred_cols <- reactive({
    # vincoliamo all'intervallo 2018–2022
    cols <- paste0("pred", 2018:2022)
    cols
  })
  
  # Validazione colonne
  observe({
    df <- raw_data()
    missing <- setdiff(pred_cols(), names(df))
    if (length(missing) > 0) {
      showNotification(paste("Mancano colonne:", paste(missing, collapse = ", ")),
                       type = "error", duration = 6)
    }
  })
  
  # Dataset preparato: profilo, filtro per valori/occorrenze
  prepared <- reactive({
    df <- raw_data()
    cols <- pred_cols()
    validate(
      need(all(cols %in% names(df)), "Il file deve contenere le colonne pred2018..pred2022.")
    )
    df <- df %>%
      mutate(across(all_of(cols), as.character))
    
    # Conteggio delle occorrenze dei valori selezionati (colonna per colonna)
    vals  <- input$valori
    min_n <- max(1, as.integer(input$min_n %||% 1))
    
    cmp <- do.call(cbind, lapply(df[cols], function(x) x %in% vals))
    # gestisci eventuali NA come FALSE
    cmp[is.na(cmp)] <- FALSE
    
    mask <- rowSums(cmp) >= min_n
    
    
    df_filt <- df[mask, , drop = FALSE] %>%
      unite("profilo", all_of(cols), sep = "-", remove = FALSE)
    
    if (isTRUE(input$solo_cambiati)) {
      # escludi profili stabili (AAAAA, BBBBB, ...)
      df_filt <- df_filt %>%
        filter(!(pred2018 == pred2019 &
                   pred2019 == pred2020 &
                   pred2020 == pred2021 &
                   pred2021 == pred2022))
    }
    
    df_filt
  })
  
  # Tabella frequenze profili (ordinata)
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
  
  output$plot_profili <- renderPlot({
    tab <- profili_freq()
    validate(need(nrow(tab) > 0, "Nessun profilo soddisfa i criteri selezionati."))
    
    top_n <- min(input$top_n, nrow(tab))
    tab %>%
      slice_head(n = top_n) %>%
      ggplot(aes(x = reorder(profilo, frequenza), y = frequenza)) +
      geom_col() +
      geom_text(aes(label = paste0(comma(frequenza), " (", percent(perc, accuracy = 0.1), ")")),
                hjust = -0.05, size = 3) +
      coord_flip(clip = "off") +
      scale_y_continuous(expand = expansion(mult = c(0, 0.15))) +
      labs(
        title = paste("Top", top_n, "profili filtrati"),
        subtitle = paste0("Valori cercati: {", paste(input$valori, collapse = ","), "}  •  occorrenze ≥ ", input$min_n),
        x = "Profilo (2018-2019-2020-2021-2022)",
        y = "Frequenza"
      ) +
      theme_minimal(base_size = 12)
  })
  
  output$tab_profili <- renderDT({
    tab <- profili_freq() %>%
      mutate(perc = percent(perc, accuracy = 0.01))
    datatable(tab, rownames = FALSE, options = list(pageLength = 15))
  })
  
  output$summary_txt <- renderText({
    df_filt <- prepared()
    if (nrow(df_filt) == 0) return("")
    tot <- nrow(raw_data())
    sel <- nrow(df_filt)
    paste0("Righe selezionate: ", sel, " su ", tot, " (", percent(sel / tot, accuracy = 0.01), ").")
  })
  
  output$dl_profili <- downloadHandler(
    filename = function() paste0("profili_filtrati_", Sys.Date(), ".csv"),
    content = function(file) {
      write.csv(profili_freq(), file, row.names = FALSE)
    }
  )
}

shinyApp(ui, server)

