## Shiny App: Combined trajectories and monthly boxplots for each index
setwd("C:/Users/UTENTE/Google Drive laptop/LUCAS Copernicus/EarthEngine/TRAJECTORIES/data")
# install.packages(c("shiny", "tidyverse"))
library(shiny)
library(tidyverse)

# Percorso dati (adatta al tuo sistema)
df_all <- read_csv(file.path("S2_S1_Italy_2022_trajectories_sample.csv"),
                   show_col_types = FALSE)

ui <- fluidPage(
  titlePanel("Traiettorie e Boxplot Mensili per LC1"),
  sidebarLayout(
    sidebarPanel(
      checkboxGroupInput(
        inputId = "lc1_sel",
        label = "Classi LC1:",
        choices = unique(df_all$LC1),
        selected = character(0)
      ),
      checkboxInput(
        inputId = "show_box",
        label = "Mostra boxplot mensili",
        value = TRUE
      )
    ),
    mainPanel(
      plotOutput("combined_ndvi"),
      plotOutput("combined_vv"),
      plotOutput("combined_vh")
    )
  )
)

server <- function(input, output, session) {
  # Reactive subset by selected classes
  df <- reactive({
    req(input$lc1_sel)
    df_all %>% filter(LC1 %in% input$lc1_sel)
  })
  
  # Convert to long format
  df_long <- reactive({
    df() %>%
      pivot_longer(
        cols = -LC1,
        names_to = c("index", "month"),
        names_sep = "_",
        values_to = "value"
      ) %>%
      mutate(month = as.integer(month))
  })
  
  # Function to render combined plot
  render_combined <- function(idx_name, y_label, title) {
    renderPlot({
      data_long <- df_long() %>% filter(index == idx_name)
      
      # compute monthly means (trajectories)
      traj <- data_long %>%
        group_by(LC1, month) %>%
        summarize(mean_val = mean(value, na.rm = TRUE), .groups = "drop")
      
      # base plot
      p <- ggplot()
      
      # opzionale: boxplot per mese & classe
      if (isTRUE(input$show_box)) {
        p <- p +
          geom_boxplot(
            data = data_long,
            aes(
              x = month,
              y = value,
              group = interaction(LC1, month),
              fill = LC1
            ),
            position = position_dodge(width = 0.8),
            width = 0.6,
            alpha = 0.4
          ) +
          scale_fill_brewer(palette = "Set1")
      }
      
      # trajectory lines & points
      p <- p +
        geom_line(
          data = traj,
          aes(x = month, y = mean_val, color = LC1, linetype = LC1),
          size = 1
        ) +
        geom_point(
          data = traj,
          aes(x = month, y = mean_val, color = LC1),
          size = 2,
          alpha = 0.8
        ) +
        labs(
          title = title,
          x = "Mese",
          y = y_label,
          fill = "Classe LC1",
          color = "Classe LC1",
          linetype = "Classe LC1"
        ) +
        scale_x_continuous(breaks = 1:12, limits = c(1, 12)) +
        scale_color_brewer(palette = "Set1") +
        scale_linetype_discrete() +
        theme_minimal() +
        theme(legend.position = "bottom")
      
      p
    })
  }
  
  output$combined_ndvi <- render_combined(
    idx_name = "NDVI", y_label = "NDVI", title = "NDVI medio mensile"
  )
  output$combined_vv <- render_combined(
    idx_name = "VV", y_label = "VV", title = "VV medio mensile"
  )
  output$combined_vh <- render_combined(
    idx_name = "VH", y_label = "VH", title = "VH medio mensile"
  )
}

shinyApp(ui, server)
