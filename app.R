library(shiny)
library(ggplot2)
library(palmerpenguins)
library(DT)
library(dplyr)
library(bslib)

# UI
ui <- function() {
  page_navbar(
    title = "Penguin Dashboard",
    sidebar = sidebar(
      checkboxGroupInput("species_filter", "Filter by Species:",
        choices = unique(penguins$species), selected = unique(penguins$species)
      ),
      div(
        style = "text-align:left; font-size: 15px",
        strong("View Code "), a(icon("fab fa-github"), href = "https://github.com/MounaBelaid/shinylive-bslib-shiny-app", target = "_blank")
      )
    ),
    theme = bs_theme(
      version = 5,
      bootswatch = "materia"
    ),
    nav_panel("Dashboard", layout_column_wrap(
      card(
        card_header("Penguin Distribution"),
        plotOutput("penguin_plot"),
        full_screen = TRUE
      )
    )),
    nav_panel("Summary", layout_column_wrap(
      card(
        card_header("Penguin Summary"),
        dataTableOutput("penguin_table"),
        full_screen = TRUE
      )
    ))
  )
}

# Server
server <- function(input, output) {
  # bs_themer()
  # Filter penguins data based on user selection
  filtered_data <- reactive({
    req(input$species_filter)
    if (is.null(input$species_filter)) {
      return(penguins)
    } else {
      filter(penguins, species %in% input$species_filter)
    }
  })

  # Render penguin distribution plot
  output$penguin_plot <- renderPlot({
    ggplot(filtered_data(), aes(x = species, fill = island)) +
      geom_bar(position = "dodge") +
      labs(title = "Penguin Distribution by Species and Island") +
      theme_bw()
  })

  # Render penguin summary table based on filter
  output$penguin_table <- renderDataTable({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

# Run the application
shinyApp(ui, server)
