library(shiny)
library(shinyBS)

source(here::here("R/functions.R"))


# Allow uploads up to 100 MB
options(shiny.maxRequestSize = 100 * 1024^2)

ui <- fluidPage(
  titlePanel("Time spent at work from Google's location history"),
  fluidRow(
    column(3,
      fileInput("zip_file", "Upload your Google Takeout zip file", accept = ".zip"),
      selectizeInput("year", "Choose a year", choices = list()),
      selectizeInput("month", "Pick a month", choices = list()),
      downloadButton("download", "Download"),
      bsTooltip("download", "Daily hours in CSV format.")
    ),
    column(5,
      tableOutput("work_summary"),
      tableOutput("work_details"),
      class = "mx-auto"
    ),
    column(4,
      includeMarkdown(here::here("usage.md"))
    )
  ),
  tags$footer("Copyright © 2021 Mikko Marttila", class = "footer"),
  tags$style(".footer { position: absolute; bottom: 10px }")
)

server <- function(input, output, session) {

  # Scan zip file to find available data
  file_index <- reactive({
    req(input$zip_file$datapath)
    index_build(input$zip_file$datapath)
  })

  # Update year choices based on available data
  observe({
    updateSelectizeInput(session, "year", choices = unique(file_index()$year))
  })

  # Update month choices based on year and data
  observe({
    months <- unique(subset(file_index(), input$year == year)$month)
    choices <- setNames(months, month.name[months])
    updateSelectizeInput(session, "month", choices = choices)
  })

  # Load timeline data for visited places in selected period
  visited_places <- reactive({
    req(input$year, input$month)

    file <- index_find(file_index(), input$year, input$month)
    timeline <- jsonlite::fromJSON(unz(input$zip_file$datapath, file))

    find_places(timeline)
  })

  # Output summaries of work time
  output$work_summary <- renderTable({
    summarise_work_time(visited_places())
  })

  work_details <- reactive({
    summarise_work_days(visited_places())
  })

  output$work_details <- renderTable({
    format_work_days(work_details())
  }, align = "rccr")

  # Hand daily data download
  output$download <- downloadHandler(
    filename = function() {
      paste0("work-hours_", input$year, "-", input$month, ".csv")
    },
    content = function(file) {
      readr::write_csv(work_details(), file)
    }
  )
}

shinyApp(ui, server)
