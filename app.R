library(shiny)

source(here::here("R/functions.R"))

tzInput <- function() {
  tagList(
    radioButtons(
      "tz_type",
      "Time zone",
      choices = c(
        "Use the location time zone" = "auto",
        "Use a fixed time zone" = "fixed"
      )
    ),
    conditionalPanel(
      "input.tz_type === 'fixed'",
      selectizeInput(
        "fixed_tz",
        "Select a fixed time zone",
        choices = tz_choices(),
        selected = "Europe/London"
      ),
    ),
  )
}

# Allow uploads up to 100 MB
options(shiny.maxRequestSize = 100 * 1024 ^ 2)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Time spent at work from your location history"),
  fluidRow(
    column(3,
      tags$h3("Controls"),
      fileInput("zip_file", "Upload your Google Takeout zip file", accept = ".zip"),
      selectizeInput("year", "Choose a year", choices = list()),
      selectizeInput("month", "Pick a month", choices = list()),
      downloadButton("download", "Download", class = "disabled"),
      shinyBS::bsTooltip("download", "Daily hours in CSV format."),
      tags$hr(),
      tags$h4("Options"),
      tzInput(),
    ),
    column(5,
      tags$h3("Your summary"),
      tableOutput("work_summary"),
      tableOutput("work_details"),
    ),
    column(4,
      includeMarkdown(here::here("usage.md")),
      tags$hr(),
      tags$span("Copyright © 2021 Mikko Marttila", style = "color: #ababab"),
    )
  ),
)

server <- function(input, output, session) {
  # Scan zip file to find available data
  file_index <- reactive({
    req(input$zip_file$datapath)

    index <- try(index_build(input$zip_file$datapath))
    validate(need(index, "Failed to open file. Is it a valid zip file?"))
    validate(need(nrow(index) > 0, "Couldn't find any timeline data in file."))

    index
  })

  # Update year choices based on available data
  observe({
    updateSelectizeInput(session, "year", choices = unique(file_index()$year))
  })

  # Update month choices based on year and data
  observe({
    months <- unique(subset(file_index(), input$year == year)$month)
    choices <- setNames(months, month.name[months])
    selected <- isolate(input$month)
    selected <- if (selected %in% choices) selected else NULL
    updateSelectizeInput(session, "month", choices = choices, selected = selected)
  })

  # Load timeline data for visited places in selected period
  timeline_file <- reactive({
    index_find(file_index(), input$year, input$month)
  })

  work_visits <- reactive({
    req(timeline_file())

    zip_conn <- unz(input$zip_file$datapath, timeline_file())
    timeline <- jsonlite::fromJSON(zip_conn)

    visits <- find_places(timeline) %>%
      filter(place_type == "TYPE_WORK")
    validate(need(nrow(visits) > 0, "No work visits in given period."))

    visits
  })

  work_visits_zoned <- reactive({
    if (input$tz_type == "fixed") {
      work_visits() %>%
        mutate(tz = input$fixed_tz)
    } else {
      work_visits()
    }
  })

  # Output summaries of work time
  output$work_summary <- renderTable({
    summarise_total_hours(work_visits_zoned())
  })

  work_details <- reactive({
    summarise_daily_hours(work_visits_zoned())
  })

  # Enable download only after daily summary is available
  observe({
    req(work_details())
    shinyjs::enable("download")
  })

  output$work_details <- renderTable({
    format_daily_hours(work_details())
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
