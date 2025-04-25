library(shiny)
library(readr)
library(dplyr)
library(DT)

downloadButton <- function(...) {
  tag <- shiny::downloadButton(...)
  tag$attribs$download <- NULL
  tag
}

ui <- fluidPage(
  titlePanel("Conflicting Records Finder"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose CSV File 1", accept = c(".csv")),
      numericInput("skip1", "Skip rows in File 1:", value = 0, min = 0, step = 1),
      numericInput("slice1", "Max rows to read from File 1 (0 = all):", value = 0, min = 0, step = 1),
      tags$hr(),
      fileInput("file2", "Choose CSV File 2", accept = c(".csv")),
      numericInput("skip2", "Skip rows in File 2:", value = 0, min = 0, step = 1),
      numericInput("slice2", "Max rows to read from File 2 (0 = all):", value = 0, min = 0, step = 1),
      tags$hr(),
      fileInput("file3", "Choose CSV File 3 (optional)", accept = c(".csv")),
      numericInput("skip3", "Skip rows in File 3:", value = 0, min = 0, step = 1),
      numericInput("slice3", "Max rows to read from File 3 (0 = all):", value = 0, min = 0, step = 1),
      tags$hr(),
      actionButton("go", "Find Conflicts")
    ),
    mainPanel(
      DTOutput("conflicts"),
      downloadButton("downloadData", "Download Report as CSV")
    )
  )
)

server <- function(input, output, session) {
  # Helper to read and slice a table
  read_table <- function(file, skip, slice_n) {
    req(file)
    tbl <- read_csv(file$datapath, skip = skip)
    if (slice_n > 0) tbl <- slice(tbl, 1:slice_n)
    tbl
  }
  
  conflicts_data <- eventReactive(input$go, {
    # Read each file
    tbl1 <- read_table(input$file1, input$skip1, input$slice1)
    tbl2 <- read_table(input$file2, input$skip2, input$slice2)
    tables <- list(File1 = tbl1, File2 = tbl2)
    if (!is.null(input$file3)) {
      tbl3 <- read_table(input$file3, input$skip3, input$slice3)
      tables$File3 <- tbl3
    }
    
    result_list <- list()
    # For each table, find rows not present in any of the others
    for (name in names(tables)) {
      this_tbl <- tables[[name]]
      others <- bind_rows(tables[names(tables) != name])
      # explicitly join by all common columns to replicate default anti_join behavior
      common_cols <- intersect(names(this_tbl), names(others))
      uniq <- anti_join(this_tbl, others)
      if (nrow(uniq) > 0) {
        uniq <- mutate(uniq, Source = name)
        result_list[[name]] <- uniq
      }
    }
    if (length(result_list) > 0) {
      bind_rows(result_list)
    } else {
      tibble(Message = "No conflicting records found.")
    }
  })
  
  output$conflicts <- renderDT({
    datatable(conflicts_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste0("conflicting_records_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write_csv(conflicts_data(), file)
    }
  )
}

shinyApp(ui, server)
