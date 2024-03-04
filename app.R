library(shiny)
library(DT)
library(dplyr)
library(tidyr) # For pivot_longer and pivot_wider
library(rio)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Data Distillery: Cleaning and Preprocessing Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose your data file", accept = c(".csv", ".xlsx", ".rds")),
      hr(),
      checkboxGroupInput("varSelect", "Variables to keep:", choices = NULL),
      selectInput("missingDataOption", "Missing data handling:",
                  choices = c("None", "Convert Common Missing Values to NA", "Listwise Deletion", "Mean Imputation")),
      checkboxInput("detectOutliers", "Detect and handle outliers", value = FALSE),
      numericInput("outlierThreshold", "Outlier threshold (z-score)", value = 3, min = 1.5, step = 0.1),
      hr(),
      selectInput("pivotOption", "Pivot Option:", choices = c("None", "Pivot Longer", "Pivot Wider")),
      textInput("pivotNamesTo", "Names to:", "name"),
      textInput("pivotValuesTo", "Values to:", "value"),
      textInput("pivotIDsTo", "ID Column:", "id_col"),
      actionButton("processData", "Process Data", class = "btn-primary"),
      downloadButton("downloadData", "Download Processed Data")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data View", DTOutput('dataTable')),
        tabPanel("Learn It!", tags$iframe(style = "height:600px; width:100%", src = "r_intro_guide.pdf"))
      )
    )
  )
)

library(shiny)
library(DT)
library(dplyr)
library(tidyr) # For pivot_longer and pivot_wider
library(rio)
library(ggplot2)

ui <- fluidPage(
  titlePanel("Data Distillery: Cleaning and Preprocessing Tool"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose your data file", accept = c(".csv", ".xlsx", ".rds")),
      hr(),
      checkboxGroupInput("varSelect", "Variables to keep:", choices = NULL),
      selectInput("missingDataOption", "Missing data handling:",
                  choices = c("None", "Convert Common Missing Values to NA", "Listwise Deletion", "Mean Imputation")),
      checkboxInput("detectOutliers", "Detect and handle outliers", value = FALSE),
      numericInput("outlierThreshold", "Outlier threshold (z-score)", value = 3, min = 1.5, step = 0.1),
      hr(),
      selectInput("pivotOption", "Pivot Option:", choices = c("None", "Pivot Longer", "Pivot Wider")),
      textInput("pivotNamesTo", "Names to:", "name"),
      textInput("pivotValuesTo", "Values to:", "value"),
      textInput("pivotIDsTo", "ID Column:", "id_col"),
      actionButton("processData", "Process Data", class = "btn-primary"),
      downloadButton("downloadData", "Download Processed Data")
    ),
    mainPanel(
      tabsetPanel(
        type = "tabs",
        tabPanel("Data View", DTOutput('dataTable')),
        tabPanel("Learn It!", tags$iframe(style = "height:600px; width:100%", src = "r_intro_guide.pdf"))
      )
    )
  )
)

server <- function(input, output, session) {
  data <- reactiveVal(NULL)
  
  observeEvent(input$file1, {
    df <- rio::import(input$file1$datapath)
    updateCheckboxGroupInput(session, "varSelect", choices = names(df), selected = names(df))
    data(df)
  })
  
  processedData <- eventReactive(input$processData, {
    req(data())
    df <- data()
    
    # Preprocessing steps as before
    if(input$missingDataOption == "Convert Common Missing Values to NA") {
      df[df == "" | df == "-9" | df == "-99"] <- NA
    }
    
    if (!is.null(input$varSelect)) {
      df <- df %>% select(all_of(input$varSelect))
    }
    
    df <- switch(input$missingDataOption,
                 "Listwise Deletion" = na.omit(df),
                 "Mean Imputation" = df %>% mutate(across(where(is.numeric), ~ifelse(is.na(.), mean(., na.rm = TRUE), .))),
                 df)
    
    if (input$detectOutliers) {
      df <- df %>% mutate(across(where(is.numeric), ~if_else(abs((. - mean(., na.rm = TRUE)) / sd(., na.rm = TRUE)) > input$outlierThreshold, NA_real_, .)))
    }
    
    # Pivot operations
    tryCatch({
      if (input$pivotOption == "Pivot Longer" && input$pivotNamesTo != "" && input$pivotValuesTo != "") {
        # Assuming input$pivotIDsTo is a comma-separated string of column names
        id_cols <- strsplit(input$pivotIDsTo, ",\\s*")[[1]]
        df <- pivot_longer(df, cols = all_of(id_cols), names_to = input$pivotNamesTo, values_to = input$pivotValuesTo)
      } else if (input$pivotOption == "Pivot Wider" && input$pivotNamesTo != "" && input$pivotValuesTo != "") {
        df <- df %>% spread(key = !!sym(input$pivotNamesTo), value = !!sym(input$pivotValuesTo))
      }
    }, error = function(e) {
      showModal(modalDialog(
        title = "Pivot Error",
        paste("Error during pivoting:", e$message),
        easyClose = TRUE,
        footer = NULL
      ))
    })
    
    data(df)
    df
  })
  
  output$dataTable <- renderDT({
    req(processedData())
    datatable(processedData(), options = list(pageLength = 5, autoWidth = TRUE))
  })
  
  output$downloadData <- downloadHandler(
    filename = function() { paste0("processed_data_", Sys.Date(), ".csv") },
    content = function(file) { rio::export(processedData(), file) }
  )
}



shinyApp(ui, server)
