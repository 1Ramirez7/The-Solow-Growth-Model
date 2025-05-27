#############################
# MODULE: WORLD DATA       #
#############################
mod_world_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    selectInput(ns("selectedSheet"), "Select Region", choices = NULL),
    selectInput(ns("selectedColumn"), "Select Country", choices = NULL),
    numericInput(ns("startYear"), "Start Year", value = 1960),
    numericInput(ns("endYear"),   "End Year",   value = 2023),
    actionButton(ns("calculateAvg"), "Get Avg Savings Rate 's'"),
    verbatimTextOutput(ns("avgSavingsRate"))
  )
}

mod_world_data_server <- function(id, excel_list, updateSavingsRate) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Populate region (sheet) choices
    observe({
      updateSelectInput(session, "selectedSheet",
                        choices = names(excel_list),
                        selected = names(excel_list)[1])
    })
    
    # 2. Update country choices after region changes
    observeEvent(input$selectedSheet, {
      df <- excel_list[[input$selectedSheet]]
      possibleCountries <- setdiff(names(df), "year")
      updateSelectInput(session, "selectedColumn",
                        choices = possibleCountries,
                        selected = possibleCountries[1])
    })
    
    # 3. Update numeric range for years
    observeEvent(input$selectedColumn, {
      req(input$selectedSheet)
      df <- excel_list[[input$selectedSheet]]
      valid <- !is.na(df$year) & !is.na(df[[input$selectedColumn]])
      if (!any(valid)) return()
      validYears <- df$year[valid]
      updateNumericInput(session, "startYear",
                         min = min(validYears), max = max(validYears),
                         value = min(validYears))
      updateNumericInput(session, "endYear",
                         min = min(validYears), max = max(validYears),
                         value = max(validYears))
    })
    
    # 4. Calculate average within year range
    observeEvent(input$calculateAvg, {
      df <- excel_list[[input$selectedSheet]]
      valid <- !is.na(df$year) & !is.na(df[[input$selectedColumn]])
      dfValid <- df[valid, ]
      
      subsetData <- dfValid[dfValid$year >= input$startYear & dfValid$year <= input$endYear, ]
      if (nrow(subsetData) == 0) {
        output$avgSavingsRate <- renderPrint("Country Data Not Available")
        return()
      }
      
      avgVal <- mean(subsetData[[input$selectedColumn]]) / 100
      output$avgSavingsRate <- renderPrint(paste("Average Savings Rate:", avgVal))
      
      # This calls back to the main server to update the slider input for s
      updateSavingsRate(avgVal)
    })
  })
}
