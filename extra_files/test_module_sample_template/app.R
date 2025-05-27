library(shiny)
library(ggplot2)
library(DT)
library(zip)
library(bslib)
library(rio)


source("R/mod_world_data.R")
source("R/mod_experiments.R")
source("R/mod_simulation_plots.R")

#############################
# MAIN UI                   #
#############################

ui <- page_sidebar(
  title = "Solow-Romer Macro Simulation",
  sidebar = sidebar(
    width = 500,
    h4("Simulation Parameters"),
    sliderInput("Simulation_Period", "Simulation Period (yrs)", min = 20, max = 150, value = 50, step = 1),
    sliderInput("s", "Savings Rate (s)", min = 0.001, max = 1, value = 0.2, step = 0.01),
    sliderInput("delta", "Depreciation Rate (delta)", min = 0, max = 0.5, value = 0.15, step = 0.01),
    sliderInput("n", "n", min = 0, max = 0.1, value = 0.02, step = 0.001),
    sliderInput("z", "Parameter z", min = 0, max = 0.1, value = 0.02, step = 0.001),
    sliderInput("l", "Parameter l", min = 0, max = 0.99, value = 0.1, step = 0.01),
    selectInput("A", "Initial value for A", choices = c(1, 100, 1000), selected = 1),
    selectInput("L", "Initial value for L", choices = c(1, 100, 1000), selected = 1),
    
    # Include the experiments module UI
    mod_experiments_ui("exp_ui"),
    
    h4("Download Results"),
    downloadButton("downloadPlots", "Download All Plots (ZIP)"),
    downloadButton("downloadData", "Download Data as CSV")
  ),
  
  mainPanel(
    tabsetPanel(
      # The "World" tab is from world data module
      tabPanel("World",
               mod_world_data_ui("world_ui")
      ),
      # The "Plots/Tables" tabs:
      tabPanel("Simulation Results",
               mod_simulation_plots_ui("sim_plots_ui")
      )
    )
  )
)

#############################
# MAIN SERVER               #
#############################
server <- function(input, output, session) {
  
  #---------------------------
  # 1) WORLD DATA MODULE
  #---------------------------
  # load xlsx once
  excel_list <- import_list("data/savings_rate_y.xlsx")
  
  # define a helper that the module calls to update the 's' slider
  updateSavingsRate <- function(val) {
    updateSliderInput(session, "s", value = val)
  }
  
  mod_world_data_server("world_ui", excel_list, updateSavingsRate)
  
  #---------------------------
  # 2) EXPERIMENTS MODULE
  #---------------------------
  exp_return <- mod_experiments_server("exp_ui")
  #   exp_return$experiments()  -> data.frame of experiments
  #   exp_return$simulateBtn()  -> 'simulate' button
  #   exp_return$showNoExpFlag() -> T/F if user toggles "show counterfactual"
  
  #---------------------------
  # 3) SIM/RESULTS MODULE
  #---------------------------
  # We pass a "reactive list" of the key inputs to the module.  
  # so that when user changes e.g. input$s, module sees it automatically.
  inputParams <- reactive({
    list(
      Simulation_Period = input$Simulation_Period,
      s = input$s,
      delta = input$delta,
      n = input$n,
      z = input$z,
      l = input$l,
      A = input$A,
      L = input$L
    )
  })
  
  sim_return <- mod_simulation_plots_server(
    id                = "sim_plots_ui",
    experimentsReactive = exp_return$experiments,
    simulateBtn       = exp_return$simulateBtn,
    showNoExpFlag     = exp_return$showNoExpFlag,
    inputParams       = inputParams
  )
  
  #---------------------------
  # 4) CONNECT DOWNLOADS
  #---------------------------
  # We route the "downloadData" & "downloadPlots" in the module to 
  # the main UI's download buttons. This way, they're connected:
  
  output$downloadData   <- renderUI({})
  output$downloadPlots  <- renderUI({})
  
  # Just pass them through from the module:
  # We'll do it by assigning them:
  output$downloadData   <- sim_return$downloadData
  output$downloadPlots  <- sim_return$downloadPlots
}

shinyApp(ui, server)