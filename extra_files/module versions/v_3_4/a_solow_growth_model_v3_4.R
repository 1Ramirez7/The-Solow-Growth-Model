library(shiny)
library(ggplot2)
library(DT)    # for interactive table
library(zip)   # to create ZIP files in download handler
library(bslib)
library(rio)   # to read the Excel file # v2.2

source("modules/simulation_calc.R") # module for simulate_solow # v3.4

#--- Helper function to create each experiment set. ---
initializeExperimentSet <- function(
    expSuffix,         # label like "1", "2", "3", or "4"
    paramNameInput,    # e.g. "param_name", "param_name2"
    newValueInput,     # e.g. "value", "value2"
    startPeriodInput,  # e.g. "start_period", "start_period2"
    lengthInput,       # e.g. "length", "length2"
    addExpButton,      # e.g. "add_experiment", "add_experiment2"
    dtOutput,          # e.g. "experimentsTable", "experimentsTable2"
    deleteRowInput,    # internal ID prefix for deletion, e.g. "delete_1", "delete_2"
    input, output, session
) {
  
  # A reactiveVal to hold the table of experiments for this set
  rv <- reactiveVal(
    data.frame(
      param        = character(0),
      value    = numeric(0),
      start_period = integer(0),
      length       = integer(0),
      stringsAsFactors = FALSE
    )
  )
  
  # Add new experiment row when the user clicks
  observeEvent(input[[addExpButton]], {
    new_experiment <- data.frame(
      param        = input[[paramNameInput]],
      value    = input[[newValueInput]],
      start_period = input[[startPeriodInput]],
      length       = input[[lengthInput]],
      stringsAsFactors = FALSE
    )
    rv( rbind(rv(), new_experiment) )
  })
  
  # Render the experiments table with "Delete" buttons
  output[[dtOutput]] <- renderDT({
    exps <- rv()
    if (nrow(exps) == 0) return(NULL)
    
    deleteButtons <- sapply(seq_len(nrow(exps)), function(i) {
      as.character(
        actionButton(
          inputId = paste0("delete_", expSuffix, "_", i),
          label = "Delete",
          class = "btn btn-danger btn-sm",
          onclick = paste0(
            'Shiny.setInputValue("delete_', expSuffix, '", this.id, {priority: "event"})'
          )
        )
      )
    })
    
    exps$Delete <- deleteButtons
    
    datatable(
      exps,
      escape = FALSE,
      selection = "none",
      options   = list(pageLength = 5)
    )
  }, server = FALSE)
  
  # Handle row deletions
  observeEvent(input[[paste0("delete_", expSuffix)]], {
    row_str <- gsub(paste0("delete_", expSuffix, "_"), "", input[[paste0("delete_", expSuffix)]])
    row_num <- as.numeric(row_str)
    
    exps <- rv()
    if (!is.na(row_num) && row_num >= 1 && row_num <= nrow(exps)) {
      exps <- exps[-row_num, ]
      rv(exps)
    }
  })
  
  return(rv)
}

#--------------------------------------------------------
#     UI
#--------------------------------------------------------
# app layout and input controls version1
ui <- page_sidebar( # version
  title = "Solow-Romer Macro Simulation",
  sidebar = sidebar( # all the input controls
    width = 600, # edit sidebar width. This is not friendly with smaller screens. I was having trouble with the sidebar overlapping with tabs so I added this to default a sidebar lenght.
    h4("Simulation Parameters"),
    sliderInput("Simulation_Period", "Simulation Period (yrs)", 
                min = 20, max = 150, value = 50, step = 1),
    sliderInput("s", "Savings Rate (s)", min = 0.001, max = 1, value = 0.2, step = 0.01), # v2.3 change to min .001 due to div by zero error
    sliderInput("delta", "Depreciation Rate (delta)", min = 0, max = .5, value = 0.15, step = 0.01),
    sliderInput("n", "n", min = 0, max = 0.1, value = 0.02, step = 0.001),
    sliderInput("z", "Parameter z", min = 0, max = 0.1, value = 0.02, step = 0.001),
    sliderInput("l", "Parameter l", min = 0, max = 0.99, value = 0.1, step = 0.01), 
    selectInput("A", "Initial value for A", choices = c(1, 100, 1000), selected = 1), # 343
    selectInput("L", "Initial value for L", choices = c(1, 100, 1000), selected = 1), # 343
    
    h4("Experiments"),
    
    # -- ADD TABSETPANEL HERE -- # v3.3.1
    do.call(tabsetPanel, c(id = "experiment_tabs",
                           lapply(1:4, function(i) {
                             tabPanel(
                               paste0("Experiments Set ", i),
                               selectInput(paste0("param_name", i), paste0("Quantitative exp (s, delta, n, z, l) Set ", i),
                                           choices = c("s", "delta", "n", "z", "l"),
                                           selected = "s"),
                               numericInput(paste0("value", i), paste0("New Value Set ", i), value = 0, step = 0.01),
                               numericInput(paste0("start_period", i), paste0("Start Period Set ", i), value = 10, step = 1),
                               numericInput(paste0("length", i), paste0("Length of Effect Set ", i), value = 200, step = 1),
                               actionButton(paste0("add_experiment", i), paste0("Add Exp ", i)),
                               fileInput(paste0("upload_file_", i), paste0("Upload Excel for Exp ", i), accept = c(".xlsx", ".xls")),
                               div(style = "width: 400px;", DTOutput(paste0("experimentsTable", i)))
                             )
                           })
    )),
    # -- END TABSETPANEL -- # v3.3.1
    
    checkboxInput("show_counter_visual",    "Show Counterfactual",       value = TRUE),
    checkboxInput("show_second_exp_visual", "Show Second Experiment",     value = FALSE), # v3.3
    checkboxInput("show_third_exp_visual","Show Third Experiment",    value = FALSE), # v3.3
    checkboxInput("show_fourth_exp_visual", "Show Fourth Experiment",     value = FALSE), # v3.3
    
    actionButton("simulate", "Simulate"),
    h4("Download Results"),
    downloadButton("downloadPlots", "Download All Plots as PNG (ZIP)"),
    downloadButton("downloadData", "Download Data as CSV")
  ),
  mainPanel( # tab controls. v1
    tabsetPanel(
      tabPanel("World", # v 2.2
               # adding option to get average savings rate
               selectInput("selectedSheet", "Select Region", choices = NULL),
               selectInput("selectedColumn", "Select Country", choices = NULL),
               numericInput("startYear", "Start Year", value = 1960),
               numericInput("endYear",   "End Year",   value = 2023),
               actionButton("calculateAvg", "Get Avg Savings Rate 's'"),
               verbatimTextOutput("avgSavingsRate")  # displays the avg s calculated
      ),
      tabPanel("Plots",
               div(style = "width: 1800px; margin: auto;",
                   fluidRow(
                     column(6, plotOutput("plot_K", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_Y", width = "900px", height = "600px"))
                   ),
                   fluidRow(
                     column(6, plotOutput("plot_little_k", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_percent_delta_k", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_MPK", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_MPL", width = "900px", height = "600px"))
                   )
               )
      ),
      tabPanel("Log/Ratio scale",
               div(style = "width: 1800px; margin: auto;",
                   fluidRow(
                     column(6, plotOutput("plot_log_L", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_log_K", width = "900px", height = "600px"))
                   ),
                   fluidRow(
                     column(6, plotOutput("plot_log_Y", width = "900px", height = "600px")),
                     column(6, plotOutput("plot_log_A", width = "900px", height = "600px"))
                   )
               )
      ),
      tabPanel("Program Results", # v3.3.5
               tabsetPanel(
                 tabPanel("First Scenario", tableOutput("results_first_exp_df")),
                 tabPanel("Counterfactual", tableOutput("results_counterfactual_df")),
                 tabPanel("Second Scenario", tableOutput("results_second_exp_df")),
                 tabPanel("Third Scenario", tableOutput("results_third_exp_df")),
                 tabPanel("Fourth Scenario", tableOutput("results_fourth_exp_df"))
               )
      ) # v3.3.5  # v3
    )
  )
)

#--------------------------------------------------------
#     SERVER # version1
#--------------------------------------------------------
server <- function(input, output, session) {
  
  # adding option for Savings rate average for list of countries. 
  excel_list <- import_list("data/savings_rate_y.xlsx") # v2.2
  
  # Populate region choices
  observe({  # v2.2
    updateSelectInput(session, "selectedSheet", choices = names(excel_list), selected = names(excel_list)[1])
  })
  observeEvent(input$selectedSheet, {  # Update column choices  # v2.2
    df <- excel_list[[input$selectedSheet]]
    updateSelectInput(session, "selectedColumn", choices = setdiff(names(df), "year"), selected = setdiff(names(df), "year")[1])
  })
  
  # find the available years
  observeEvent(input$selectedColumn, { # v2.2
    df <- excel_list[[input$selectedSheet]]
    yearVals <- df$year
    yVals <- as.numeric(df[[input$selectedColumn]])
    valid <- !is.na(yearVals) & !is.na(yVals)
    if (!any(valid)) return()
    validYears <- yearVals[valid]
    minY <- min(validYears)
    maxY <- max(validYears)
    updateNumericInput(session, "startYear", min = minY, max = maxY, value = minY)
    updateNumericInput(session, "endYear",   min = minY, max = maxY, value = maxY)
  })
  
  # Calculate average within the chosen year range
  observeEvent(input$calculateAvg, {
    df <- excel_list[[input$selectedSheet]]
    yearVals <- df$year
    yVals <- as.numeric(df[[input$selectedColumn]])
    valid <- !is.na(yearVals) & !is.na(yVals)
    dfValid <- df[valid, ]
    
    subsetData <- dfValid[dfValid$year >= input$startYear & dfValid$year <= input$endYear, ]
    if (nrow(subsetData) == 0) {  # Check if country has any data
      output$avgSavingsRate <- renderPrint("Country Data Not Available") 
      return()
    }
    
    avgVal <- (mean(subsetData[[input$selectedColumn]]))/100
    updateSliderInput(session, "s", value = avgVal)
    output$avgSavingsRate <- renderPrint(paste("Average Savings Rate:", avgVal)) # Display the average
  }) # v2.2
  
  #----------------------------------
  # EXPERIMENTS TABLE MANAGEMENT # v3 
  #----------------------------------
  # This code along w/ the initializeExperiment function help loop in experiment simulations. 
  experiment_sets <- lapply(1:4, function(i) { # v3.2 looping exp tables
    initializeExperimentSet(
      expSuffix        = as.character(i),
      paramNameInput   = paste0("param_name", i),
      newValueInput    = paste0("value", i),
      startPeriodInput = paste0("start_period", i),
      lengthInput      = paste0("length", i),
      addExpButton     = paste0("add_experiment", i),
      dtOutput         = paste0("experimentsTable", i),
      deleteRowInput   = paste0("delete_", i),
      input = input, output = output, session = session
    )
  })
  names(experiment_sets) <- paste0("experiments", 1:4)  # v3.2 looping exp tables
  
  
  # v3.1: adding excel upload for faster param inputs
  # v3.2: loop this to match expe loop code
  for (i in 1:4) {
    local({
      set_index <- i
      observeEvent(input[[paste0("upload_file_", set_index)]], {
        req(input[[paste0("upload_file_", set_index)]])
        df_uploaded <- import(input[[paste0("upload_file_", set_index)]]$datapath)
        
        # Validate columns
        required_cols <- c("param", "value", "start_period", "length")
        if (!all(required_cols %in% names(df_uploaded))) {
          showModal(modalDialog(
            title = "Error",
            "The uploaded file must contain columns: param, value, start_period, length.",
            easyClose = TRUE
          ))
          return(NULL)
        }
        
        # Append
        current_data <- experiment_sets[[paste0("experiments", set_index)]]()
        updated_data <- rbind(current_data, df_uploaded)
        experiment_sets[[paste0("experiments", set_index)]](updated_data)
      }) # v3.1 and v3.2 end
    })
  }
  
  #----------------------------------
  # EVENT-REACTIVE RESULTS
  #----------------------------------
  # v3.2 edit to loop with lapply exp tables
  
  simulate_first_exp_calculations <- eventReactive(input$simulate, { # v2.1
    # If first set is empty, show a modal (original logic)
    if (nrow(experiment_sets$experiments1()) == 0) {
      showModal(modalDialog(
        title = "Error",
        "Please add at least one experiment in Set 1 before running the simulation.",
        easyClose = TRUE,
        footer = NULL
      ))
      return(NULL)
    }
    num_periods <- input$Simulation_Period + 1
    A_init <- as.numeric(input$A)
    L_init <- as.numeric(input$L)
    
    # v3: streamline this section of the simulate_solow.
    simulate_solow(num_periods, input$s, input$delta, input$n, input$z, input$l,
                   A_init, L_init, experiments_df = experiment_sets$experiments1()) # v3
  })
  
  simulate_counter_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    A_init <- as.numeric(input$A)
    L_init <- as.numeric(input$L)
    simulate_solow(num_periods, input$s, input$delta, input$n, input$z, input$l,
                   A_init, L_init, experiments_df = NULL) # v3
  })
  
  # v3: adding the 3 extra experiment simulations
  simulate_second_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    A_init <- as.numeric(input$A)
    L_init <- as.numeric(input$L)
    experiments_df_to_use <- if (nrow(experiment_sets$experiments2()) == 0) NULL else experiment_sets$experiments2()
    simulate_solow(num_periods, input$s, input$delta, input$n, input$z, input$l,
                   A_init, L_init, experiments_df = experiments_df_to_use)
  })
  
  simulate_third_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    A_init <- as.numeric(input$A)
    L_init <- as.numeric(input$L)
    experiments_df_to_use <- if (nrow(experiment_sets$experiments3()) == 0) NULL else experiment_sets$experiments3()
    simulate_solow(num_periods, input$s, input$delta, input$n, input$z, input$l,
                   A_init, L_init, experiments_df = experiments_df_to_use)
  })
  
  simulate_fourth_exp_calculations <- eventReactive(input$simulate, {
    num_periods <- input$Simulation_Period + 1
    A_init <- as.numeric(input$A)
    L_init <- as.numeric(input$L)
    experiments_df_to_use <- if (nrow(experiment_sets$experiments4()) == 0) NULL else experiment_sets$experiments4()
    simulate_solow(num_periods, input$s, input$delta, input$n, input$z, input$l,
                   A_init, L_init, experiments_df = experiments_df_to_use)
  }) 
  
  # v3.2 updated to be loop with exp tables
  
  
  #----------------------------------
  # 4. OUTPUT: TABLES - data frames
  #----------------------------------
  
  # v3 not much change from before v3c for results, and simulate_counter_exp_calculations, but
  # added the 3 results tables for the other experiments
  output$results_first_exp_df <- renderTable({req(simulate_first_exp_calculations()); 
    simulate_first_exp_calculations()}, rownames = FALSE) # v3
  output$results_counterfactual_df <- renderTable({ req(simulate_counter_exp_calculations()) ; 
    simulate_counter_exp_calculations() }, rownames = FALSE) # v3
  output$results_second_exp_df <- renderTable({ req(simulate_second_exp_calculations()); 
    simulate_second_exp_calculations() }, rownames = FALSE) # v3
  output$results_third_exp_df<- renderTable({ req(simulate_third_exp_calculations()); 
    simulate_third_exp_calculations() }, rownames = FALSE) # v3
  output$results_fourth_exp_df <- renderTable({ req(simulate_fourth_exp_calculations()); 
    simulate_fourth_exp_calculations() }, rownames = FALSE) # v3
  
  #---------------------------
  # 5. PLOTS # v3.3
  #---------------------------
  plot_theme <- theme_bw() + 
    theme(
      plot.title        = element_text(hjust = 0.5),
      axis.title        = element_text(size = 12),
      axis.text         = element_text(size = 10),
      panel.grid.major  = element_line(color = "grey80"),
      panel.grid.minor  = element_blank(),
      strip.background  = element_rect(fill = "grey90", color = "grey90")
    )
  
  # Extended make_plot to include optional 3rd,4th,5th data
  make_plot <- function(
    data_exp,      # "First Scenario" (the main one from Set 1)
    data_default_counter,   # "Counterfactual" (counterfactual)
    data_third = NULL,
    data_fourth = NULL,
    data_fifth = NULL,
    y_var, title, y_label,
    show_counter_visual = TRUE,
    show_second_exp_visual = FALSE,
    show_third_exp_visual = FALSE,
    show_fourth_exp_visual = FALSE
  ) {
    
    # Base plot with the main experiment data
    p <- ggplot(data_exp, aes(x = Period, y = .data[[y_var]])) +
      geom_line(aes(color = "First Scenario")) +
      geom_point(data = if(nrow(data_exp) > 10) { # v3.3.3 adding point limit
        data_exp[round(seq(1, nrow(data_exp), length.out = 10)), ]
      } else {
        data_exp
      }, aes(color = "First Scenario")) + # v3.3.3 end of adding point limit
      scale_color_manual(values = c("First Scenario" = "blue",
                                    "Counterfactual" = "red",
                                    "Second Scenario"  = "green",
                                    "Third Scenario" = "purple",
                                    "Fourth Scenario"  = "orange")) +
      ggtitle(title) + xlab("Period") + ylab(y_label) +
      plot_theme
    
    # Optionally add no-exp line
    if (show_counter_visual && !is.null(data_default_counter)) {
      p <- p +
        geom_line(data = data_default_counter, 
                  aes(x = Period, y = .data[[y_var]], color = "Counterfactual"), 
                  linetype = "dashed") +
        geom_point(data = if(nrow(data_default_counter) > 12) { # v3.3.3 adding point limit
          tmp <- data_default_counter[-1, ] # remove n rows
          tmp[round(seq(1, nrow(tmp), length.out = 12)), ]
        } else {
          data_default_counter[-1, ] # end v3.3.3 limit points
        }, aes(x = Period, y = .data[[y_var]], color = "Counterfactual"), 
        shape = 1, size = 2)
    }
    
    # Optionally add Second Scenario
    if (show_second_exp_visual && !is.null(data_third)) {
      p <- p +
        geom_line(data = data_third,
                  aes(x = Period, y = .data[[y_var]], color = "Second Scenario")) +
        geom_point(data = if(nrow(data_third) > 14) { # v3.3.3 adding point limit
          tmp <- data_third[-(1:3), ]
          tmp[round(seq(1, nrow(tmp), length.out = 14)), ]
        } else {
          data_third[-(1:3), ] # end v3.3.3 
        }, aes(x = Period, y = .data[[y_var]], color = "Second Scenario"), size = 1)
    }
    
    # Optionally add Third Scenario
    if (show_third_exp_visual && !is.null(data_fourth)) {
      p <- p +
        geom_line(data = data_fourth,
                  aes(x = Period, y = .data[[y_var]], color = "Third Scenario")) +
        geom_point(data = if(nrow(data_fourth) > 16) { # v3.3.3 adding point limit
          tmp <- data_fourth[-(1:5), ]
          tmp[round(seq(1, nrow(tmp), length.out = 16)), ]
        } else {
          data_fourth[-(1:5), ] # end v3.3.3 
        }, aes(x = Period, y = .data[[y_var]], color = "Third Scenario"), size = 1)
    }
    
    # Optionally add Fourth Scenario
    if (show_fourth_exp_visual && !is.null(data_fifth)) {
      p <- p +
        geom_line(data = data_fifth,
                  aes(x = Period, y = .data[[y_var]], color = "Fourth Scenario")) +
        geom_point(data = if(nrow(data_fifth) > 18) { # v3.3.3 adding point limit
          tmp <- data_fifth[-(1:7), ]
          tmp[round(seq(1, nrow(tmp), length.out = 18)), ]
        } else {
          data_fifth[-(1:7), ] # end v3.3.3 
        }, aes(x = Period, y = .data[[y_var]], color = "Fourth Scenario"), size = 1)
    }
    
    p
  }
  
  # Store plots for download
  plot_objects <- reactiveValues()
  
  output$plot_K <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "K",
      title         = "Capital (K) Over Time",
      y_label       = "Capital (K)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_K <- p
    p
  }, res = 100)
  
  output$plot_Y <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "Y",
      title         = "Output (Y) Over Time",
      y_label       = "Output (Y)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_Y <- p
    p
  }, res = 100)
  
  output$plot_little_k <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "little_k",
      title         = "Capital Efficiency Units",
      y_label       = "k (Capital Efficiency Units)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_little_k <- p
    p
  }, res = 100)
  
  output$plot_percent_delta_k <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "percent_delta_k",
      title         = "Percent Change in Capital Over Time",
      y_label       = "Δk/k (Percent)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_percent_delta_k <- p
    p
  }, res = 100)
  
  output$plot_MPK <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "MPK",
      title         = "Interest Rate Return on Capital", # r = MPK # v3.3.1
      y_label       = "r",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_MPK <- p
    p
  }, res = 100)
  
  output$plot_MPL <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "MPL",
      title         = "Wage Rate",
      y_label       = "w",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_MPL <- p
    p
  }, res = 100)
  
  output$plot_log_L <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "log_L",
      title         = "Log of Labor (L) Over Time",
      y_label       = "Log(L)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_log_L <- p
    p
  }, res = 100)
  
  output$plot_log_K <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "log_K",
      title         = "Log of Capital (K) Over Time",
      y_label       = "Log(K)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_log_K <- p
    p
  }, res = 100)
  
  output$plot_log_Y <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "log_Y",
      title         = "Log of Output (Y) Over Time",
      y_label       = "Log(Y)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_log_Y <- p
    p
  }, res = 100)
  
  output$plot_log_A <- renderPlot({
    req(simulate_first_exp_calculations(), simulate_counter_exp_calculations(), simulate_second_exp_calculations(), simulate_third_exp_calculations(), simulate_fourth_exp_calculations())
    p <- make_plot(
      data_exp      = simulate_first_exp_calculations(),
      data_default_counter   = simulate_counter_exp_calculations(),
      data_third    = simulate_second_exp_calculations(),
      data_fourth   = simulate_third_exp_calculations(),
      data_fifth    = simulate_fourth_exp_calculations(),
      y_var         = "log_A",
      title         = "Log of TFP (A) Over Time",
      y_label       = "Log(A)",
      show_counter_visual   = input$show_counter_visual,
      show_second_exp_visual = input$show_second_exp_visual,
      show_third_exp_visual = input$show_third_exp_visual,
      show_fourth_exp_visual  = input$show_fourth_exp_visual
    )
    plot_objects$plot_log_A <- p
    p
  }, res = 100) # v3.3: this whole plot code is new with v3.3
  
  
  #----------------------------------
  # DOWNLOAD HANDLERS
  #----------------------------------
  output$downloadData <- downloadHandler( # version1
    filename = function() {
      paste0("results_", Sys.Date(), ".csv")
    },
    content = function(file) {req(simulate_first_exp_calculations()) # v2.1
      write.csv(simulate_first_exp_calculations(), file, row.names = FALSE)
    }
  )
  
  output$downloadPlots <- downloadHandler( # version1
    filename = function() { 
      paste0("plots_", Sys.Date(), ".zip")
    },
    content = function(file) {
      temp_dir <- tempdir() # temp file storage but new shiny update change the way file downloads happen so this might not be needed now
      file_paths <- c()
      
      # Use the stored plot objects version2
      plots <- reactiveValuesToList(plot_objects) # Convert reactiveValues to list
      plots <- Filter(Negate(is.null), plots) # Remove NULL entries
      
      for (plot_name in names(plots)) { # Save each plot to a PNG version1
        plot_file <- file.path(temp_dir, paste0(plot_name, ".png"))
        ggsave(plot_file, plots[[plot_name]], device = "png")
        file_paths <- c(file_paths, plot_file)
      }
      zipr(zipfile = file, files = file_paths) # zip em. v1
    }
  )
}

shinyApp(ui = ui, server = server)

# v3.3.3.1

# reverted back to version 3.3.3 and put all df in one tabpanel 
# reverted back due to complications in calculations for the param_mods

# I basically just need to mirror my exp table code
# The main difference is this mods_df will have a function/def/helper function that allows it to be called during the calculations.
# similar to if i == 1 then return 0 or inital etc. this one will be
# if param_mods has value for said calculation and current period then return the param_mod value.
# this one can probably be before the i == 0 since this mods sometimes change period 0
# this code needs to be independent so it can be removed without affecting the model. (ideally)
# I made some good code for this 3/7/2025 with ai help but it was wrong so better to start from scratch

# my next model will be #v3.4