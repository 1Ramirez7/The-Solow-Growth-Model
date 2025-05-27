#############################
# MODULE: SIM + PLOTS      #
#############################
mod_simulation_plots_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tabsetPanel(
      tabPanel("Plots",
               div(style = "width: 1800px; margin: auto;",
                   fluidRow(
                     column(6, plotOutput(ns("plot_K"), width = "900px", height = "600px")),
                     column(6, plotOutput(ns("plot_Y"), width = "900px", height = "600px"))
                   ),
                   fluidRow(
                     column(6, plotOutput(ns("plot_little_k"), width = "900px", height = "600px")),
                     column(6, plotOutput(ns("plot_percent_delta_k"), width = "900px", height = "600px")),
                     column(6, plotOutput(ns("plot_MPK"), width = "900px", height = "600px")),
                     column(6, plotOutput(ns("plot_MPL"), width = "900px", height = "600px"))
                   )
               )
      ),
      tabPanel("Log/Ratio scale",
               div(style = "width: 1800px; margin: auto;",
                   fluidRow(
                     column(6, plotOutput(ns("plot_log_L"), width = "900px", height = "600px")),
                     column(6, plotOutput(ns("plot_log_K"), width = "900px", height = "600px"))
                   ),
                   fluidRow(
                     column(6, plotOutput(ns("plot_log_Y"), width = "900px", height = "600px")),
                     column(6, plotOutput(ns("plot_log_A"), width = "900px", height = "600px"))
                   )
               )
      ),
      tabPanel("Table", tableOutput(ns("resultsTable"))),
      tabPanel("No Experiments", tableOutput(ns("resultsNoExpTable")))
    )
  )
}

mod_simulation_plots_server <- function(id, 
                                        experimentsReactive, 
                                        simulateBtn, 
                                        showNoExpFlag,
                                        # pass the numeric inputs from outside:
                                        inputParams # a reactive list: s, delta, etc.
) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    ################################
    # 1. The core simulation code  #
    ################################
    simulate_solow <- function(num_periods, s, delta, n, z, l, A_init, L_init, experiments_df = NULL) {
      # Create a parameter data frame for all periods
      params <- data.frame(
        Period   = 0:(num_periods - 1),
        s        = rep(s, num_periods),
        delta    = rep(delta, num_periods),
        n        = rep(n, num_periods),
        z        = rep(z, num_periods),
        l        = rep(l, num_periods),
        delta_k  = rep(0.0, num_periods),
        percent_delta_k = rep(0.0, num_periods),
        little_k = rep(0.0, num_periods),
        A        = rep(A_init, num_periods),
        L        = rep(L_init, num_periods),
        K        = rep(0.0, num_periods),
        Y        = rep(0.0, num_periods)
      )
      
      # initial "k" guess:
      k_star_initial <- (s / (delta + n + z*l))^(1.5) * (1 - l)
      params$little_k[1] <- k_star_initial
      
      # apply any experiments
      if (!is.null(experiments_df) && nrow(experiments_df) > 0) {
        for (exp_i in seq_len(nrow(experiments_df))) {
          start <- experiments_df$start_period[exp_i] + 1
          end   <- min(start + experiments_df$length[exp_i] - 1, num_periods)
          param_name <- experiments_df$param[exp_i]
          new_value  <- experiments_df$new_value[exp_i]
          
          params[start:end, param_name] <- new_value
        }
      }
      
      # run iteration
      for (i in seq_len(num_periods)) {
        if (i == 1) {
          params$delta_k[i] <- 0
        } else {
          params$delta_k[i] <- params$s[i]*( (1 - params$l[i])^(2/3) )*(params$little_k[i-1]^(1/3)) -
            (params$z[i]*params$l[i] + params$n[i] + params$delta[i])*params$little_k[i-1]
          
          params$little_k[i] <- params$little_k[i-1] + params$delta_k[i]
          
          # A, L
          params$A[i] <- params$A[i-1]*(1 + params$z[i]*params$l[i])
          params$L[i] <- params$L[i-1]*(1 + params$n[i])
        }
        # K, Y
        params$K[i] <- params$little_k[i]*params$A[i]*params$L[i]
        params$Y[i] <- (1 - params$l[i])^(2/3) * params$K[i]^(1/3) * (params$A[i]*params$L[i])^(2/3)
      }
      
      params$MPL <- (1 - params$l)^(1 - 1/3) * (params$K^(1/3)) * ((params$A*params$L)^(1 - 1/3)) / params$L
      params$MPK <- (1/3)*(1 - params$l)^(1 - 1/3) * (params$K^(1/3)) * ((params$A*params$L)^(1 - 1/3)) / params$K
      
      params$log_L <- log(params$L)
      params$log_K <- log(params$K)
      params$log_Y <- log(params$Y)
      params$log_A <- log(params$A)
      params$percent_delta_k <- (params$delta_k / params$little_k) * 100
      
      params
    }
    
    ################################
    # 2. Reactives for results     #
    ################################
    # main simulation with experiments
    results <- eventReactive(simulateBtn(), {
      exps <- experimentsReactive()  # from mod_experiments
      if (nrow(exps) == 0) {
        showModal(modalDialog(
          title = "Error",
          "Please add at least one experiment before running the simulation.",
          easyClose = TRUE
        ))
        return(NULL)
      }
      p <- inputParams() # see below for how we define it in main
      simulate_solow(
        num_periods = p$Simulation_Period + 1,
        s           = p$s,
        delta       = p$delta,
        n           = p$n,
        z           = p$z,
        l           = p$l,
        A_init      = as.numeric(p$A),
        L_init      = as.numeric(p$L),
        experiments_df = exps
      )
    })
    
    # simulation *without* experiments
    results_no_exp <- eventReactive(simulateBtn(), {
      p <- inputParams()
      simulate_solow(
        num_periods = p$Simulation_Period + 1,
        s           = p$s,
        delta       = p$delta,
        n           = p$n,
        z           = p$z,
        l           = p$l,
        A_init      = as.numeric(p$A),
        L_init      = as.numeric(p$L),
        experiments_df = NULL
      )
    })
    
    ################################
    # 3. Tables                    #
    ################################
    output$resultsTable <- renderTable({
      req(results())
      results()
    }, rownames = FALSE)
    
    output$resultsNoExpTable <- renderTable({
      req(results_no_exp())
      results_no_exp()
    }, rownames = FALSE)
    
    ################################
    # 4. Plots                     #
    ################################
    plot_theme <- theme_bw() +
      theme(plot.title = element_text(hjust = 0.5),
            axis.title = element_text(size = 12),
            axis.text = element_text(size = 10))
    
    make_plot <- function(data_exp, data_no_exp, y_var, title, y_label, show_no_exp) {
      p <- ggplot(data_exp, aes(x = Period, y = .data[[y_var]])) +
        geom_line(aes(color = "With Experiments")) +
        geom_point(aes(color = "With Experiments")) +
        scale_color_manual(values = c("With Experiments" = "blue",
                                      "Without Experiments" = "red")) +
        ggtitle(title) + xlab("Period") + ylab(y_label) + plot_theme
      
      if (show_no_exp) {
        p <- p +
          geom_line(data = data_no_exp,
                    aes(x = Period, y = .data[[y_var]], color = "Without Experiments"),
                    linetype = "dashed") +
          geom_point(data = data_no_exp,
                     aes(x = Period, y = .data[[y_var]], color = "Without Experiments"),
                     shape = 1, size = 3)
      }
      p
    }
    
    # store the plots in reactiveValues for downloads
    plot_objects <- reactiveValues()
    
    output$plot_K <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "K", "Capital (K)", "K", showFlag)
      plot_objects$plot_K <- p
      p
    })
    
    output$plot_Y <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "Y", "Output (Y)", "Y", showFlag)
      plot_objects$plot_Y <- p
      p
    })
    
    output$plot_little_k <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "little_k", "k (Eff. Capital)", "k", showFlag)
      plot_objects$plot_little_k <- p
      p
    })
    
    output$plot_percent_delta_k <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "percent_delta_k", "%Δk", "%Δk", showFlag)
      plot_objects$plot_percent_delta_k <- p
      p
    })
    
    output$plot_MPL <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "MPL", "MPL", "MPL", showFlag)
      plot_objects$plot_MPL <- p
      p
    })
    
    output$plot_MPK <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "MPK", "MPK", "MPK", showFlag)
      plot_objects$plot_MPK <- p
      p
    })
    
    output$plot_log_L <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "log_L", "log(L)", "log(L)", showFlag)
      plot_objects$plot_log_L <- p
      p
    })
    
    output$plot_log_K <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "log_K", "log(K)", "log(K)", showFlag)
      plot_objects$plot_log_K <- p
      p
    })
    
    output$plot_log_Y <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "log_Y", "log(Y)", "log(Y)", showFlag)
      plot_objects$plot_log_Y <- p
      p
    })
    
    output$plot_log_A <- renderPlot({
      req(results(), results_no_exp())
      showFlag <- showNoExpFlag()
      p <- make_plot(results(), results_no_exp(), "log_A", "log(A)", "log(A)", showFlag)
      plot_objects$plot_log_A <- p
      p
    })
    
    ################################
    # 5. Download Handlers         #
    ################################
    output$downloadData <- downloadHandler(
      filename = function() {
        paste0("results_", Sys.Date(), ".csv")
      },
      content = function(file) {
        req(results())
        write.csv(results(), file, row.names = FALSE)
      }
    )
    
    output$downloadPlots <- downloadHandler(
      filename = function() {
        paste0("plots_", Sys.Date(), ".zip")
      },
      content = function(file) {
        temp_dir <- tempdir()
        file_paths <- c()
        
        # convert reactiveValues to list
        plots_list <- reactiveValuesToList(plot_objects)
        plots_list <- Filter(Negate(is.null), plots_list)
        
        for (plot_name in names(plots_list)) {
          plot_file <- file.path(temp_dir, paste0(plot_name, ".png"))
          ggsave(plot_file, plots_list[[plot_name]], device = "png")
          file_paths <- c(file_paths, plot_file)
        }
        zip::zipr(zipfile = file, files = file_paths)
      }
    )
    
    # Return something if needed outside (e.g. results) 
    return(list(
      results       = results,
      resultsNoExp  = results_no_exp
    ))
  })
}
