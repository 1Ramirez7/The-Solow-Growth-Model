#############################
# MODULE: EXPERIMENTS      #
#############################
mod_experiments_ui <- function(id) {
  ns <- NS(id)
  tagList(
    h4("Experiments"),
    selectInput(ns("param_name"), "Quantitative exp (s, delta, n, z, l)",
                choices = c("s", "delta", "n", "z", "l"), selected = "s"),
    numericInput(ns("new_value"), "New Value", value = 0, step = 0.01),
    numericInput(ns("start_period"), "Start Period", value = 5, step = 1),
    numericInput(ns("length"), "Length of Effect", value = 5, step = 1),
    actionButton(ns("add_experiment"), "Add Experiment"),
    checkboxInput(ns("show_no_exp"), "Show Counterfactual", value = TRUE),
    actionButton(ns("simulate"), "Simulate"),
    DTOutput(ns("experimentsTable"))
  )
}

mod_experiments_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Store experiments in a reactiveVal
    experiments <- reactiveVal(
      data.frame(
        param = character(0),
        new_value = numeric(0),
        start_period = integer(0),
        length = integer(0),
        stringsAsFactors = FALSE
      )
    )
    
    # 2. Add new experiment row
    observeEvent(input$add_experiment, {
      new_experiment <- data.frame(
        param        = input$param_name,
        new_value    = input$new_value,
        start_period = input$start_period,
        length       = input$length,
        stringsAsFactors = FALSE
      )
      experiments(rbind(experiments(), new_experiment))
    })
    
    # 3. Render experiments table with delete buttons
    output$experimentsTable <- renderDT({
      exps <- experiments()
      if (nrow(exps) == 0) return(NULL)
      
      deleteButtons <- sapply(seq_len(nrow(exps)), function(i) {
        as.character(
          actionButton(
            inputId = paste0(ns("delete_row_"), i),
            label = "Delete",
            class = "btn btn-danger btn-sm",
            onclick = paste0('Shiny.setInputValue(\"', ns("delete_row"), '\", this.id, {priority: \"event\"})')
          )
        )
      })
      exps$Delete <- deleteButtons
      
      datatable(
        exps,
        escape = FALSE,
        selection = "none",
        options = list(pageLength = 5)
      )
    }, server = FALSE)
    
    # 4. Delete row logic
    observeEvent(input$delete_row, {
      row_str <- gsub("delete_row_", "", input$delete_row)
      row_num <- as.numeric(row_str)
      
      exps <- experiments()
      if (!is.na(row_num) && row_num >= 1 && row_num <= nrow(exps)) {
        exps <- exps[-row_num, ]
        experiments(exps)
      }
    })
    
    # Return multiple reactive values
    return(
      list(
        experiments   = experiments,
        simulateBtn   = reactive(input$simulate),
        showNoExpFlag = reactive(input$show_no_exp)
      )
    )
  })
}
