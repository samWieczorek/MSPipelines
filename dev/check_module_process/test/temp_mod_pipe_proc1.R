
#' proc1 UI Function
#'
#' @description A shiny Module.
#'
#' @param id, input, output, session  #Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#' @import shinyjs
#' @importFrom shinyalert useShinyalert
#'
mod_proc1_ui <- function(id){
  ns <- NS(id)
}

#' proc1 Server Function
#'
#' @noRd
#'
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#'
mod_proc1_server <- function(id,
                            dataIn = NULL,
                            steps.enabled = reactive({NULL}),
                            remoteReset = reactive({FALSE})
                            ){

  #' @field global xxxx
  global <- list(
    VALIDATED = 1,
    UNDONE = 0,
    SKIPPED = -1
  )
# Define default selected values for widgets
widgets.default.values <- list(
step1_a = a,
step1_b = b,
step2_c = c,
step2_d = d
)


 ###-------------------------------------------------------------###
  ###                                                             ###
  ### ------------------- MODULE SERVER --------------------------###
  ###                                                             ###
  ###-------------------------------------------------------------###
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    rv.widgets <- reactiveValues()

    rv <- reactiveValues(
      dataIn = NULL,
      dataOut = NULL,
      status = NULL,
      reset = NULL,
      steps.enabled = NULL
    )


    dataOut <- reactiveValues(
      trigger = NULL,
      value = NULL
    )

config <- list(
    name = proc1,
    steps = c('step1','step2'),
    mandatory = c(TRUE,TRUE)
  )
rv.widgets <- reactiveValues(
step1_a =  widgets.default.values$step1_a,
step1_b =  widgets.default.values$step1_b,
step2_c =  widgets.default.values$step2_c,
step2_d =  widgets.default.values$step2_d
)


  # Initialization of the module
    observeEvent(steps.enabled(), ignoreNULL = TRUE, {
      if (is.null(steps.enabled()))
        rv$steps.enabled <- setNames(rep(FALSE, rv.process$length), rv.process$config$steps)
      else
        rv$steps.enabled <- steps.enabled()
    })


    observeEvent(remoteReset(), {
      lapply(names(rv.widgets), function(x){
        rv.widgets[[x]] <- widgets.default.values[[x]]
      })
    })

###### ------------------- Code for Description (step 0) -------------------------    #####
output$Description <- renderUI({
  rv$steps.enabled
  #browser()
  wellPanel(
    tagList(
      includeMarkdown( system.file('app/md', paste0(config$name, '.md'), package='Magellan')),
      uiOutput(ns('datasetDescription')),
      if (isTRUE(rv$steps.enabled['Description'])  )
        actionButton(ns('btn_validate_Description'),
                     paste0('Start ', config$name),
                     class = btn_success_color)
      else
        shinyjs::disabled(
          actionButton(ns('btn_validate_Description'),
                       paste0('Start ', config$name),
                       class = btn_success_color)
        )
    )
  )
})



observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
  rv$dataIn <- dataIn()
  dataOut$trigger <- Send_Result_to_Caller(rv$dataIn)$trigger
  dataOut$value <- Send_Result_to_Caller(rv$dataIn)$value
  #rv$status['Description'] <- global$VALIDATED

})

# Code for step step1
observeEvent(input$step1_a,{rv.widgets$step1_a <- input$step1_a})

observeEvent(input$step1_b,{rv.widgets$step1_b <- input$step1_b})

if (rv$steps.enabled['#step.name#'])
                selectInput(ns('step1_a'), 'Select step1_a',
                          choices = 1:3,
                          selected = rv.widgets$step1_a,
                          width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('step1_a'), 'Select step1_a',
                              choices = 1:5,
                              selected = rv.widgets$step1_a,
                              width = '150px')
                )
# Code for step step2
observeEvent(input$step2_c,{rv.widgets$step2_c <- input$step2_c})

observeEvent(input$step2_d,{rv.widgets$step2_d <- input$step2_d})

if (rv$steps.enabled['#step.name#'])
                selectInput(ns('step2_c'), 'Select step2_c',
                          choices = 1:3,
                          selected = rv.widgets$step2_c,
                          width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('step2_c'), 'Select step2_c',
                              choices = 1:5,
                              selected = rv.widgets$step2_c,
                              width = '150px')
                )
