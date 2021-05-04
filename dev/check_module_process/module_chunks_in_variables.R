
mod_Protein_Description_ui <- function(id){
  ns <- NS(id)
}


header <- "btn_style <- 'display:inline-block; vertical-align: middle; padding: 7px'"

ui <- "
#' #name# UI Function
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
mod_#name#_ui <- function(id){
  ns <- NS(id)
}
"



start_server <- "
#' #name# Server Function
#'
#' @noRd
#'
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#'
mod_#name#_server <- function(id,
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
"


moduleServerFunc_template <- "
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
"

config_template <- "
config <- list(
    name = #name#,
    steps = #steps#,
    mandatory = #mandatory#
  )
"


init_module_template <- "
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
"


output_Description_template <- "
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

"


observer_for_widget_template <- "
observeEvent(input$#name#,{rv.widgets$#name# <- input$#name#})
"


widget.renderUI.template <-"
output$#name#_UI <- renderUI({
  #rv$steps.enabled
  rv.widgets$#name#
    if (rv$steps.enabled['#step#'])
    selectInput(ns('#name#'), '#name# in renderUI',
                choices = 1:4,
                selected = rv.widgets$#name#,
                width = '150px')
  else
    shinyjs::disabled(
      selectInput(ns('#name#'), '#name# in renderUI',
                  choices = 1:4,
                  selected = rv.widgets$#name#,
                  width = '150px')
      )
})
"


code_for_global_step_renderUI_header <- "
output$#step.name# <- renderUI({
  #rv$steps.enabled
  name <- '#step.name#'
  wellPanel(id = ns('foo'),
    actionButton(ns('btn1'), 'Example button'),
    tagList(
"

code_for_global_step_renderUI_call_widget_renderUI <- "
   uiOutput(ns('#widget.name#_UI'))
   "

code_for_global_step_renderUI_call_widget <- "
if (rv$steps.enabled['#step.name#'])
                selectInput(ns('#widget.name#'), 'Select #widget.name#',
                          choices = 1:3,
                          selected = rv.widgets$#widget.name#,
                          width = '150px')
              else
                shinyjs::disabled(
                  selectInput(ns('#widget.name#'), 'Select #widget.name#',
                              choices = 1:5,
                              selected = rv.widgets$#widget.name#,
                              width = '150px')
                )
"

code_for_global_step_renderUI_call_validate_btn <- "
if (rv$steps.enabled['#step.name#'])
                actionButton(ns(paste0('btn_validate_', #step.name#)),
                             'Perform #step.name#',
                             class = btn_success_color)
              else
                shinyjs::disabled(
                  actionButton(ns(paste0('btn_validate_', #step.name#)),
                               'Perform #step.name#',
                               class = btn_success_color)
                  )
"

code_for_global_step_renderUI_end <- "
    )
  )
})
"

end_server <-  "
 return({reactive(rv$dataOut)})

}

## To be copied in the UI
# mod_pipe_name_ui('pipe_name_ui_1')

## To be copied in the server
# mod_pipe_name_server('pipe_name_ui_1')
"


screen_content_ui <- c(
  "mod_infos_dataset_ui('infos')",

  "mod_plots_corr_matrix_ui('plots_corr_matrix')"
)


screen_content_server <- c(
  "mod_infos_dataset_server('infos',
                 obj = reactive({rv$dataIn}))",

  "rv$settings <- mod_settings_server('settings', obj=reactive({rv$dataIn}))

    mod_plots_corr_matrix_server('plots_corr_matrix',
                 obj = reactive({rv$dataIn}),
                 names = reactive({NULL}),
                 gradientRate = reactive({r$settings()$defaultGradientRate})
                 )"
  )



watch_file <- "
Watch_mod_pipe_name <- mod_pipe_name_server('mod_pipe_name',
                                                obj = reactive({rv.core$current.obj}),
                                                indice = reactive({rv.core$current.indice})
                                       )




observeEvent(req(Watch_mod_pipe_name()),{
  rv.core$current.obj <- Watch_mod_pipe_name()
})
"
