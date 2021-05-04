
#' toto_shinyTest UI Function
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
mod_toto_shinyTest_ui <- function(id){
  ns <- NS(id)
}

#' toto_shinyTest Server Function
#'
#' @noRd
#'
#' @import DAPAR2
#' @import QFeatures
#' @importFrom shinyalert shinyalert
#'
mod_toto_shinyTest_server <- function(id,
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

config <- list(
    name = toto_shinyTest,
    steps = c('A','Z'),
    mandatory = c('T',' F')
  )