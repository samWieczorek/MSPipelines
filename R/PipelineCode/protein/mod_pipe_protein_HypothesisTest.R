#' pipe_prot_hypotest UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
mod_pipe_protein_HypothesisTest_ui <- function(id){
  ns <- NS(id)
  tagList(
    mod_navigation_ui(ns('nav_pipe_prot_hypotest'))
  )
}

#' pipe_prot_hypotest Server Function
#'
#' @noRd
#'
#' @param input,output,session
#'
#' @param obj
#'
#' @param ind
#'
#' @import QFeatures
#'
mod_pipe_protein_HypothesisTest_server <- function(input, output, session, obj, indice){
  ns <- session$ns


  ##
  ##
  ## Calls to other modules
  ##
  ##

  rv.hypotest$settings <- mod_settings_server("settings",
                                              obj = reactive({obj()}))



  ##
  ## Definitions of the screens
  ##

  observe({
    req(obj())
    rv.hypotest$dataIn <- obj()
    rv.hypotest$i <- indice()
  })




  ###---------------------------------------------------------------------------------###
  ###                                 Screen 1                                        ###
  ###---------------------------------------------------------------------------------###



  ###---------------------------------------------------------------------------------###
  ###                                 Screen 2                                        ###
  ###---------------------------------------------------------------------------------###

  output$Screen_Prot_hypotest_2 <- renderUI({

    tagList(

    )
  })


  output$btn_valid <- renderUI({

    cond <- (input$diffAnaMethod != "None")&&(input$anaDiff_Design != "None")
    if (!cond){return(NULL)}
    actionButton(ns("ValidTest"),"Save significance test", class = actionBtnClass)

  })



  observeEvent(input$ValidTest,{



    rv.hypotest$dataOut <- rv.hypotest$dataIn

    r.nav$isDone[2] <- TRUE
  })



  return({reactive(rv.hypotest$dataOut)})


}


## To be copied in the UI
# mod_pipe_protein_hypothesisTest_ui("pipe_hypotest_ui_1")

## To be copied in the server
# callModule(mod_pipe_protein_hypothesisTest_server, "pipe_hypotest_ui_1")

