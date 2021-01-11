library(shiny)
library(R6)
library(tibble)
library(Magellan)
options(shiny.fullstacktrace = T)

#------------------------ Class TimelineDraw --------------------------------------
source(file.path('../../R', 'global.R'), local=TRUE)$value
source(file.path("../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../R", "mod_format_DT.R"), local = TRUE)$value

source(file.path('.', 'Example_ProcessA.R'), local=TRUE)$value
source(file.path('.', 'Example_ProcessB.R'), local=TRUE)$value
source(file.path('.', 'Example_Description.R'), local=TRUE)$value


#----------------------------------------------------------------------------





Pipeline <- R6Class(
  "Pipeline",
  public = list(
    id = NULL,
    ns = NULL,
    tmp.return = reactiveValues(),
    rv = reactiveValues(dataIn = NULL),
    child.process = list(
      #Example_Description = NULL,
      Example_ProcessA = NULL
      #Example_ProcessB = NULL
    ),
    initialize = function(id){
      self$id <- id
      self$ns <- NS(id)
      self$child.process <- setNames(lapply(names(self$child.process),
                                            function(x){
                                              assign(x, base::get(x))$new(self$ns(x))
                                            }),
                                     names(self$child.process)
      )
    },


    Launch_Servers = function(data){
      lapply(names(self$child.process), function(x){
        self$tmp.return[[x]] <- self$child.process[[x]]$server(dataIn = reactive({data()}))
      })
    },

ui = function() {
  tagList(
    lapply(names(self$child.process), function(x){
      wellPanel(h3(x), self$child.process[[x]]$ui())
    })
  )
},
server = function(dataIn ) {

  self$Launch_Servers(data = reactive({dataIn()}))

  moduleServer(self$id, function(input, output, session) {

    output$show_ui <- renderUI({
    tagList(
     lapply(names(self$child.process), function(x){
        wellPanel(h3(x), self$child.process[[x]]$ui())
      })
    )
  })



  })
}
)
)

rv <- reactiveValues()
Pipeline <- Pipeline$new('App')
ui = fluidPage(
  tagList(
    actionButton('send', 'Send dataset'),
    Pipeline$ui()
  )
)




server = function(input, output){
  # Get a QFeatures dataset for example

  Pipeline$server(dataIn = reactive({rv$dataIn}))

  utils::data(Exp1_R25_prot, package='DAPARdata2')


  observeEvent(input$send,{
    if (input$send%%2 != 0)
      rv$dataIn <- Exp1_R25_prot
    else
      rv$dataIn <- NULL
  })

  }
shiny::shinyApp(ui, server)
