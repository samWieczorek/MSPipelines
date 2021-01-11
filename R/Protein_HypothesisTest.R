#' @importFrom R6 R6Class
#' @importFrom Magellan Process
#' @import shiny
#'
#' @export
#'
Protein_HypothesisTest = R6::R6Class(
  "Protein_HypothesisTest",
  inherit = Magellan::Process,
  private = list(
    .config = list(name = 'HypothesisTest',
                   steps = c('Description', 'Step1', 'Step2'),
                   mandatory = c(T, F, T)
    )
  ),

  public = list(

    ## reactive values for variables in the module
    rv.hypotest = reactiveValues(
      name = "processHypotest",
      dataIn = NULL,
      i = NULL,
      settings = NULL,
      # contient l'objet de sortie du module (ie. a QFeatures instance)
      dataOut = NULL,
      widgets = list(design = "None",
                     method = "None",
                     ttest_options = "Student",
                     th_logFC = 0,
                     listNamesComparison = NULL),
      res_AllPairwiseComparisons = NULL
    ),


    Global_server = function(input, output){},

    Description_server = function(input, output){
      observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
        cat(paste0(class(self)[1], "::observeEvent(input$btn_validate_Description from - ", self$id, '\n'))
        private$InitializeDataIn()
        self$ValidateCurrentPos()
      })

      output$datasetDescription <- renderUI({
        tagList(
          p(paste0('Dataset description: ', paste0(names(self$rv$temp.dataIn), collapse=", ")))
        )
      })
    },


    Description_ui = function(){

      wellPanel(
        tagList(
          includeMarkdown( system.file("md", paste0(self$config$name, ".md"), package="MSPipelines")),
          uiOutput(self$ns('datasetDescription')),
          actionButton(self$ns('btn_validate_Description'),
                       paste0('Start ', self$config$name),
                       class = 'btn-success')
        )
      )
    },

    ############### SCREEN 2 ######################################

    Step1_server = function(input, output){

      output$Screen_Prot_hypotest_1 <- renderUI({

        print('screen 1')
        isolate({
          NA.count<- length(which(is.na(assay(self$rv.hypotest$dataIn[[self$rv.hypotest$i]]))))
          if (NA.count > 0){
            tags$p("Your dataset contains missing values. Before using the differential analysis, you must filter/impute them")
          } else {
            tagList(

              tags$div(
                tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                          selectInput(self$ns("anaDiff_Design"), "Contrast",
                                      choices=c("None"="None", "One vs One"='OnevsOne', "One vs All"="OnevsAll"),
                                      selected = self$rv.hypotest$widgets$design,
                                      width='150px')
                ),
                tags$div( style="display:inline-block; vertical-align: middle;padding-right: 20px;",
                          selectInput(self$ns("diffAnaMethod"),"Statistical test",
                                      choices = c("None"="None",
                                                  "Limma"="Limma",
                                                  "t-tests"="ttests"),
                                      selected = self$rv.hypotest$widgets$method,
                                      width = '150px')
                ),
                tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                          hidden( radioButtons(self$ns("ttest_options"), "t-tests options",choices=c("Student", "Welch"),
                                               selected = self$rv.hypotest$widgets$ttest_options,
                                               width = '150px'))
                ),
                tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                          textInput(self$ns("seuilLogFC"), "log(FC) threshold",
                                    value = self$rv.hypotest$widgets$th_logFC,
                                    width = '150px'),
                          #module_Not_a_numericUI(ns("test_seuillogFC"))
                ),
                tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                          uiOutput(self$ns("correspondingRatio"))

                ),
                tags$div( style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                          actionButton(self$ns("PerformLogFCPlot"), "Perform log FC plot",class = actionBtnClass )

                )
              ),
              tags$hr(),
              highchartOutput(self$ns("FoldChangePlot"), height="100%")
            )

          }
        })
      })



      observeEvent(input$anaDiff_Design, ignoreInit=TRUE,{
        self$rv.hypotest$widgets$design <- input$anaDiff_Design
      })

      observeEvent(input$diffAnaMethod, ignoreInit=TRUE,{
        self$rv.hypotest$widgets$method <- input$diffAnaMethod
        toggle(id = "ttest_options",  condition = (input$diffAnaMethod == "ttests"))
      })

      observeEvent(input$ttest_options, ignoreInit=TRUE,{
        self$rv.hypotest$widgets$ttest_options <- input$ttest_options
      })

      observeEvent(input$seuilLogFC, ignoreInit=TRUE,{
        self$rv.hypotest$widgets$th_logFC <- as.numeric(input$seuilLogFC)
      })



      output$FoldChangePlot <- renderHighchart({
        req(input$PerformLogFCPlot)


        self$rv.hypotest$res_AllPairwiseComparisons <- self$rv.hypotest$dataIn

        if(!is.null(rv.hypotest$res_AllPairwiseComparisons[['proteins_hypotest']])){

          ind <- grep('_logFC', colnames(MultiAssayExperiment::metadata(self$rv.hypotest$res_AllPairwiseComparisons[['proteins_hypotest']])$t_test))

          self$rv.hypotest$widgets$listNamesComparison <- names(MultiAssayExperiment::metadata(self$rv.hypotest$res_AllPairwiseComparisons[['proteins_hypotest']])$t_test)[ind]

          df <- setNames(as.data.frame(MultiAssayExperiment::metadata(self$rv.hypotest$res_AllPairwiseComparisons[['proteins_hypotest']])$t_test[ , ind]),
                         colnames(MultiAssayExperiment::metadata(self$rv.hypotest$res_AllPairwiseComparisons[['proteins_hypotest']])$t_test)[ind])


          hc_logFC_DensityPlot(df, as.numeric(input$seuilLogFC))

        }

      })

      ########################################################
      ### calcul des comparaisons                         ####
      ########################################################

      observeEvent(input$PerformLogFCPlot, {
        req(input$diffAnaMethod)
        req(input$anaDiff_Design)
        input$ttest_options


        if ((input$diffAnaMethod=="None")|| (input$anaDiff_Design=="None")) {return (NULL)}
        if (length(which(is.na(assay(self$rv.hypotest$dataIn[[self$rv.hypotest$i]])))) > 0) { return(NULL)}


        rv.hypotest$dataIn <- obj()
        rv.hypotest$i <- indice()


        #browser()
        isolate({
          switch(input$diffAnaMethod,
                 Limma={
                   self$rv.hypotest$dataIn <- t_test_sam(object = self$rv.hypotest$dataIn,
                                                    i = self$rv.hypotest$i,
                                                    name = "proteins_hypotest",
                                                    FUN = "limma.complete.test",
                                                    comp.type = input$anaDiff_Design)
                 },
                 ttests={
                   self$rv.hypotest$dataIn <- t_test_sam(object = self$rv.hypotest$dataIn,
                                                    i = self$rv.hypotest$i,
                                                    name = "proteins_hypotest",
                                                    FUN = "compute.t.test",
                                                    contrast = input$anaDiff_Design,
                                                    type = input$ttest_options)
                 })
          self$rv.hypotest$i <- rv.hypotest$i + 1
        })

      })



      output$correspondingRatio <- renderUI({

        ratio <- as.numeric(rv.hypotest$widgets$th_logFC)

        p("(FC = ", round(2^(ratio),3), ")")

      })

      observeEvent(input$btn_validate_Step1, ignoreInit = T, {

        # Add your stuff code here


        self$ValidateCurrentPos()
      })
    },

    Step1_ui = function(){uiOutput(self$ns('Screen_Prot_hypotest_1'))},


    Step2_server = function(input, output){

      output$btn_valid <- renderUI({

        cond <- (input$diffAnaMethod != "None")&&(input$anaDiff_Design != "None")
        if (!cond){return(NULL)}
        actionButton(self$ns("ValidTest"),"Save significance test", class = actionBtnClass)

      })

      observeEvent(input$btn_validate_Step3, ignoreInit = T, {
        if (self$rv.hypotest$widgets$method != 'Limma') {
          MultiAssayExperiment::metadata(self$rv.hypotest$dataIn[[self$rv.hypotest$i]])$Params <- list(
            design = self$rv.hypotest$widgets$design,
            method = self$rv.hypotest$widgets$method,
            ttest_options = self$rv.hypotest$widgets$ttest_options,
            th_logFC = self$rv.hypotest$widgets$th_logFC,
            listNamesComparison = self$rv.hypotest$widgets$listNamesComparison
          )
        } else {
          MultiAssayExperiment::metadata(self$rv.hypotest$dataIn[[self$rv.hypotest$i]])$Params <- list(
            design = self$rv.hypotest$widgets$design,
            method = self$rv.hypotest$widgets$method,
            th_logFC = self$rv.hypotest$widgets$th_logFC,
            listNamesComparison = self$rv.hypotest$widgets$listNamesComparison
          )
        }
        self$rv$dataIn <- Add_Item_to_Dataset(self$rv$dataIn, self$config$name)
        self$ValidateCurrentPos()
      })
    },

    Step2_ui = function(){uiOutput(self$ns("btn_valid"))}
  )
)
