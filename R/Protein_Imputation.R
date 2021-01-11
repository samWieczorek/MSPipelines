#' @importFrom R6 R6Class
#' @importFrom Magellan Process
#' @import shiny
#'
#' @export
#'
Protein_Imputation = R6::R6Class(
  "Protein_Imputation",
  inherit = Magellan::Process,
  private = list(
    .config = list(name = 'Imputation',
                   steps = c('Description', 'POV imputation', 'MEC imputation', 'Save'),
                   mandatory = c(T,F,T,F)
    )
  ),

  public = list(

    ## reactive values for variables in the module
    rv.impute = reactiveValues(
      name = "processProtImpute",
      dataIn = NULL,
      dataOut = NULL,
      i = NULL,
      settings = NULL,
      tmp = NULL,

      widgets = list(POV_algorithm = "None",
                     POV_detQuant_quantile = 0,
                     POV_detQuant_factor = 1,
                     POV_KNN_n = 10,
                     MEC_algorithm = "None",
                     MEC_detQuant_quantile = 2.5,
                     MEC_detQuant_factor = 1,
                     MEC_fixedValue = 0
      ),
      imputePlotsSteps = list(step0 = NULL,
                              step1 = NULL,
                              step2 = NULL
      )
    ),

    #global variables for the module
    imputationAlgorithmsProteins_POV = list("None" = "None",
                                             "slsa" = "slsa",
                                             "Det quantile" = "detQuantile",
                                             "KNN" = "KNN"),

    imputationAlgorithmsProteins_MEC = list("None" = "None",
                                             "Det quantile" = "detQuantile",
                                             "Fixed value" = "fixedValue"),


    Global_server = function(input, output){

      mod_plots_mv_for_imputation_server("mvImputationPlots_MV",
                                         obj = reactive({dataIn()}),
                                         ind = reactive({self$rv.impute$i}),
                                         title = reactive({"POV distribution"}),
                                         palette = reactive({rv.impute$settings()$basePalette}))

      mod_plots_mv_for_imputation_server("mvImputationPlots_MEC",
                                         obj = reactive({self$rv.impute$imputePlotsSteps$step1}),
                                         ind = reactive({length(names(self$rv.impute$imputePlotsSteps$step1))}),
                                         title = reactive({"Distribution after POV imputation"}),
                                         palette = reactive({rv.impute$settings()$basePalette}))

      mod_plots_mv_for_imputation_server("mvImputationPlots_Valid",
                                         obj = reactive({self$rv.impute$imputePlotsSteps$step2}),
                                         ind = reactive({length(names(self$rv.impute$imputePlotsSteps$step2))}),
                                         title = reactive({"Distribution after POV and MEC imputation"}),
                                         palette = reactive({rv.impute$settings()$basePalette}))

      mod_det_quant_impute_Values_server("POV_DetQuantValues_DT",
                                         qData = reactive({req(self$rv.impute$dataIn, self$rv.impute$i)
                                           SummarizedExperiment::assay(self$rv.impute$dataIn[[self$rv.impute$i]])
                                         }),
                                         quant = reactive({self$rv.impute$widgets$POV_detQuant_quantile}),
                                         factor = reactive({self$rv.impute$widgets$POV_detQuant_factor}))

      mod_det_quant_impute_Values_server("MEC_DetQuantValues_DT",
                                         qData = reactive({req(self$rv.impute$dataIn, self$rv.impute$i)
                                           SummarizedExperiment::assay(self$rv.impute$dataIn[[self$rv.impute$i]])
                                         }),
                                         quant = reactive({self$rv.impute$widgets$MEC_detQuant_quantile}),
                                         factor = reactive({self$rv.impute$widgets$MEC_detQuant_factor}))
      },

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


      output$Screen_Impute_1 <- renderUI({

        tagList(
          tags$div(
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      uiOutput(self$ns("sidebar_imputation_step1"))) ,
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      uiOutput(self$ns("POV_Params"))),
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      uiOutput(self$ns("POV_showDetQuantValues")))
          ),

          tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                    actionButton(self$ns("perform.imputationClassical.button"),
                                 "Perform imputation", class = actionBtnClass)),
          tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                    uiOutput(self$ns("ImputationStep1Done"))),

          htmlOutput(self$ns("helpForImputation")),
          tags$hr(),
          mod_plots_mv_for_imputation_ui(self$ns("mvImputationPlots_MV"))
        )
      })


      observeEvent(input$POV_missing.value.algorithm, {
        self$rv.impute$widgets$POV_algorithm <- input$POV_missing.value.algorithm
      })

      observeEvent(input$POV_detQuant_quantile, {
        self$rv.impute$widgets$POV_detQuant_quantile <- input$POV_detQuant_quantile
      })

      observeEvent(input$POV_detQuant_factor, {
        self$rv.impute$widgets$POV_detQuant_factor <- input$POV_detQuant_factor
      })

      observeEvent(input$KNN_nbNeighbors, {
        self$rv.impute$widgets$POV_KNN_n <- input$KNN_nbNeighbors
      })


      observeEvent(input$perform.imputationClassical.button,{

        nbMV_Before <- length(which(is.na(assay(self$rv.impute$dataIn, self$rv.impute$i))))

        withProgress(message = '',detail = '', value = 0, {

          incProgress(0.5, detail = 'POV Imputation')

          #browser()
          switch(self$rv.impute$widgets$POV_algorithm,
                 slsa = {
                   self$rv.impute$dataIn <- impute_dapar(object = self$rv.impute$dataIn,
                                                    i = self$rv.impute$i,
                                                    name = 'POV_impute',
                                                    method = 'POV_slsa',
                                                    sampleTab = colData(self$rv.impute$dataIn))
                 },
                 detQuantile = {
                   self$rv.impute$dataIn <- impute_dapar(object = self$rv.impute$dataIn,
                                                    i = self$rv.impute$i,
                                                    name = 'POV_impute',
                                                    method = 'POV_det_quant',
                                                    conds = colData(self$rv.impute$dataIn)$Condition,
                                                    qval = self$rv.impute$widgets$POV_detQuant_quantile/100,
                                                    factor = self$rv.impute$widgets$POV_detQuant_factor)
                 },
                 KNN = {
                   self$rv.impute$dataIn <- impute_dapar(object = self$rv.impute$dataIn,
                                                    i =  self$rv.impute$i,
                                                    name = 'POV_impute',
                                                    method = 'POV_knn_by_conds',
                                                    conds = colData(self$rv.impute$dataIn)$Condition,
                                                    k = self$rv.impute$widgets$POV_KNN_n)
                 }
          )

          incProgress(1, detail = 'Finalize POV imputation')
          self$rv.impute$i <- length(names(self$rv.impute$dataIn))
          nbMV_After <- length(which(is.na(assay(self$rv.impute$dataIn, self$rv.impute$i))))
          self$rv.impute$nb_POV_imputed <-  nbMV_Before - nbMV_After

          self$rv.impute$imputePlotsSteps$step1 <- rv.impute$dataIn
          r.nav$isDone[1] <- TRUE
        })

      })





      output$POV_Params <- renderUI({
        req(self$rv.impute$widgets$POV_algorithm)


        switch(self$rv.impute$widgets$POV_algorithm,
               detQuantile = {

                 tagList(
                   tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                             numericInput(self$ns("POV_detQuant_quantile"), "Quantile",
                                          value = self$rv.impute$widgets$POV_detQuant_quantile,
                                          step=0.5, min=0, max=100, width='100px')),
                   tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                             numericInput(self$ns("POV_detQuant_factor"), "Factor",
                                          value = self$rv.impute$widgets$POV_detQuant_factor,
                                          step=0.1, min=0, max=10, width='100px'))
                 )
               },
               KNN = {
                 numericInput(self$ns("KNN_nbNeighbors"), "Neighbors",
                              value = self$rv.impute$widgets$POV_KNN_n, step=1, min=0,
                              max=max(nrow(self$rv.impute$dataIn), self$rv.impute$widgets$POV_KNN_n),
                              width='100px')
               }
        )


      })



      output$sidebar_imputation_step1 <- renderUI({
        # req(rv.impute$dataIn)


        if (length(grep("imputed", names(self$rv.impute$dataIn)))==0){
          shinyjs::enable("perform.imputationClassical.button")

        } else {
          shinyjs::disable("perform.imputationClassical.button")
        }

        algo <- imputationAlgorithmsProteins_POV

        tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                  selectInput(self$ns("POV_missing.value.algorithm"),"Algorithm for POV",
                              choices = algo,
                              selected = self$rv.impute$widgets$POV_algorithm,
                              width='150px')
        )
      })



      output$POV_showDetQuantValues <- renderUI({

        req(self$rv.impute$widgets$POV_algorithm)

        if (self$rv.impute$widgets$POV_algorithm == 'detQuantile')
        {
          tagList(
            h5("The POV will be imputed by the following values :"),
            mod_det_quant_impute_Values_ui(self$ns("POV_DetQuantValues_DT"))
          )
        }
      })



      output$ImputationStep1Done <- renderUI({

        if (isTRUE(r.nav$isDone[1])) {
          tagList(
            h5(paste0("POV imputation done.", self$rv.impute$nb_POV_imputed, " were imputed")),
            # br(),
            h5("Updated graphs can be seen on step \"2 - Missing on the Entire Condition\".")
          )
        }
      })



      observeEvent(input$btn_validate_Step1, ignoreInit = T, {

        # Add your stuff code here


        self$ValidateCurrentPos()
      })
    },

    Step1_ui = function(){uiOutput(self$ns('Screen_Impute_1'))},


    # Screen 3
    Step2_server = function(input, output){
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$btn_validate_Step2, ignoreInit = T, {
        self$ValidateCurrentPos()
      })


      output$Screen_Impute_2 <- renderUI({

        tagList(
          uiOutput("warningMECImputation"),
          tags$div(
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      uiOutput(self$ns("MEC_chooseImputationMethod"))),
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      uiOutput(self$ns("MEC_Params"))),
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      uiOutput(self$ns("MEC_showDetQuantValues")))),

          tagList(
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      actionButton(self$ns("btn.perform.imputationMEC"),"Perform imputation", class = actionBtnClass)),
            tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                      uiOutput(self$ns("ImputationStep2Done")))),
          tags$hr(),
          withProgress(message = '',detail = '', value = 0, {
            incProgress(0.5, detail = 'Building plots...')

            mod_plots_mv_for_imputation_ui(self$ns("mvImputationPlots_MEC"))
          })
        )

      })

      observeEvent(input$MEC_detQuant_quantile, {
        self$rv.impute$widgets$MEC_detQuant_quantile <- input$MEC_detQuant_quantile
      })

      observeEvent(input$MEC_fixedValue, {
        self$rv.impute$widgets$MEC_fixedValue <- input$MEC_fixedValue
      })

      observeEvent(input$MEC_detQuant_factor, {
        self$rv.impute$widgets$MEC_detQuant_factor <- input$MEC_detQuant_factor
      })

      observeEvent(input$MEC_missing.value.algorithm, {
        self$rv.impute$widgets$MEC_algorithm <- input$MEC_missing.value.algorithm
      })



      output$warningMECImputation<- renderUI({

        tags$p(tags$b("Warning:"),"Imputing MEC in a conservative way
  is a real issue as, in the given condition, there is no observed value to rely on.
   Thus, if imputation is not avoidable, imputed MEC must be very cautiously interpreted.")
      })



      observeEvent(input$btn.perform.imputationMEC,{


        withProgress(message = '',detail = '', value = 0, {
          #rv.impute$dataIn <- reIntroduceMEC(rv.impute$dataIn, rv.impute$MECIndex)

          nbMV_Before <- length(which(is.na(assay(self$rv.impute$dataIn, self$rv.impute$i))))
          incProgress(0.75, detail = 'MEC Imputation')
          switch(rv.impute$widgets$MEC_algorithm,
                 detQuantile = {
                   self$rv.impute$dataIn <- DAPAR2::impute_dapar(object = self$rv.impute$dataIn ,
                                                    i = self$rv.impute$i,
                                                    name = 'MEC_impute',
                                                    method = 'det_quant',
                                                    qval = self$rv.impute$widgets$MEC_detQuant_quantile/100,
                                                    factor = self$rv.impute$widgets$MEC_detQuant_factor
                   )
                 },
                 fixedValue = {
                   self$rv.impute$dataIn <- DAPAR2::impute_dapar(object = self$rv.impute$dataIn,
                                                    i = self$rv.impute$i,
                                                    name = 'MEC_impute',
                                                    method = 'fixed_val',
                                                    value = self$rv.impute$widgets$MEC_fixedValue
                   )
                 }
          )

          self$rv.impute$i <- length(names(self$rv.impute$dataIn))
          nbMV_After <- length(which(is.na(assay(self$rv.impute$dataIn , self$rv.impute$i))))
          self$rv.impute$nb_MEC_imputed <-  nbMV_Before - nbMV_After

          incProgress(1, detail = 'Finalize MEC imputation')
          #rv.impute$impute_Step <- 2
          self$rv.impute$imputePlotsSteps$step2 <- self$rv.impute$dataIn
        })
      })



      output$MEC_Params <- renderUI({
        req(self$rv.impute$widgets$MEC_algorithm)

        switch (self$rv.impute$widgets$MEC_algorithm,
                detQuantile = {
                  tagList(
                    tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                              numericInput(self$ns("MEC_detQuant_quantile"), "Quantile",
                                           value = self$rv.impute$widgets$MEC_detQuant_quantile,
                                           step=0.5, min=0, max=100,
                                           width='100px')),
                    tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                              numericInput(self$ns("MEC_detQuant_factor"), "Factor",
                                           value = self$rv.impute$widgets$MEC_detQuant_factor,
                                           step=0.1, min=0, max=10,
                                           width='100px'))
                  )
                },
                fixedValue = {

                  numericInput(self$ns("MEC_fixedValue"), "Fixed value",
                               value = self$rv.impute$widgets$MEC_fixedValue,
                               step=0.1, min=0, max=100,
                               width='100px')
                })

      })



      output$MEC_chooseImputationMethod <- renderUI({
        algo <- self$imputationAlgorithmsProteins_MEC

        tags$div( style="display:inline-block; vertical-align: top; padding-right: 40px;",
                  selectInput(self$ns("MEC_missing.value.algorithm"), "Algorithm for MEC", choices = algo,
                              selected = self$rv.impute$widgets$MEC_algorithm, width='150px')
        )
      })



      output$MEC_showDetQuantValues <- renderUI({

        req(self$rv.impute$widgets$MEC_algorithm)

        if (self$rv.impute$widgets$MEC_algorithm == 'detQuantile')
        {
          tagList(
            h5("The MEC will be imputed by the following values :"),
            mod_det_quant_impute_Values_ui(self$ns("MEC_DetQuantValues_DT"))
          )
        }
      })


      output$ImputationStep2Done <- renderUI({

        if (r.nav$isDone[2]) {
          tagList(
            h5("MEC imputation done.", self$rv.impute$nb_MEC_imputed, " were imputed"),
            h5("Updated graphs cans be seen on step \"3 - Save\"."))
        }

      })

    },

    Step2_ui = function(){uiOutput(self$ns('Screen_Impute_2'))},

    Step3_server = function(input, output){

      observeEvent(input$btn_validate_Step3, ignoreInit = T, {
        ind <- grep('_impute', names(rv.impute$dataIn))

        if (length(ind) > 1){
          txt <- lapply(ind, function(x){MultiAssayExperiment::metadata(rv.impute$dataIn[[x]])$Params})
          ind <- ind[-length(ind)]
          rv.impute$dataIn <- rv.impute$dataIn[ , ,-ind]
          MultiAssayExperiment::metadata(rv.impute$dataIn[[rv.impute$i]])$Params <- txt
        } else if (length(ind)==1){
          names(rv.impute$dataIn)[rv.impute$i] <- 'impute'
        }
        self$rv$dataIn <- Add_Item_to_Dataset(self$rv$dataIn, self$config$name)
        self$ValidateCurrentPos()
      })
    },

    Step3_ui = function(){
      tagList(
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  actionButton(self$ns("ValidImputation"),"Save imputation", class = actionBtnClass)),
        tags$div( style="display:inline-block; vertical-align: top; padding-right: 20px;",
                  uiOutput(self$ns("ImputationSaved"))),
        tags$hr(),
        mod_plots_mv_for_imputation_ui(self$ns("mvImputationPlots_Valid"))
      )
    }
  )
)
