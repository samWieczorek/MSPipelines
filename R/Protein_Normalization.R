#' @importFrom R6 R6Class
#' @importFrom Magellan Process
#' @import shiny
#' @import highcharter
#' @import shinyjs
#' @import QFeatures
#'
#' @export
#'
Protein_Normalization = R6::R6Class(
  "Protein_Normalization",
  inherit = Magellan::Process,
  private = list(
    .config = list(name = 'Normalization',
                   steps = c('Description', 'Normalize', 'Save'),
                   mandatory = c(T, F, T)
    ),
    new.name = 'proteins_norm'
  ),

  public = list(

    Global_server = function(session, input, output){

      self$rv$trackFromBoxplot = NULL
      self$rv$selectProt = NULL
      self$rv$resetTracking = FALSE

      # Update widgets if dataset already exists
      observe({
        #browser()
        req(private$new.name %in% names(self$rv$temp.dataIn))

        params <- metadata(self$rv$temp.dataIn[[private$new.name]])$Params

        # if (!is.null(params$method))
        #   updateSelectInput(session, inputId = self$ns('method'), selected = params$method)

        if (!is.null(params$type))
          updateSelectInput(session, inputId = 'type', selected = params$type)

        if (!is.null(params$quantile))
          updateNumericInput(session, 'quantile', value = params$quantile)

        if (!is.null(params$spanLOESS))
          updateNumericInput(session, 'spanLOESS', value = params$spanLOESS)

        if (!is.null(params$varReduction))
          updateCheckboxInput(session, inputId = 'spanLOESS', value = params$varReduction)

      })

      ##
      ##
      ## Calls to other modules
      ##
      ##
      cat(paste0(class(self)[1], '::mod_plots_density_server("densityPlot_Norm"() from - ', self$id, '\n\n'))
      mod_plots_density_server("densityPlot_Norm",
                               obj = reactive({self$rv$dataIn[[length(self$rv$dataIn)]]}),
                               conds = reactive({colData(self$rv$dataIn)[["Condition"]]}),
                               legend = reactive({colData(self$rv$dataIn)[["Sample.name"]]}),
                               base_palette = reactive({DAPAR2::Base_Palette(conditions = colData(self$rv$dataIn)[["Condition"]])}))



      mod_popover_for_help_server("modulePopover_normQuanti",
                                  data = list(title = HTML(paste0("<strong>Normalization quantile</strong>")),
                                              content="lower limit/noise (quantile = 0.15), median (quantile = 0.5). Min value=0, max value=1")
      )

      cat(paste0(class(self)[1], '::self$rv$selectProt <- mod_plots_tracking_server() from - ', self$id, '\n\n'))
      self$rv$selectProt <- mod_plots_tracking_server("master_ProtSelection",
                                                      obj = reactive({self$rv$dataIn[[length(self$rv$dataIn)]]}),
                                                      params = reactive({NULL}),
                                                      keyId = reactive({MultiAssayExperiment::metadata(self$rv$dataIn)[['keyId']]}),
                                                      reset = reactive({self$rv$resetTracking}),
                                                      slave = reactive({FALSE})
      )


      cat(paste0(class(self)[1], '::self$rv$trackFromBoxplot <- mod_plots_intensity_server("boxPlot_Norm") from - ', self$id, '\n\n'))
      self$rv$trackFromBoxplot <- mod_plots_intensity_server("boxPlot_Norm",
                                                             dataIn = reactive({self$rv$dataIn[[length(self$rv$dataIn)]]}),
                                                             meta = reactive({MultiAssayExperiment::metadata(self$rv$dataIn)}),
                                                             conds = reactive({colData(self$rv$dataIn)[['Condition']]}),
                                                             base_palette = reactive({DAPAR2::Base_Palette(conditions=colData(self$rv$dataIn)[['Condition']])}),
                                                             params = reactive({
                                                               if(input$sync)
                                                                 self$rv$selectProt()
                                                               else
                                                                 NULL
                                                             }),
                                                             reset = reactive({self$rv$resetTracking}),
                                                             slave = reactive({input$sync})
      )

    },


    #
    ############################### Description screen ######################################
    #
    Description_server = function(input, output){

      observeEvent(input$btn_validate_Description, ignoreInit = T, ignoreNULL=T, {
        cat(paste0(class(self)[1], "::observeEvent(input$btn_validate_Description from - ", self$id, '\n'))
        private$InitializeDataIn()
        self$ValidateCurrentPos()
      })

      output$datasetDescription <- renderUI({
        req(self$rv$temp.dataIn)
        tagList(
          p(paste0('Dataset description: ', paste0(names(self$rv$temp.dataIn), collapse=", "))),
          if (!is.null(metadata(self$rv$temp.dataIn[[length(self$rv$temp.dataIn)]])$Params))
            p(paste0(names(self$rv$temp.dataIn)[length(self$rv$temp.dataIn)], ' : ', paste0(metadata(self$rv$temp.dataIn[[length(self$rv$temp.dataIn)]])$Params, collapse = ' ')))
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

    Normalize_server = function(input, output){

      output$Screen_Prot_norm_1 <- renderUI({
        isolate({
          tagList(
            div(
              div(
                style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                selectInput(self$ns("method"),"Normalization method",
                            choices = c('None' = 'None', DAPAR2::normalizeMethods.dapar()),
                            selected = metadata(self$rv$temp.dataIn[[private$new.name]])$Params$method,
                            width='200px')
              ),
              div(
                style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                shinyjs::hidden(selectInput(self$ns("type"), "Normalization type",
                                   choices = c("overall", "within conditions"),
                                   selected = metadata(self$rv$temp.dataIn[[private$new.name]])$Params$type,
                                   width='150px'))
              ),
              div(
                style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                shinyjs::hidden(numericInput(self$ns("spanLOESS"),
                                 "Span",
                                 value = if(!is.null(metadata(self$rv$temp.dataIn[[private$new.name]])$Params$spanLOESS))
                                   metadata(self$rv$temp.dataIn[[private$new.name]])$Params$spanLOESS
                                 else 0.7,
                                 min = 0,
                                 max = 1,
                                 width='100px')),
                uiOutput(self$ns("test_spanLOESS")),
                uiOutput(self$ns("choose_normalizationQuantile")),
                uiOutput(self$ns("choose_normalizationScaling"))
              ),
              div(
                style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                shinyjs::hidden(
                  div(id = self$ns('DivMasterProtSelection'),
                    mod_plots_tracking_ui(self$ns('master_ProtSelection')))
                  )
              ),
              div(
                style="display:inline-block; vertical-align: middle; padding-right: 20px;",
                shinyjs::hidden(actionButton(self$ns("perform.normalization"),
                                    "Perform normalization",
                                    class = actionBtnClass,
                                    width="170px"))
              )
            ),
            uiOutput(self$ns("helpForNormalizationMethods")),
            tags$hr(),
            fluidRow(
              column(width=4, mod_plots_density_ui(self$ns("densityPlot_Norm"))),
              column(width=4,
                     shinyjs::hidden(checkboxInput(self$ns("sync"),
                                          "Synchronise with selection above",
                                          value = FALSE)
                            ),
                     withProgress(message = 'Building plot',detail = '', value = 0, {
                       mod_plots_intensity_ui(self$ns("boxPlot_Norm"))
                     })),
              column(width=4, highchartOutput(self$ns("viewComparisonNorm_UI"))
              )
            )
          )
        })

      })

      # observeEvent(lapply(names(input), function(x){input[[x]]}), ignoreInit=TRUE,  {
      #   print('##################################################"')
      #   print(paste0(input, collapse=' '))
      #   lapply(names(input), function(x){
      #     input[[x]] <- input[[x]]
      #   })
      # })

#
#       output$test_spanLOESS <- renderUI({
#         req(input$spanLOESS)
#         if (!is.numeric(input$spanLOESS)){
#           tags$p("Please choose a number.")
#         }
#       })
#
#
#       output$test_normQuant <- renderUI({
#         req(input$quantile)
#         if (!is.numeric(input$quantile)){
#           tags$p("Please choose a number.")
#         }
#       })

      output$helpForNormalizationMethods <- renderUI({
        req(input$method != "None")

        switch(input$method,
               GlobalQuantileAlignment= txt <- "This method proposes a normalization of important
         magnitude that should be cautiously used. It proposes to align the quantiles of all
         the replicates as described in [Other ref. 1]; practically it amounts to replace
         abundances by order statistics.",
               QuantileCentering = txt <- "These methods propose to shift the sample distributions
         (either all of them at once, or within each condition at a time) to align a specific
         quantile: the median (under the assumption that up-regulations and down-regulations
         are equally frequent), the 15% quantile (under the assumption that the signal/noise ratio is
         roughly the same in all the samples), or any other user's choice.",
               MeanCentering = txt <- "These methods propose to shift the sample distributions (either all
         of them at once, or within each condition at a time) to align their means. It is also possible
         to force unit variance (or not).",
               SumByColumns = txt <- "These methods propose normalizations of important magnitude that should be cautiously used.
         It operates on the original scale (not the log2 one) and propose to normalize each abundance by the
         total abundance of the sample (so as to focus on the analyte proportions among each sample).",
               LOESS = txt <- "This method proposes to apply a cyclic LOESS [Other ref. 4, 5] normalization to the data
         (either all of them at once, or on each condition independently). It relates to  a
         combination of multiple regression models. The user can tune the regression span (an higher span smooths
         the fit more, while a lower span captures more trends).",
               vsn = txt <- "This method proposes to apply the Variance Stabilization Normalization [Other ref. 6] to the
         data (either all of them at once, or on each condition independently). No specific parameters required."
        )

        tags$p(txt)
      })




      output$choose_normalizationQuantile <- renderUI({
        req(input$method == "QuantileCentering")

        tagList(
          mod_popover_for_help_ui(self$ns("modulePopover_normQuanti")),
          numericInput(self$ns("quantile"), NULL,
                    value = if(!is.null(metadata(self$rv$temp.dataIn[[private$new.name]])$Params$quantile))
                      metadata(self$rv$temp.dataIn[[private$new.name]])$Params$quantile
                    else 0.15,
                    min = 0,
                    max = 1,
                    width='150px')
          #uiOutput(self$ns("test_normQuant"))
        )

      })


      output$choose_normalizationScaling <- renderUI({
        req(input$method == "MeanCentering")
        checkboxInput(self$ns("varReduction"),
                      "Include variance reduction",
                      value = if(!is.null(metadata(self$rv$temp.dataIn[[private$new.name]])$Params$varReduction))
                        metadata(self$rv$temp.dataIn[[private$new.name]])$Params$varReduction
                      else FALSE)
      })


      observeEvent(input$method, {
        req(self$rv$dataIn)
        if (input$method == "None"){
          self$rv$dataIn <- self$rv$temp.dataIn
          self$rv$resetTracking <- TRUE
        }
        shinyjs::toggle("perform.normalization", condition = input$method != "None")
        shinyjs::toggle("spanLOESS", condition = input$method == "LOESS")

        shinyjs::toggle("type",
                        condition=( input$method %in% c(DAPAR2::normalizeMethods.dapar()[-which(DAPAR2::normalizeMethods.dapar()=="GlobalQuantileAlignment")])))

        cond <- MultiAssayExperiment::metadata(self$rv$dataIn[[length(self$rv$dataIn)]])$typeOfData == 'protein'
        trackAvailable <- input$method %in% DAPAR2::normalizeMethods.dapar(withTracking=TRUE)
        shinyjs::toggle('DivMasterProtSelection', condition= cond && trackAvailable)
        shinyjs::toggle('sync', condition= cond && trackAvailable)
      })


      GetIndicesOfSelectedProteins <- reactive({
        req(self$rv$trackFromBoxplot())
        #print('in GetIndicesOfSelectedProteins')
        #print(self$rv$trackFromBoxplot())
        ind <- NULL
        ll <- SummarizedExperiment::rowData(self$rv$dataIn[[length(self$rv$dataIn)]])[,MultiAssayExperiment::metadata(self$rv$dataIn)$keyId]
        tt <- self$rv$trackFromBoxplot()$typeSelect
        switch(tt,
               ProteinList = ind <- self$rv$trackFromBoxplot()$list.indices,
               Random = ind <- self$rv$trackFromBoxplot()$rand.indices,
               Column = ind <- self$rv$trackFromBoxplot()$col.indices
        )
        if (length(ind)==0)
          ind <- NULL

        ind
      })


      ##' Reactive behavior : Normalization of data
      ##' @author Samuel Wieczorek
      observeEvent(input$perform.normalization, ignoreInit = TRUE, {
        input$method
        self$rv$dataIn
        # isolate({
        conds <- colData(self$rv$dataIn)$Condition

        switch(input$method,
               None = self$rv$dataIn <- self$rv$temp.dataIn,

               GlobalQuantileAlignment = {
                 self$rv$dataIn <- DAPAR2::normalizeD(object = self$rv$dataIn,
                                                      i = length(self$rv$dataIn),
                                                      name = private$new.name,
                                                      method='GlobalQuantileAlignment'
                 )
               },

               QuantileCentering = {
                 quant <- if (!is.null(input$quantile))
                   as.numeric(input$quantile)
                 else NA

                 self$rv$dataIn <- DAPAR2::normalizeD(object = self$rv$dataIn,
                                                      i = length(self$rv$dataIn),
                                                      name = private$new.name,
                                                      method = 'QuantileCentering',
                                                      conds = conds,
                                                      type = input$type,
                                                      subset.norm = GetIndicesOfSelectedProteins(),
                                                      quantile = quant
                 )

               } ,

               MeanCentering = {
                 self$rv$dataIn <- DAPAR2::normalizeD(object = self$rv$dataIn,
                                                      i = length(self$rv$dataIn),
                                                      name = private$new.name,
                                                      method = 'MeanCentering',
                                                      conds = conds,
                                                      type = input$type,
                                                      subset.norm = GetIndicesOfSelectedProteins(),
                                                      scaling = input$varReduction
                 )
               },

               SumByColumns = {
                 self$rv$dataIn <- DAPAR2::normalizeD(object = self$rv$dataIn,
                                                      i = length(self$rv$dataIn),
                                                      name = private$new.name,
                                                      method = 'SumByColumns',
                                                      conds = conds,
                                                      type = input$type,
                                                      subset.norm = GetIndicesOfSelectedProteins()
                 )
               },

               LOESS = {
                 self$rv$dataIn <- DAPAR2::normalizeD(object = self$rv$dataIn,
                                                      i = length(self$rv$dataIn),
                                                      name = private$new.name,
                                                      method = 'LOESS',
                                                      conds = conds,
                                                      type = input$type,
                                                      span = as.numeric(input$spanLOESS)
                 )
               },

               vsn = {
                 self$rv$dataIn <- DAPAR2::normalizeD(object = self$rv$dataIn,
                                                      i = length(self$rv$dataIn),
                                                      name = private$new.name,
                                                      method = 'vsn',
                                                      conds = conds,
                                                      type = input$type)
               }
        )
        self$ValidateCurrentPos()
      })


      #######################
      output$viewComparisonNorm_UI <- renderHighchart({
        req(self$rv$dataIn)
        GetIndicesOfSelectedProteins()
        #print(GetIndicesOfSelectedProteins())

        hc <- DAPAR2::compareNormalizationD_HC(qDataBefore = assay(self$rv$temp.dataIn, length(self$rv$temp.dataIn)),
                                               qDataAfter = assay(self$rv$dataIn, length(self$rv$dataIn)),
                                               conds= MultiAssayExperiment::colData(self$rv$dataIn)$Condition,
                                               palette = DAPAR2::Base_Palette(conditions = colData(self$rv$dataIn)$Condition),
                                               subset.view = GetIndicesOfSelectedProteins(),
                                               n = 50)
        hc
      })


    },

Normalize_ui = function(){
      uiOutput(self$ns('Screen_Prot_norm_1'))
    },


    ########################### SCREEN STEP 2 ###################################
    Save_server = function(input, output){
      ## Logics to implement: here, we must take the last data not null
      # in previous datas. The objective is to take account
      # of skipped steps
      observeEvent(input$btn_validate_Step2, ignoreInit = T, {
        if (input$method != "None") {
          MultiAssayExperiment::metadata(self$rv$dataIn[[length(self$rv$dataIn)]])$Params <- list(
            method = input$method,
            type = input$type,
            quantile = input$quantile,
            varReduction = input$varReduction,
            spanLOESS = input$spanLOESS
          )
        }

        self$ValidateCurrentPos()
      })

    },

    Save_ui = function(){
      tagList(
        actionButton(self$ns("btn_validate_Step2"),
                     "Save normalization",
                     class = actionBtnClass,
                     width="170px")
      )
    }
  )
)
