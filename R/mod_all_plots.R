#' all_plots UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @importFrom shiny NS tagList
#'
#' @export
#'
mod_all_plots_ui <- function(id){
  ns <- NS(id)
  tagList(
    shinyjs::useShinyjs(),
    fluidPage(

      tags$style(".topimg {
                            margin-left:-25px;
                            margin-right:-20px;
                            margin-top:-20px;
                            margin-bottom:-20px;
                            padding: 10px;
                          }"),

      div( style="display:inline-block; vertical-align: middle; padding: 7px",
           tags$button(
             id = ns("btn_quanti"),
             class = "btn action-button",
             div(class="topimg",imageOutput(ns('plot_quanti_small'), height=30, width=30))
           )
      ),
      div( style="display:inline-block; vertical-align: middle; padding: 7px",
           tags$button(
             id = ns("btn_intensity"),
             class = "btn action-button",
             div(class="topimg",imageOutput(ns('plot_intensity_small'), height=30, width=30))
           )
      ),
      div(style="display:inline-block; vertical-align: middle; padding: 7px",
          tags$button(
            id = ns("btn_pca"),
            class = "btn action-button",
            div(class="topimg",imageOutput(ns('plot_pca_small'), height=30, width=30))
          )
      ),
      div(style="display:inline-block; vertical-align: middle; padding: 7px",
          tags$button(
            id = ns("btn_var_dist"),
            class = "btn action-button",
            div(class="topimg",imageOutput(ns('plot_var_dist_small'), height=30, width=30))
          )
      ),
      div(style="display:inline-block; vertical-align: middle; padding: 7px",
          tags$button(
            id = ns("btn_corr_matrix"),
            class = "btn action-button",
            div(class="topimg",imageOutput(ns('plot_corr_matrix_small'), height=30, width=30))
          )
      ),
      div(style="display:inline-block; vertical-align: middle; padding: 7px",
          tags$button(
            id = ns("btn_heatmap"),
            class = "btn action-button",
            div(class="topimg",imageOutput(ns('plot_heatmap_small'), height=30, width=30))
          )
      ),
      div(style="display:inline-block; vertical-align: middle; padding: 7px",
          tags$button(
            id = ns("btn_group_mv"),
            class = "btn action-button",
            div(class="topimg",imageOutput(ns('plot_group_mv_small'), height=30, width=30))
          )
      )
    ),

    br(),br(),br(),
    shinyjs::hidden(div(id=ns('div_plot_quanti_large'),mod_plots_se_explorer_ui(ns('plot_quanti_large')))),
    shinyjs::hidden(div(id=ns('div_plot_intensity_large'),mod_plots_intensity_ui(ns('plot_intensity_large')))),
    shinyjs::hidden(div(id=ns('div_plot_pca_large'),mod_plots_pca_ui(ns('plot_pca_large')))),
    shinyjs::hidden(div(id=ns('div_plot_var_dist_large'),mod_plots_var_dist_ui(ns('plot_var_dist_large')))),
    shinyjs::hidden(div(id=ns('div_plot_corr_matrix_large'),mod_plots_corr_matrix_ui(ns('plot_corr_matrix_large')))),
    shinyjs::hidden(div(id=ns('div_plot_heatmap_large'),mod_plots_heatmap_ui(ns('plot_heatmap_large')))),
    shinyjs::hidden(div(id=ns('div_plot_group_mv_large'),mod_plots_group_mv_ui(ns('plot_group_mv_large'))))
  )

}

#' @description
#' all_plots Server Function
#'
#' @param id xxxx
#' @param dataIn xxxx
#' @param indice xxx
#'
#' @export
#'
#' @noRd
mod_all_plots_server <- function(id, dataIn, indice){

  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    .width <- .height <- 40
    ll <- c('quanti', 'intensity', 'pca', 'var_dist', 'corr_matrix', 'heatmap', 'group_mv')


    rv <- reactiveValues(
      current.plot = NULL,
      current.obj = NULL,
      colData = NULL,
      conditions = NULL
    )

    observeEvent(dataIn(), {
      rv$colData <- SummarizedExperiment::colData(dataIn())
      rv$metadata <- MultiAssayExperiment::metadata(dataIn())
      rv$conditions <- SummarizedExperiment::colData(dataIn())[['Condition']]
      rv$current.obj <- dataIn()[[indice()]]
    })


    mod_plots_se_explorer_server('plot_quanti_large',
                                 obj = reactive({rv$current.obj}),
                                 originOfValues = reactive({ rv$metadata[['OriginOfValues']] }),
                                 colData = reactive({ rv$colData })
    )


    mod_plots_intensity_server('plot_intensity_large',
                               dataIn=reactive({rv$current.obj}),
                               meta = reactive({ rv$metadata }),
                               conds = reactive({ rv$conditions }),
                               params = reactive({NULL}),
                               reset = reactive({FALSE}),
                               slave = reactive({FALSE}),
                               base_palette = reactive({
                                 DAPAR2::Example_Palette(
                                   rv$conditions,
                                   DAPAR2::Base_Palette(conditions = rv$conditions)
                                 )
                                 })
                               )


    mod_plots_pca_server('plot_pca_large',
                         obj=reactive({rv$current.obj}),
                         coldata = reactive({ rv$colData })
    )


    mod_plots_var_dist_server('plot_var_dist_large',
                              obj=reactive({rv$current.obj}),
                              conds = reactive({ rv$conditions}),
                              base_palette = reactive({
                                DAPAR2::Example_Palette(
                                rv$conditions,
                                DAPAR2::Base_Palette(conditions = rv$conditions)
                              )
                                })
                              )

    mod_plots_corr_matrix_server('plot_corr_matrix_large',
                                 obj = reactive({rv$current.obj}),
                                 names = reactive({NULL}),
                                 gradientRate = reactive({0.9})
                                 )


    mod_plots_heatmap_server("plot_heatmap_large",
                             obj = reactive({rv$current.obj}),
                             conds = reactive({ Srv$conditions })
    )


    mod_plots_group_mv_server("plot_group_mv_large",
                              obj = reactive({rv$current.obj}),
                              conds = reactive({ rv$colData }),
                              base_palette = reactive({
                                DAPAR2::Example_Palette(
                                  rv$conditions,
                                  DAPAR2::Base_Palette(conditions = rv$conditions)
                                )
                              })
    )

    # Update the value of current.plot
    observeEvent(input$btn_quanti,{rv$current.plot <- 'quanti'})
    observeEvent(input$btn_intensity,{rv$current.plot <- 'intensity'})
    observeEvent(input$btn_pca,{rv$current.plot <- 'pca'})
    observeEvent(input$btn_var_dist,{rv$current.plot <- 'var_dist'})
    observeEvent(input$btn_corr_matrix,{rv$current.plot <- 'corr_matrix'})
    observeEvent(input$btn_heatmap,{rv$current.plot <- 'heatmap'})
    observeEvent(input$btn_group_mv,{rv$current.plot <- 'group_mv'})




    observeEvent(rv$current.plot,{
      req(rv$current.obj)
      for (i in ll){
        if (i == rv$current.plot) {
          shinyjs::show(paste0('div_plot_',rv$current.plot,'_large'))
        } else {
          shinyjs::hide(paste0('div_plot_',i,'_large'))
        }
      }

    })





    ################### Plot for correlation matrix
    output$plot_corr_matrix_small <- renderImage({
      filename <- system.file("extdata/images", "desc_corrmatrix.png", package="MSPipelines")
      list(src = filename,
           width = .width,
           height = .height)
    }, deleteFile = FALSE)




    ##### Plots for missing values

    output$plot_group_mv_small <- renderImage({
      filename <- system.file("inst/extdata/images", "desc_group_mv.png", package="MSPipelines")
      list(src = filename,
           width = .width,
           height = .height)
    }, deleteFile = FALSE)




    ############# Plots for SE explorer
    output$plot_quanti_small <- renderImage({
      filename <- system.file("inst/extdata/images", "desc_quantiData.png", package="MSPipelines")
      list(src = filename,
           width = .width,
           height = .height)
    }, deleteFile = FALSE)




    ##### Code for heatmap

    output$plot_heatmap_small <- renderImage({
      filename <- system.file("inst/extdata/images", "desc_heatmap.png", package="MSPipelines")
      list(src = filename,
           width = .width,
           height = .height)
    }, deleteFile = FALSE)


    #### Code for PCA
    output$plot_pca_small <- renderImage({
      filename <- system.file("inst/extdata/images", "desc_pca.png", package="MSPipelines")
      list(src = filename,
           width = .width,
           height = .height)
    }, deleteFile = FALSE)




    ################################################
    #### Code for intensity plots
    output$plot_intensity_small <- renderImage({
      filename <- system.file("inst/extdata/images", "desc_intdistrib.png", package="MSPipelines")
      list(src = filename,
           width = .width,
           height = .height)
    }, deleteFile = FALSE)




    ############ Module for variance distribution plots

    output$plot_var_dist_small <- renderImage({
      filename <- system.file("inst/extdata/images", "desc_varDist.jpg", package="MSPipelines")
      list(src = filename,
           width = .width,
           height = .height)
    }, deleteFile = FALSE)

    return(NULL)

  })

}

## To be copied in the UI
# mod_all_plots_ui("all_plots_ui_1")

## To be copied in the server
# callModule(mod_all_plots_server, "all_plots_ui_1")

