
source(file.path("../../../R","mod_plots_legend_colored_exprs.R"), local=TRUE)$value


ui <- fluidPage(
  mod_plots_legend_colored_exprs_ui("legend_colored_exprs")
)


server <- function(input, output, session) {

  mod_plots_legend_colored_exprs_server('legend_colored_exprs')

}


shinyApp(ui=ui, server=server)
