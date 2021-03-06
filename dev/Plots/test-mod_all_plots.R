library(shiny)
library(highcharter)

source(file.path("../../../R", "mod_plots_intensity.R"), local = TRUE)$value
source(file.path("../../../R", "mod_plots_tracking.R"), local = TRUE)$value
source(file.path("../../../R", "mod_plots_legend_colored_exprs.R"), local = TRUE)$value
source(file.path("../../../R", "mod_plots_corr_matrix.R"), local = TRUE)$value
source(file.path("../../../R", "mod_plots_heatmap.R"), local = TRUE)$value
source(file.path("../../../R", "mod_plots_group_mv.R"),  local = TRUE)$value
source(file.path("../../../R", "mod_plots_se_explorer.R"),  local = TRUE)$value
source(file.path("../../../R", "mod_plots_var_dist.R"), local = TRUE)$value
source(file.path("../../../R", "mod_plots_pca.R"), local = TRUE)$value
source(file.path("../../../R", "mod_all_plots.R"), local=TRUE)$value

source(file.path("../../../R", "mod_popover_for_help.R"), local = TRUE)$value
source(file.path("../../../R", "global.R"), local = TRUE)$value
source(file.path("../../../R", "mod_format_DT.R"), local = TRUE)$value


ui <- fluidPage(
  mod_all_plots_ui('plots')
)


server <- function(input, output, session) {

  utils::data(Exp1_R25_prot, package='DAPARdata2')

  obj <- QFeatures::addAssay(Exp1_R25_prot, (QFeatures::filterNA(Exp1_R25_prot,i=2))[[2]], "original_log_NAfiltered")

  mod_all_plots_server('plots',
                       dataIn = reactive({obj})
                       )
}


shinyApp(ui, server)
