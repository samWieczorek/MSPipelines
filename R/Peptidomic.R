#' @title
#' Simple pipeline for test purpose
#'
#' @description
#' This pipeline inherits from the class `Pipeline` and have only one private field `.config`.
#'
#' @importFrom R6 R6Class
#' @importFrom Magellan Pipeline
#'
#' @export
#'
Peptidomic = R6::R6Class(
  "Peptidomic",
  inherit = Magellan::Pipeline,

  private = list(

    # .config = list(name = 'Protein',
    #                steps = c('Description', 'Filtering', 'Normalization', 'Imputation', 'HypothesisTest'),
    #                mandatory = c(T, F, F, F, F)
    # )

    .config = list(name = 'Peptidomic',
                   steps = c('Description', 'Normalization'),
                   mandatory = c(T, F)
    )
  ),

  public = list(

  )
)
