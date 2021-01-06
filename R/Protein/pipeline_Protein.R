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
Protein = R6::R6Class(
  "Protein",
  inherit = Magellan::Pipeline,

  private = list(

    .config = list(name = 'Protein',
                   steps = c('Description', 'Filtering', 'Normalization', 'Imputation', 'HypothesisTest'),
                   mandatory = c(T, F, F, F, F)
    )
  ),

  public = list()
)
