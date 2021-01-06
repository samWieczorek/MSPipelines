#' @title xxx
#'
#' @description
#' Get the list of pipelines available in the package
#'
#' @export
#'
Pipelines <- function(){
  c('Protein', 'Peptide', 'P2p', 'Peptidomic')
}



#' @title xxx
#'
#' @description
#' Adds one or more items to the dataset. This function is specific of the
#' type of dataset.
#'
#' @importFrom QFeatures addAssay
#'
#' @return
#' The dataset minus some items
#'
#' @export
#'
Add_Item_to_Dataset <- function(dataset, name){
  QFeatures::addAssay(dataset,
                      dataset[[length(dataset)]],
                      name=name)
}

#' @title xxx
#'
#' @description
#' Removes one or more items from the dataset. This function is specific of the
#' type of dataset.
#'
#' @return
#' The dataset minus some items
#'
#' @export
#'
Keep_Items_from_Dataset <- function(dataset, range){
  dataset[ , , range]
}
