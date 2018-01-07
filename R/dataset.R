
#' List dataset types
#'
#' Given a login token, all available dataset types are listed.
#' 
#' @inheritParams logout_openbis
#' 
#' @return A a list of DataSetType objects.
#' 
#' @export
#' 
list_dataset_types <- function(token)
  request_openbis("listDataSetTypes", token)
