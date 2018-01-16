
#' List experiment types
#'
#' Given a login token, all available experiment types are listed.
#' 
#' @inheritParams logout_openbis
#' 
#' @return A a list of ExperimentType objects.
#' 
#' @export
#' 
list_experiment_types <- function(token)
  request_openbis("listExperimentTypes", token)
