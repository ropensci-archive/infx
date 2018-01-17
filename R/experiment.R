
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

#' List experiment ids
#'
#' Given a login token, all available experiment ids are listed.
#' 
#' @inheritParams logout_openbis
#' 
#' @return A a list of ExperimentIdentifier objects.
#' 
#' @export
#' 
list_experiment_ids <- function(token)
  request_openbis("listExperiments", token, "IScreeningApiServer")
