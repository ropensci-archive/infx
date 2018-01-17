
#' List experiments
#'
#' Given a login token and a set of experiment ids, all available experiments
#' are listed.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned experiments, e.g. a set of
#' `ExperimentIdentifier` objects.
#' 
#' @return A a list of ExperimentIdentifier objects.
#' 
#' @export
#' 
list_experiments <- function(x, ...)
  UseMethod("list_experiments")

#' @export
list_experiments.ExperimentIdentifier <- function(x, token, ...) {

  if (is_json_vec(x)) {
    exps <- lapply(x, function(y) {
      paste0("/", y[c("spaceCode", "projectCode", "experimentCode")],
             collapse = "")
    })
  } else if (is_json_class(x)) {
    exps <- paste0("/", x[c("spaceCode", "projectCode", "experimentCode")],
                   collapse = "")
    exps <- list(exps)
  }

  request_openbis("listExperiments", list(token, exps))
}

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
