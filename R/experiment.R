
#' List experiments
#'
#' Given a login token and a set of experiment ids, all available experiments
#' are listed. The two utility functions `list_experiment_ids()` and
#' `list_experiment_types()` list all available experiment ids and experiment
#' types of the queried openBIS instance.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned experiments, e.g. a set of
#' `ExperimentIdentifier` objects.
#' @param ... Generic compatibility
#' 
#' @export
#' 
list_experiments <- function(x, ...)
  UseMethod("list_experiments")

#' @export
list_experiments.ExperimentIdentifier <- function(x, token, ...) {

  fields <- c("spaceCode", "projectCode", "experimentCode")

  assert_that(has_fields(x, fields))

  if (is_json_vec(x))
    exps <- lapply(x, function(y) paste0("/", y[fields], collapse = ""))
  else if (is_json_class(x))
    exps <- list(paste0("/", x[fields], collapse = ""))

  request_openbis("listExperiments", list(token, exps))
}

#' @rdname list_experiments
#' @export
#' 
list_experiment_ids <- function(token)
  request_openbis("listExperiments", token, "IScreeningApiServer")

#' @rdname list_experiments
#' @export
#' 
list_experiment_types <- function(token)
  request_openbis("listExperimentTypes", token)
