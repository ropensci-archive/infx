
#' List image metadata
#'
#' Experiment metadata can be fetched using `list_image_metadata()`,
#' which accepts either a set of `Experiment` or `ExperimentIdentifier`
#' objects and returns all corresponding metadata as `json_vec` of
#' `ExperimentImageMetadata` objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned experiments, e.g. a set of
#' `ExperimentIdentifier` or `Project` objects.
#' @param ... Generic compatibility
#' 
#' @export
#' 
list_image_metadata <- function(token, x, ...)
  UseMethod("list_image_metadata", x)

#' @rdname list_experiments
#' @export
#' 
list_image_metadata.ExperimentIdentifier <- function(token, x, ...) {

  res <- lapply(as_json_vec(x), function(y)
    request_openbis("getExperimentImageMetadata", list(token, y),
                    "IScreeningApiServer"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_experiments
#' @export
#' 
list_image_metadata.Experiment <- function(token, x, ...)
  list_image_metadata(token, exp_to_expid(x))
