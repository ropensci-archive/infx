
#' List image metadata
#'
#' Experiment metadata can be fetched using `list_image_metadata()`,
#' which accepts either a set of `Experiment`, `ExperimentIdentifier` or
#' `ImageDatasetReference` objects and returns all corresponding meta data as
#' `json_vec` of either `ExperimentImageMetadata`, in case experiment objects
#' were used or `ImageDatasetMetadata` objects, in case
#' `ImageDatasetReference` objects were used for calling
#' `list_image_metadata()`.
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

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.ExperimentIdentifier <- function(token, x, ...) {

  res <- lapply(as_json_vec(x), function(y)
    request_openbis("getExperimentImageMetadata", list(token, y),
                    "IScreeningApiServer"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.Experiment <- function(token, x, ...)
  list_image_metadata(token, exp_to_expid(x))

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.ImageDatasetReference <- function(token, x, ...)
  request_openbis("listImageMetadata", list(token, as_json_vec(x)),
                  "IDssServiceRpcScreening")
