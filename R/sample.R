
#' List samples
#'
#' Given a login token, all samples available to the corresponding user on the
#' queried openBIS instance are listed for an experiment, plate or well. If
#' multiple objects are used for limiting the search (e.g. a `json_vec` of
#' wells), an API call for each object has to be made.
#' 
#' @section TODO:
#' Add class specific functions to generic `list_samples()` for
#' classes `PlateIdentifier` and `WellIdentifier` (as well as `Plate` and
#' `Well`).
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned samples, e.g. a set of
#' `ExperimentIdentifier` or `Experiment` objects.
#' @param ... Generic compatibility
#' 
#' @export
#' 
list_samples <- function(token, x, ...)
  UseMethod("list_samples", x)

#' @rdname list_samples
#' @export
#' 
list_samples.ExperimentIdentifier <- function(token, x, ...) {

  fields <- c("spaceCode", "projectCode", "experimentCode")

  assert_that(has_fields(x, fields))

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  exps <- lapply(x, function(y) paste0("/", y[fields], collapse = ""))

  res <- lapply(exps, function(exp)
    request_openbis("listSamplesForExperiment", list(token, exp)))

  as_json_vec(do.call(c, res))
}

#' @rdname list_samples
#' @export
#' 
list_samples.Experiment <- function(token, x, ...) {

  assert_that(has_fields(x, "identifier"))

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  exps <- lapply(x, `[[`, "identifier")

  res <- lapply(exps, function(exp)
    request_openbis("listSamplesForExperiment", list(token, exp)))

  as_json_vec(do.call(c, res))
}

#' List sample types
#'
#' Given a login token, all available sample types are listed.
#' 
#' @inheritParams logout_openbis
#' 
#' @return A a list of SampleType objects.
#' 
#' @export
#' 
list_sample_types <- function(token)
  request_openbis("listSampleTypes", token)
