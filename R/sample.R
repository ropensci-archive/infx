
#' List samples
#'
#' Given a login token, all samples available to the corresponding user on the
#' queried openBIS instance are listed for an experiment, plate or well. If
#' multiple objects are used for limiting the search (e.g. a `json_vec` of
#' wells), an API call for each object has to be made. Furthermore, all
#' available sample types can be listed using `list_sample_types()`.
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
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "listSamplesForExperiment")}
#' * \Sexpr{infx::docs_link("sas", "getPlateSample")}
#' * \Sexpr{infx::docs_link("sas", "getWellSample")}
#' * \Sexpr{infx::docs_link("gis", "listSampleTypes")}
#' 
#' @export
#' 
list_samples <- function(token, x, ...)
  UseMethod("list_samples", x)

#' @rdname list_samples
#' @export
#' 
list_samples.ExperimentIdentifier <- function(token, x, ...) {
  list_samples_for_exp(token, x)
}

#' @rdname list_samples
#' @export
#' 
list_samples.Experiment <- function(token, x, ...) {
  list_samples_for_exp(token, x)
}

list_samples_for_exp <- function(token, x) {
  res <- lapply(exp_id_str(x), function(exp)
    request_openbis("listSamplesForExperiment", list(token, exp)))

  as_json_vec(do.call(c, res))
}

#' @rdname list_samples
#' @export
#' 
list_samples.PlateIdentifier <- function(token, x, ...) {

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  res <- lapply(x, function(plate)
    request_openbis("getPlateSample", list(token, plate),
                    "IScreeningApiServer"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_samples
#' @export
#' 
list_samples.Plate <- function(token, x, ...)
  list_samples(token, plate_to_plateid(x))

#' @rdname list_samples
#' @export
#' 
list_samples.WellIdentifier <- function(token, x, ...) {

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  res <- lapply(x, function(well) {
    request_openbis("getWellSample", list(token, remove_null(well)),
                    "IScreeningApiServer")
  })

  as_json_vec(do.call(c, res))
}

#' @rdname list_samples
#' @export
#' 
list_sample_types <- function(token)
  request_openbis("listSampleTypes", token)
