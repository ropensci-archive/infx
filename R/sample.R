
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

  params <- lapply(exp_id_str(x), function(exp) list(token, exp))

  res <- make_requests(api_url("gis"), "listSamplesForExperiment", params)

  as_json_vec(do.call(c, res))
}

#' @rdname list_samples
#' @export
#' 
list_samples.PlateIdentifier <- function(token, x, ...) {

  params <- lapply(as_json_vec(x), function(plate) list(token, plate))

  res <- make_requests(api_url("sas"), "getPlateSample", params)

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

  params <- lapply(as_json_vec(x),
                   function(well) list(token, well))

  res <- make_requests(api_url("sas"), "getWellSample", params)

  as_json_vec(do.call(c, res))
}

#' @rdname list_samples
#' @export
#' 
list_sample_types <- function(token)
  make_request(api_url("gis"), "listSampleTypes", list(token))
