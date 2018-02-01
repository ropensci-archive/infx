
#' List experiments
#'
#' Given a login token and a set of experiment ids or projects, all available
#' experiments are listed. In case of projects being used as filtering
#' criterion, it is also possible to limit the search to experiments that are
#' of a certain type and exclude experiments that either have no datasets or
#' samples associated. If several experiment types are requested (default: all
#' available), an API call per experiment type has to be made.
#' 
#' The two utility functions `list_experiment_ids()` and
#' `list_experiment_types()` list all available experiment ids and experiment
#' types of the queried openBIS instance. Furthermore, experiment objects can
#' be converted to experiment id objects using `exp_to_expid()`. This function
#' does not incur an API call and can act on both a single experiment object
#' or a vector of experiment objects (passed as a `json_vec` object) and will
#' return a `json_vec` object of type `ExperimentIdentifier`.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned experiments, e.g. a set of
#' `ExperimentIdentifier` or `Project` objects.
#' @param types Either a single of vector of `ExperimentType` objects
#' @param require A switch to require the resulting experiments to contain a
#' nonzero number of dataset or sample. Default behavior is no requirement.
#' @param ... Generic compatibility
#' 
#' @export
#' 
list_experiments <- function(token, x, ...)
  UseMethod("list_experiments", x)

#' @rdname list_experiments
#' @export
#' 
list_experiments.ExperimentIdentifier <- function(token, x, ...)
  request_openbis("listExperiments", list(token, exp_id_str(x)))

#' @rdname list_experiments
#' @export
#' 
list_experiments.Project <- function(token,
                                     x,
                                     types = list_experiment_types(token),
                                     require = c(NA, "DataSets", "Samples"),
                                     ...) {

  check_and_extract <- function(obj, fields) {
    assert_that(has_fields(obj, fields))
    if (!is_json_vec(obj))
      obj <- as_json_vec(obj)
    lapply(obj, `[`, fields)
  }

  require <- match.arg(require)

  x <- check_and_extract(x, c("spaceCode", "code"))
  types <- check_and_extract(types, "code")

  if (is.na(require))
    fun <- "listExperiments"
  else if (require == "DataSets")
    fun <- "listExperimentsHavingDataSets"
  else
    fun <- "listExperimentsHavingSamples"

  res <- lapply(types, function(type)
    request_openbis(fun, list(token, x, as.character(type))))

  as_json_vec(do.call(c, res))
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

#' @rdname list_experiments
#' @export
#' 
exp_to_expid <- function(x) {

  convert <- function(x) {
    id <- unlist(strsplit(sub("^/", "", x[["identifier"]]), "/"))
    assert_that(length(id) == 3L)
    json_class(permId = x[["permId"]], spaceCode = id[1], projectCode = id[2],
               experimentCode = id[3], class = "ExperimentIdentifier")
  }

  fields <- c("permId", "identifier")

  assert_that(inherits(x, "Experiment"),
              has_fields(x, fields))

  if (is_json_class(x))
    res <- convert(x)
  else
    res <- lapply(x, convert)

  as_json_vec(res)
}

#' Extract experiment string
#' 
#' Experiments can be uniquely identified by a string made up of space code,
#' project code and experiment code. This function extracts the relevant fields
#' from experiment or experiment id objects and returns a vector of experiment
#' strings.
#' 
#' @param x Experiment object(s).
#' @param ... Generic compatibility.
#' 
#' @keywords internal
#' @export
#' 
exp_id_str <- function(x, ...)
  UseMethod("exp_id_str")

#' @rdname exp_id_str
#' @keywords internal
#' @export
#' 
exp_id_str.ExperimentIdentifier <- function(x, ...) {

  fields <- c("spaceCode", "projectCode", "experimentCode")

  assert_that(has_fields(x, fields))

  lapply(as_json_vec(x), function(y) paste0("/", y[fields], collapse = ""))
}

#' @rdname exp_id_str
#' @keywords internal
#' @export
#' 
exp_id_str.Experiment <- function(x, ...) {

  assert_that(has_fields(x, "identifier"))

  lapply(as_json_vec(x), `[[`, "identifier")
}
