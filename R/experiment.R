
#' List experiments
#'
#' Experiments can be represented by two object classes: `Experiment` and
#' `ExperimentIdentifier`. The fields that make up an `ExperimentIdentifier`
#' object are a subset of those required for an `Experiment` object. Therefore
#' an experiment can be turned into an experiment id object without an API
#' call, using the function `as_experiment_id()`. The reverse can be achieved by
#' calling `list_experiments()` on experiment id objects. In general,
#' experiments and experiment id objects can be listed using
#' `list_experiments()` and `list_experiment_ids()`.
#' 
#' By calling `list_experiments()` on project objects, all corresponding
#' experiments are listed. It is possible to limit the search to experiments
#' that are of a certain type and exclude experiments that either have no
#' datasets or samples associated. An exhaustive list of realized experiment
#' types can be retrieved using `list_experiment_types()`. If several
#' experiment types are requested in `list_experiments()`, the default is to
#' iterate over all available types, an API call per experiment type has to be
#' made.
#' 
#' `ExperimentIdentifier` objects present a more compact way of uniquely
#' representing experiments. All experiments that are available to the current
#' user can be listed with `list_experiment_ids()`. There is no way of limiting
#' the search for experiments.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned experiments, e.g. a set of
#' `ExperimentIdentifier` or `Project` objects.
#' @param ... Generic compatibility. Extra arguments will be passed to
#' [make_requests()].
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis("rdgr2014", "IXPubReview")
#' 
#'   # list all available projects to limit the search for experiments
#'   proj <- list_projects(tok)
#' 
#'   # list all experiments corresponding to the project with index 1
#'   exps <- list_experiments(tok, proj[[1L]])
#' 
#'   # convert experiment to experiment ids and back
#'   exp_ids <- as_experiment_id(exps)
#'   identical(exps, list_experiments(tok, exp_ids))
#' 
#'   # experiments can also be searched for
#'   exp <- search_openbis(tok,
#'                         search_criteria(
#'                           attribute_clause("code",
#'                                            get_field(exps[[1L]],
#'                                                      "identifier"))
#'                         ),
#'                         target_object = "experiment")
#'   identical(exps[[1L]], exp)
#' 
#'   logout_openbis(tok)
#' }
#' 
#' @export
#' 
list_experiments <- function(token, x, ...)
  UseMethod("list_experiments", x)

#' @rdname list_experiments
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "listExperiments")}
#' @export
#' 
list_experiments.ExperimentIdentifier <- function(token, x, ...)
  make_request(api_url("gis", attr(token, "host_url"), ...),
               "listExperiments",
               list(token, exp_id_str(x)),
               ...)

#' @rdname list_experiments
#' 
#' @param types Either a single or set of `ExperimentType` objects
#' @param require A switch to require the resulting experiments to contain a
#' nonzero number of dataset or sample. Default behavior is no requirement.
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "listExperimentsHavingDataSets")}
#' * \Sexpr{infx::docs_link("gis", "listExperimentsHavingSamples")}
#' 
#' @export
#' 
list_experiments.Project <- function(token,
                                     x,
                                     types = list_experiment_types(token),
                                     require = c(NA, "DataSets", "Samples"),
                                     ...) {

  require <- match.arg(require)
  types <- get_field(types, "code")
  x <- as_json_vec(remove_null(x))

  if (is.na(require))
    fun <- "listExperiments"
  else if (require == "DataSets")
    fun <- "listExperimentsHavingDataSets"
  else
    fun <- "listExperimentsHavingSamples"

  params <- lapply(types, function(type) list(token, x, as.character(type)))

  res <- make_requests(api_url("gis", attr(token, "host_url"), ...),
                       fun,
                       params,
                       ...)

  as_json_vec(res, simplify = TRUE)
}

#' @rdname list_experiments
#' @section openBIS:
#' * \Sexpr{infx::docs_link("sas", "listExperiments")}
#' @export
#' 
list_experiment_ids <- function(token, ...)
  make_request(api_url("sas", attr(token, "host_url"), ...),
               "listExperiments",
               list(token),
               ...)

#' @rdname list_experiments
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "listExperimentTypes")}
#' @export
#' 
list_experiment_types <- function(token, ...)
  make_request(api_url("gis", attr(token, "host_url"), ...),
               "listExperimentTypes",
               list(token),
               ...)

#' @rdname list_experiments
#' @export
#' 
as_experiment_id <- function(x, ...)
  UseMethod("as_experiment_id")

#' @rdname list_experiments
#' @export
#' 
as_experiment_id.Experiment <- function(x, ...) {

  codes <- strsplit(sub("^/", "", get_field(x, "identifier")), "/")

  as_json_vec(
    Map(json_class,
        permId = get_field(x, "permId"),
        spaceCode = vapply(codes, `[`, 1L, character(1L)),
        projectCode = vapply(codes, `[`, 2L, character(1L)),
        experimentCode = vapply(codes, `[`, 3L, character(1L)),
        MoreArgs = list(class = "ExperimentIdentifier"))
  )
}

#' @rdname list_experiments
#' @export
#' 
as_experiment_id.ExperimentIdentifier <- function(x, ...)
  as_json_vec(x)

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
#' 
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
exp_id_str.Experiment <- function(x, ...)
  get_field(x, "identifier")
