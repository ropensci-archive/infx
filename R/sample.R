
#' List samples and sample types
#' 
#' In openBIS, samples can be seen as a generalization of plates and wells (
#' see [list_plates()] and [list_wells()]. In fact, for the HTS focused openBIS
#' instance of InfectX, wells and plates represent the only types of samples
#' available. Samples can either be retrieved using [search_openbis()] and
#' setting the argument `target_object` to `sample` or listed by calling
#' `list_samples()`. Furthermore, all available sample types can be listed
#' using `list_sample_types()`.
#' 
#' `list_samples()` can be dispatched on objects identifying experiments
#' (`Experiment` and `ExperimentIdentifier`), in which case the associated
#' plate samples are returned, on objects representing plates (`Plate`,
#' `PlateIdentifier` and `PlateMetadata`) or on objects representing wells
#' (`WellIdentifier` and `WellMetadata`). For plates, the corresponding plate
#' samples and for and wells, the corresponding well samples are returned. It
#' is therefore not possible to list all well samples for a plate. This could
#' however be achieved by listing all wells of a plate using [list_wells()]
#' and calling `list_samples()` on the returned set of `WellIdentifier`
#' objects. A separate API call is required for each object is a `json_vec`
#' of objects is passed to `list_samples()`.
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
#' @examples
#' \donttest{
#'   tok <- login_openbis("rdgr2014", "IXPubReview")
#' 
#'   # search for an experiment, e.g. ADENO-AU-K1
#'   exp <- search_openbis(tok,
#'                         search_criteria(
#'                           property_clause("pathogen", "Adenovirus"),
#'                           property_clause("library", "Ambion"),
#'                           property_clause("geneset", "Kinome"),
#'                           property_clause("replicate", 1L)
#'                         ),
#'                         target_object = "experiment")
#' 
#'   # list all plate samples associated with this experiment
#'   plate_samp <- list_samples(tok, exp)
#'   length(plate_samp)
#' 
#'   # the same plates are returned using list_plates(), not regarding order
#'   plates <- list_plates(tok, exp)
#'   plate_samp <- plate_samp[order(get_field(plate_samp, "code"))]
#' 
#'   identical(as_plate_id(plates), as_plate_id(plate_samp))
#' 
#'   # plates can be converted to samples and back to plates again
#'   identical(as_plate_id(plates), as_plate_id(list_samples(tok, plates)))
#' 
#'   # the same is not possible for wells: first well ids are listed for a
#'   # plate
#'   well_ids <- list_wells(tok, plates[[1L]])
#'   # for efficiency only the first 5 wells are retained
#'   well_ids <- well_ids[1:5]
#'   print(well_ids, length = 10L)
#'   # the corresponding well samples are fetched
#'   well_samp <- list_samples(tok, well_ids)
#'   # from well sample objects it is not possible to directly create well id
#'   # objects as no plate id information is available
#'   print(well_samp, length = 20L)
#'   # with a bit of manual work however it is possible to create well id
#'   # objects from well samples
#'   wells <- well_id(get_field(well_samp, "permId"), plates[[1L]],
#'                    well_code = get_field(well_samp, "code"))
#'   identical(wells, well_ids)
#' 
#'   logout_openbis(tok)
#' }
#' 
#' @export
#' 
list_samples <- function(token, x, ...)
  UseMethod("list_samples", x)

list_samples_for_exp <- function(token, x, ...) {

  params <- lapply(exp_id_str(x), function(exp) list(token, exp))

  res <- make_requests(api_url("gis", attr(token, "host_url"), ...),
                       "listSamplesForExperiment",
                       params,
                       ...)

  as_json_vec(
    Map(set_attr,
        unlist(res, recursive = FALSE),
        rep(as_experiment_id(x), lengths(res)),
        MoreArgs = list(attr_name = "exp_id"))
  )
}

#' @rdname list_samples
#' @export
#' 
list_samples.ExperimentIdentifier <- list_samples_for_exp

#' @rdname list_samples
#' @export
#' 
list_samples.Experiment <- list_samples_for_exp

list_samples_for_plate <- function(token, x, ...) {

  x <- as_plate_id(x)

  params <- lapply(x, function(plate) list(token, plate))

  res <- make_requests(api_url("sas", attr(token, "host_url"), ...),
                       "getPlateSample",
                       params,
                       ...)

  as_json_vec(
    Map(set_attr,
        unlist(res, recursive = FALSE),
        rep(x, lengths(res)),
        MoreArgs = list(attr_name = "plate_id"))
  )
}

#' @rdname list_samples
#' @export
#' 
list_samples.Plate <- list_samples_for_plate

#' @rdname list_samples
#' @export
#' 
list_samples.PlateIdentifier <- list_samples_for_plate

#' @rdname list_samples
#' @export
#' 
list_samples.PlateMetadata <- list_samples_for_plate

list_samples_for_well <- function(token, x, ...) {

  x <- as_well_id(x)

  params <- lapply(x, function(well) list(token, remove_null(well)))

  res <- make_requests(api_url("sas", attr(token, "host_url"), ...),
                       "getWellSample",
                       params,
                       ...)

  as_json_vec(
    Map(set_attr,
        unlist(res, recursive = FALSE),
        rep(x, lengths(res)),
        MoreArgs = list(attr_name = "well_id"))
  )
}

#' @rdname list_samples
#' @export
#' 
list_samples.WellIdentifier <- list_samples_for_well

#' @rdname list_samples
#' @export
#' 
list_samples.WellMetadata <- list_samples_for_well

#' @rdname list_samples
#' @export
#' 
list_sample_types <- function(token, ...)
  make_request(api_url("gis", attr(token, "host_url"), ...),
               "listSampleTypes",
               list(token),
               ...)
