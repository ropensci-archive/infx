
#' List plates and wells
#' 
#' Plates and wells are special cases of `Sample` objects and play an
#' important organizational role when openBIS is used in HTS experiments. All
#' InfectX screens were arrayed onto 384-well plates, arranged into 16 rows
#' (A through P) and 24 columns (1 through 24). `Plate` and `PlateIdentifier`
#' objects are used to identify plates while for wells only `WellIdentifier`
#' objects exists for representing individual wells. Additional objects
#' relevant in this context are `PlateMetadata`, which for all associated wells
#' contain respective `WellMetadata` objects and
#' `PlateWellReferenceWithDatasets` objects, each holding a `Plate` object and
#' a `WellPosition` object, thereby in a sense also identifying individual
#' wells.
#' 
#' `Plate` objects are listed using `list_plates()`, which can either list all
#' available plates (default or dispatch on `NULL`) or restrict the listing to
#' a set of supplied experiment objects (dispatch on either `Experiment` or
#' `ExperimentIdentifier` objects). If multiple experiments are used for
#' limiting the search (i.e. a `json_vec` of experiments), a separate API call
#' for each object has to be made. `PlateMetadata` objects (including all
#' corresponding `WellMetadata` objects) are retrieved by
#' `list_plate_metadata()` which can be dispatched on objects that represent
#' plates, including `Plate`, `PlateIdentifier` and `Sample` (given that the
#' sample is of type `PLATE`). Finally, `PlateIdentifier` can be created
#' either by calling `plate_id()` or though coercion of `Plate`, `Sample`
#' (with type `PLATE`) or `PlateMetadata`  objects using the function
#' `as_plate_id()`. Neither `plate_id()` nor `as_plate_id()` incur API calls.
#' 
#' Wells can be listed with `list_wells()`, which returns `WellIdentifier`
#' objects if dispatch occurs on objects representing plates, including
#' `Plate`, `PlateIdentifier` and `Sample` (with type `PLATE`). In this case
#' the entire set of well id objects corresponding to the selected plates is
#' returned and a separate API call is required per plate.
#' 
#' Whenever `list_wells()` is dispatched on material objects (any of
#' `MaterialScreening`, `MaterialIdentifierScreening`, `MaterialGeneric` or
#' `MaterialIdentifierGeneric`), `PlateWellReferenceWithDatasets` objects are
#' returned, representing wells associated with the given material. If multiple
#' material ids are passed, an API call for each object is issued. The well
#' search can be limited to an experiment by passing a single `Experiment` or
#' `ExperimentIdentifier` object as `experiment` argument and image dataset
#' references as well as feature vector dataset references can be retrieved
#' as part of the `PlateWellReferenceWithDatasets` objects if the logical
#' switch `include_datasets` is set to `TRUE`. A separate API call per passed
#' material object is required.
#' 
#' Instantiation of `WellIdentifier` objects can be done either using the
#' constructor `well_id()` or via coercion of `WellMetadata` objects by
#' calling `as_well_id()`. Well samples cannot be coerced to well id objects
#' as they do not contain all fields that are required. A further object type
#' relevant to this context is that of `WellPosition`, encoding the position
#' of a well within a plate. Such objects can be created using the constructor
#' `well_pos()`.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned wells or plates.
#' @param ... Generic compatibility
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("sas", "listPlates")}
#' * \Sexpr{infx::docs_link("sas", "listPlateWells")}
#' * \Sexpr{infx::docs_link("sas", "getPlateMetadataList")}
#' 
#' @rdname list_plate_well
#' 
#' @examples
#' \dontrun{
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
#'   # list all plates associated with this experiment
#'   plates <- list_plates(tok, exp)
#'   length(plates)
#'   as_plate_id(plates)
#' 
#'   # for a plate, fetch meta data objects
#'   meta <- list_plate_metadata(tok, plates[[1L]])
#'   print(meta[[1L]], depth = 2L, length = 15L)
#'   print(meta[[1L]][["wells"]][[1L]], depth = 2L)
#' 
#'   # search for a sample object corresponding to plate BB01-1I
#'   samp <- search_openbis(tok,
#'                          search_criteria(
#'                            attribute_clause("code",
#'                                             "/INFECTX_PUBLISHED/BB01-1I")
#'                          ),
#'                          target_object = "sample")
#' 
#'   # list all wells for this sample
#'   wells <- list_wells(tok, samp)
#'   identical(as_well_id(meta[[1L]][["wells"]][[1L]]),
#'             wells[1L])
#' 
#'   # search for the material corresponding to MTOR
#'   mat <- search_openbis(tok,
#'                         search_criteria(
#'                           property_clause("gene_symbol", "MTOR")
#'                         ),
#'                         target_object = "material")
#'   # search for associated wells, limited to ADENO-AU-K1
#'   mtor <- list_wells(tok, mat, exp)
#'   plates <- get_field(mtor, "experimentPlateIdentifier")
#'   as_plate_id(plates)
#'   unique(get_field(mtor, "wellPosition"))
#' }
#' 
#' @export
#' 
list_plates <- function(token, x = NULL, ...)
  UseMethod("list_plates", x)

#' @rdname list_plate_well
#' @export
#' 
list_plates.NULL <- function(token, x, ...)
  make_request("listPlates", list(token), api_endpoint = "sas", ...)

#' @rdname list_plate_well
#' @export
#' 
list_plates.ExperimentIdentifier <- function(token, x, ...) {

  params <- lapply(as_json_vec(x), function(exp) list(token, exp))

  res <- make_requests("listPlates", params, api_endpoint = "sas", ...)

  as_json_vec(do.call(c, res))
}

#' @rdname list_plate_well
#' @export
#' 
list_plates.Experiment <- function(token, x, ...) {
  list_plates(token, exp_to_expid(x), ...)
}

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata <- function(token, x, ...)
  UseMethod("list_plate_metadata", x)

list_plate_meta <- function(token, x, ...)
  make_request("getPlateMetadataList", list(token, as_plate_id(x)),
               api_endpoint = "sas", ...)

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata.PlateIdentifier <- list_plate_meta

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata.Plate <- list_plate_meta

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata.Sample <- list_plate_meta

#' @param code,space Character vectors that together can be used to create
#' `PlateIdentifier` objects.
#' 
#' @rdname list_plate_well
#' @export
#' 
plate_id <- function(code, space) {

  max_len <- max(length(code), length(space))

  if (max_len > 1L) {
    if (length(code) == 1L)
      code <- rep(code, max_len)
    if (length(space) == 1L)
      space <- rep(space, max_len)
  }

  assert_that(is.character(code), is.character(space),
              length(code) == length(space))

   as_json_vec(
    Map(json_class,
        plateCode = code,
        spaceCodeOrNull = space,
        MoreArgs = list(class = "PlateIdentifier"))
  )
}

#' @rdname list_plate_well
#' @export
#' 
as_plate_id <- function(x, ...)
  UseMethod("as_plate_id", x)

#' @rdname list_plate_well
#' @export
#' 
as_plate_id.Plate <- function(x, ...)
  plate_id(get_field(x, "plateCode"), get_field(x, "spaceCodeOrNull"))

#' @rdname list_plate_well
#' @export
#' 
as_plate_id.Sample <- function(x, ...) {

  assert_that(all(get_field(x, "sampleTypeCode") == "PLATE"))

  plate_id(get_field(x, "code"), get_field(x, "spaceCode"))
}

#' @rdname list_plate_well
#' @export
#' 
as_plate_id.PlateMetadata <- function(x, ...)
  plate_id(get_field(x, "plateCode"), get_field(x, "spaceCodeOrNull"))

#' @rdname list_plate_well
#' @export
#' 
as_plate_id.PlateIdentifier <- function(x, ...)
  plate_id(get_field(x, "plateCode"), get_field(x, "spaceCodeOrNull"))

#' @rdname list_plate_well
#' @export
#' 
list_wells <- function(token, x, ...)
  UseMethod("list_wells", x)

list_wells_for_plate <- function(token, x, ...) {

  params <- lapply(as_plate_id(x), function(plate) list(token, plate))

  res <- make_requests("listPlateWells", params, api_endpoint = "sas", ...)

  remove_null(as_json_vec(do.call(c, res)))
}

#' @rdname list_plate_well
#' @export
#' 
list_wells.PlateIdentifier <- list_wells_for_plate

#' @rdname list_plate_well
#' @export
#' 
list_wells.Plate <- list_wells_for_plate

#' @rdname list_plate_well
#' @export
#' 
list_wells.Sample <- list_wells_for_plate

list_wells_for_mat <- function(token,
                               x,
                               experiment = NULL,
                               include_datasets = FALSE,
                               ...) {

  x <- as_screening_mat_id(x)

  if (is.null(experiment)) {

    params <- lapply(x, function(y) list(token, y, include_datasets))

  } else {

    if (get_subclass(experiment) == "Experiment")
      experiment <- exp_to_expid(experiment)

    if (is_json_vec(experiment))
      experiment <- as_json_class(experiment)

    assert_that(has_subclass(experiment, "ExperimentIdentifier"))

    params <- lapply(x, function(y) list(token, experiment, y,
                                         include_datasets))
  }

  res <- make_requests("listPlateWells", params, api_endpoint = "sas", ...)

  as_json_vec(do.call(c, res))
}

#' @param experiment Additionally, the search can be limited to a single
#' experiment, specified either as `Experiment` or `ExperimentIdentifier`.
#' @param include_datasets Logical switch indicating whether to also return
#' the connected image and image analysis data sets.
#' 
#' @rdname list_plate_well
#' @export
#' 
list_wells.MaterialScreening <- list_wells_for_mat

#' @rdname list_plate_well
#' @export
#' 
list_wells.MaterialIdentifierScreening <- list_wells_for_mat

#' @rdname list_plate_well
#' @export
#' 
list_wells.MaterialGeneric <- list_wells_for_mat

#' @rdname list_plate_well
#' @export
#' 
list_wells.MaterialIdentifierGeneric <- list_wells_for_mat

#' @param perm_id,plate,well_pos Character vector, set of plate objects and
#' set of well position objects, all of the same length or length 1, that
#' together can be used to create `WellIdentifier` objects.
#' @param well_code Character vector where each entry is of the form
#' barcode:well_name, e.g. FOO-BAR-1:A1, FOO-BAR-1:A2, etc.
#' 
#' @rdname list_plate_well
#' @export
#' 
well_id <- function(perm_id,
                    plate,
                    well_pos = NULL,
                    well_code = NULL,
                    ...) {

  if (is.null(well_pos)) {
    if (is.null(well_code))
      well_pos <- well_pos(...)
    else
      well_pos <- well_pos(name = sapply(strsplit(well_code, ":"), `[`, 2L))
  } else
    assert_that(is.null(well_code),
                length(list(...)) == 0L)

  plate <- as_plate_id(plate)
  well_pos <- as_json_vec(well_pos)

  max_len <- max(length(perm_id), length(plate), length(well_pos))

  if (max_len > 1L) {
    if (length(perm_id) == 1L)
      perm_id <- rep(perm_id, max_len)
    if (length(plate) == 1L)
      plate <- rep(plate, max_len)
    if (length(well_pos) == 1L)
      well_pos <- rep(well_pos, max_len)
  }

  assert_that(is.character(perm_id),
              length(perm_id) == max_len,
              has_subclass(plate, "PlateIdentifier"),
              length(plate) == max_len,
              has_subclass(well_pos, "WellPosition"),
              length(well_pos) == max_len)

   as_json_vec(
    Map(json_class,
        permId = perm_id,
        plateIdentifier = plate,
        wellPosition = well_pos,
        MoreArgs = list(class = "WellIdentifier"))
  )
}

#' @rdname list_plate_well
#' @export
#' 
as_well_id <- function(x, ...)
  UseMethod("as_well_id", x)

#' @rdname list_plate_well
#' @export
#' 
as_well_id.WellMetadata <- function(x, ...) {

  x <- as_json_vec(x)

  well_id(get_field(x, "permId"),
          get_field(x, "plateIdentifier"),
          get_field(x, "wellPosition"))
}

#' @rdname list_plate_well
#' @export
#' 
as_well_id.WellIdentifier <- function(x, ...)
  as_json_vec(x)

#' @param row,col Character vector of plate row names or numeric vector of
#' plate row indices and numeric vector of plate column indices, both of the
#' same length or of length 1.
#' @param name Character vector of well name, where each entry is of the form
#' A1, A2, etc. 
#' 
#' @rdname list_plate_well
#' @export
#' 
well_pos <- function(row = NULL, col = NULL, name = NULL) {

  if (is.null(row) || is.null(col)) {
    assert_that(is.null(row), is.null(col), is.character(name))
    row <- substr(name, 1L, 1L)
    col <- substring(name, 2L)
  } else
    assert_that(is.null(name))

  if (is.character(row))
    row <- match(toupper(row), LETTERS)

  assert_that(is.numeric(row), is.numeric(col),
              identical(row, as.integer(row)),
              identical(col, as.integer(col)))

  max_len <- max(length(row), length(col))

  if (max_len > 1L) {
    if (length(row) == 1L)
      row <- rep(row, max_len)
    if (length(col) == 1L)
      col <- rep(col, max_len)
  }

  assert_that(length(row) == length(col))

  as_json_vec(
    Map(json_class,
        wellRow = row,
        wellColumn = col,
        MoreArgs = list(class = "WellPosition"))
  )
}
