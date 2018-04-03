
#' List wells/plates
#'
#' Using `list_plates()`, all plates available to the user (corresponding to
#' the given login token) on the queried openBIS instance are listed for one
#' or more experiment(s). If multiple experiments are used for limiting the
#' search (e.g. a `json_vec` of experiments), an API call for each object has
#' to be made. If no experiments are specified, all available plates are
#' returned.
#' 
#' Wells can be listed with `list_wells()`, which given a login token, lists
#' all wells available to the corresponding user on the queried openBIS
#' instance for one or more plate(s). If multiple plates are searched for
#' (e.g. a `json_vec` of plates), an API call for each object has to be made.
#' 
#' A convenience function that allows for the conversion of plate objects into
#' plate id objects is available as `as_plateid()`. This function does
#' not incur an API call and can act on both a single plate object or a vector
#' of plate objects (passed as a `json_vec` object) and will return a
#' `json_vec` object of type `PlateIdentifier`.
#' 
#' `list_plate_well_ref()` can list well references (as
#' `PlateWellReferenceWithDatasets` objects) corresponding to materials. This
#' can be used to find all wells that contain a certain material (for example
#' a gene knockdown or a specific compound used for a knockdown). If multiple
#' material ids are searched for (e.g. a `json_vec` of
#' `MaterialIdentifierScreening`), an API call for each object has to be made.
#' The search can be limited to a single experiment,  specified either as
#' `Experiment` or `ExperimentIdentifier`. As a further argument, the switch
#' `include_datasets` specifies whether the connected image and image analysis
#' data sets should be returned as well.
#' 
#' All metadata corresponding to a plate (specified as a set of `Plate` or
#' `PlateIdentifier` objects) can be fetched using `list_plate_metadata()`.
#' The returned `PlateMetadata` objects also contain a list of corresponding
#' `WellMetadata` objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned wells or plates.
#' @param ... Generic compatibility
#' @param material_id Material id(s) corresponding to which wells are searched
#' for a collection of `MaterialIdentifierScreening` is expected.
#' @param experiment Additionally, the search can be limited to a single
#' experiment, specified either as `Experiment` or `ExperimentIdentifier`.
#' @param include_datasets Logical switch indicating whether to also return
#' the connected image and image analysis data sets.
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("sas", "listPlates")}
#' * \Sexpr{infx::docs_link("sas", "listPlateWells")}
#' * \Sexpr{infx::docs_link("sas", "getPlateMetadataList")}
#' 
#' @rdname list_plate_well
#' 
#' @export
#' 
list_plates <- function(token, x = NULL, ...)
  UseMethod("list_plates", x)

#' @rdname list_plate_well
#' @export
#' 
list_plates.NULL <- function(token, x, ...)
  make_request(api_url("sas"), "listPlates", list(token))

#' @rdname list_plate_well
#' @export
#' 
list_plates.ExperimentIdentifier <- function(token, x, ...) {

  params <- lapply(as_json_vec(x), function(exp) list(token, exp))

  res <- make_requests(api_url("sas"), "listPlates", params)

  as_json_vec(do.call(c, res))
}

#' @rdname list_plate_well
#' @export
#' 
list_plates.Experiment <- function(token, x, ...) {
  list_plates(token, exp_to_expid(x))
}

#' @rdname list_plate_well
#' @export
#' 
as_plateid <- function(x, ...)
  UseMethod("as_plateid", x)

#' @rdname list_plate_well
#' @export
#' 
as_plateid.Plate <- function(x, ...) {

  convert <- function(x)
    json_class(plateCode = x[["plateCode"]],
               spaceCodeOrNull = x[["spaceCodeOrNull"]],
               class = "PlateIdentifier")

  fields <- c("plateCode", "spaceCodeOrNull")

  assert_that(has_fields(x, fields))

  if (is_json_class(x))
    res <- convert(x)
  else
    res <- lapply(x, convert)

  as_json_vec(res)
}

#' @rdname list_plate_well
#' @export
#' 
as_plateid.Sample <- function(x, ...) {

  convert <- function(x)
    json_class(plateCode = x[["code"]],
               spaceCodeOrNull = x[["spaceCode"]],
               class = "PlateIdentifier")

  fields <- c("code", "spaceCode")

  x <- as_json_vec(x)

  assert_that(all(sapply(x, `[[`, "sampleTypeCode") == "PLATE"),
              has_fields(x, fields))

  as_json_vec(lapply(x, convert))
}

#' @rdname list_plate_well
#' @export
#' 
list_wells <- function(token, x, ...)
  UseMethod("list_wells", x)

#' @rdname list_plate_well
#' @export
#' 
list_wells.PlateIdentifier <- function(token, x, ...) {

  params <- lapply(as_json_vec(x), function(plate) list(token, plate))

  res <- make_requests(api_url("sas"), "listPlateWells", params)

  as_json_vec(do.call(c, res))
}

#' @rdname list_plate_well
#' @export
#' 
list_wells.Plate <- function(token, x, ...)
  list_wells(token, as_plateid(x))

#' @rdname list_plate_well
#' @export
#' 
list_plate_well_ref <- function(token,
                                material_id,
                                experiment = NULL,
                                include_datasets = FALSE) {

  material_id <- as_json_vec(material_id)

  assert_that(all(sapply(material_id, has_subclass,
                         "MaterialIdentifierScreening")))

  if (is.null(experiment)) {

    params <- lapply(material_id,
                     function(mat) list(token, mat, include_datasets))
  } else {

    if (get_subclass(experiment) == "Experiment")
      experiment <- exp_to_expid(experiment)

    if (is_json_vec(experiment))
      experiment <- as_json_class(experiment)

    assert_that(has_subclass(experiment, "ExperimentIdentifier"))

    params <- lapply(material_id,
                     function(mat) list(token, experiment, mat,
                                        include_datasets))
  }

  res <- make_requests(api_url("sas"), "listPlateWells", params)

  as_json_vec(do.call(c, res))
}

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata <- function(token, x, ...)
  UseMethod("list_plate_metadata", x)

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata.PlateIdentifier <- function(token, x, ...)
  make_request(api_url("sas"), "getPlateMetadataList",
               list(token, as_json_vec(x)))

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata.Plate <- function(token, x, ...)
  make_request(api_url("sas"), "getPlateMetadataList",
               list(token, as_plateid(x)))

#' @param row Character vector plate row names or numeric vector of plate row
#' indices.
#' @param col Numeric vector of plate row indices.
#' 
#' @rdname list_plate_well
#' @export
#' 
well_pos <- function(row, col) {

  if (is.character(row)) {
    row <- toupper(row)
    row <- sapply(row, match, LETTERS)
  }

  assert_that(isTRUE(all.equal(row, suppressWarnings(as.integer(row)),
                               check.attributes = FALSE)),
              isTRUE(all.equal(col, suppressWarnings(as.integer(col)),
                               check.attributes = FALSE)))

  max_len <- max(length(row), length(col))

  if (max_len > 1L) {
    if (length(row) == 1L)
      row <- rep(row, max_len)
    if (length(col) == 1L)
      col <- rep(col, max_len)
  }

  assert_that(length(row) == length(col))

  new_json_vec(
    Map(json_class, wellRow = row, wellColumn = col,
        MoreArgs = list(class = "WellPosition"))
  )
}
