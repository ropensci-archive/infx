
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
#' plate id objects is available as `plate_to_plateid()`. This function does
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
  request_openbis("listPlates", token, "IScreeningApiServer")

#' @rdname list_plate_well
#' @export
#' 
list_plates.ExperimentIdentifier <- function(token, x, ...) {

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  res <- lapply(x, function(exp)
    request_openbis("listPlates", list(token, exp), "IScreeningApiServer"))

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
plate_to_plateid <- function(x) {

  convert <- function(x)
    json_class(plateCode = x[["plateCode"]],
               spaceCodeOrNull = x[["spaceCodeOrNull"]],
               class = "PlateIdentifier")

  fields <- c("plateCode", "spaceCodeOrNull")

  assert_that(inherits(x, "Plate"),
              has_fields(x, fields))

  if (is_json_class(x))
    res <- convert(x)
  else
    res <- lapply(x, convert)

  as_json_vec(res)
}

#' @rdname list_plate_well
#' @export
#' 
list_wells <- function(token, x = NULL, ...)
  UseMethod("list_wells", x)

#' @rdname list_plate_well
#' @export
#' 
list_wells.PlateIdentifier <- function(token, x, ...) {

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  res <- lapply(x, function(plate)
    request_openbis("listPlateWells", list(token, plate),
                    "IScreeningApiServer"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_plate_well
#' @export
#' 
list_wells.Plate <- function(token, x, ...)
  list_wells(token, plate_to_plateid(x))

#' @rdname list_plate_well
#' @export
#' 
list_plate_well_ref <- function(token,
                                material_id,
                                experiment = NULL,
                                include_datasets = FALSE) {

  if (!is_json_vec(material_id))
    material_id <- as_json_vec(material_id)

  assert_that(all(sapply(material_id, has_subclass,
                         "MaterialIdentifierScreening")))

  if (is.null(experiment)) {

    res <- lapply(material_id, function(mat)
      request_openbis("listPlateWells", list(token, mat, include_datasets),
                      "IScreeningApiServer"))
  } else {

    if (get_common_subclass(experiment) == "Experiment")
      experiment <- exp_to_expid(experiment)

    if (is_json_vec(experiment))
      experiment <- as_json_class(experiment)

    assert_that(has_subclass(experiment, "ExperimentIdentifier"))

    res <- lapply(material_id, function(mat)
      request_openbis("listPlateWells", list(token, experiment, mat,
                                             include_datasets),
                      "IScreeningApiServer"))
  }

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
  request_openbis("getPlateMetadataList", list(token, as_json_vec(x)),
                  "IScreeningApiServer")

#' @rdname list_plate_well
#' @export
#' 
list_plate_metadata.Plate <- function(token, x, ...)
  request_openbis("getPlateMetadataList", list(token, plate_to_plateid(x)),
                  "IScreeningApiServer")
