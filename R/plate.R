
#' List plates
#'
#' Given a login token, all plates available to the corresponding user on the
#' queried openBIS instance are listed for one or more experiment(s). If
#' multiple experiments are used for limiting the search (e.g. a `json_vec` of
#' experiments), an API call for each object has to be made. If no experiments
#' are specified, all available plates are returned.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned plates, e.g. a set of
#' `ExperimentIdentifier` or `Experiment` objects. The default is NULL, which
#' will fetch all available plates.
#' @param ... Generic compatibility
#' 
#' @export
#' 
list_plates <- function(token, x = NULL, ...)
  UseMethod("list_plates", x)

#' @rdname list_plates
#' @export
#' 
list_plates.NULL <- function(token, x, ...)
  request_openbis("listPlates", token, "IScreeningApiServer")

#' @rdname list_plates
#' @export
#' 
list_plates.ExperimentIdentifier <- function(token, x, ...) {

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  res <- lapply(x, function(exp)
    request_openbis("listPlates", list(token, exp), "IScreeningApiServer"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_plates
#' @export
#' 
list_plates.Experiment <- function(token, x, ...) {
  list_plates(token, exp_to_expid(x))
}

plate_to_plateid <- function(x) {

  convert <- function(x)
    json_class(plateCode = x[["plateCode"]],
               spaceCodeOrNull = x[["spaceCodeOrNull"]],
               class = "PlateIdentifier")

  fields <- c("plateCode", "spaceCodeOrNull")

  assert_that(inherits(x, "Plate"),
              has_fields(x, fields))

  if (is_json_class(x))
    convert(x)
  else
    as_json_vec(lapply(x, convert))
}

#' List wells
#'
#' Given a login token, all wells available to the corresponding user on the
#' queried openBIS instance are listed for one or more plate(s). If multiple
#' plates are searched for (e.g. a `json_vec` of plates), an API call for each
#' object has to be made.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned wells, e.g. a set of
#' `PlateIdentifier` or `Plate` objects.
#' @param ... Generic compatibility
#' 
#' @export
#' 
list_wells <- function(token, x = NULL, ...)
  UseMethod("list_wells", x)

#' @rdname list_wells
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

#' @rdname list_wells
#' @export
#' 
list_wells.Plate <- function(token, x, ...)
  list_wells(token, plate_to_plateid(x))

#' List plate/well references
#'
#' Given a login token, all wells available to the corresponding user on the
#' queried openBIS instance, which are associated with any of the given
#' material(s), are listed. If multiple material ids are searched for (e.g. a
#' `json_vec` of `MaterialIdentifierScreening`), an API call for each
#' object has to be made. The search can be limited to a single experiment,
#' specified either as `Experiment` or `ExperimentIdentifier`
#' 
#' @inheritParams logout_openbis
#' @param material_id Material id(s) corresponding to which wells are searched
#' for a collection of `MaterialIdentifierScreening` is expected.
#' @param experiment_id Additionally, the search can be limited to a single
#' experiment, specified either as `Experiment` or `ExperimentIdentifier`.
#' @param include_datasets Logical switch indicating whether to also return
#' the connected image and image analysis data sets.
#' 
#' @export
#' 
list_plate_well_ref <- function(token,
                                material_id,
                                experiment_id = NULL,
                                include_datasets = FALSE) {

  if (!is_json_vec(material_id))
    material_id <- as_json_vec(material_id)

  assert_that(all(sapply(material_id, has_json_subclass,
                         "MaterialIdentifierScreening")))

  if (is.null(experiment_id)) {

    res <- lapply(material_id, function(mat)
      request_openbis("listPlateWells", list(token, mat, include_datasets),
                      "IScreeningApiServer"))
  } else {

    if (is_json_vec(experiment_id))
      experiment_id <- as_json_class(experiment_id)

    if (has_json_subclass(experiment_id, "Experiment"))
      experiment_id <- exp_to_expid(experiment_id)

    assert_that(has_json_subclass(experiment_id, "ExperimentIdentifier"))

    res <- lapply(material_id, function(mat)
      request_openbis("listPlateWells", list(token, experiment_id, mat,
                                             include_datasets),
                      "IScreeningApiServer"))
  }

  as_json_vec(do.call(c, res))
}
