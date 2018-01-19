
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
