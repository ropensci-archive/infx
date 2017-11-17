
#' @title Create a plate id object
#'
#' @description PlateIdentifier objects are used throughout the openBis API
#' to identify plates. In addition to the plate barcode, the space the plate
#' resides in is needed. If unknown (NULL), this property can be determined by
#' listing all Plates (using [list_plates]) and matching the provided barcode.
#' If several plates in different spaces have the same barcode, an error is
#' thrown.
#' 
#' @inheritParams logout_openbis
#' @param plate_id Plate barcode.
#' @param space_code The space code of the plate; it NULL, it is determined
#' automatically.
#' 
#' @return A single PlateIdentifier object as a list of type json_class.
#' 
#' @export
#' 
create_plate_id <- function(plate_id,
                            space_code = NULL,
                            token = NULL) {

  assert_that(is.character(plate_id), length(plate_id) == 1L)

  if (is.null(space_code)) {
    assert_that(!is.null(token))
    plates <- list_plates(token)
    plate_match <- sapply(plates, `[[`, "plateCode") == plate_id
    assert_that(sum(plate_match) == 1L)

    space_code <- plates[[which(plate_match)]][["spaceCodeOrNull"]]
  }

  assert_that(is.character(space_code), length(space_code) == 1L)

  structure(list(plateCode = plate_id,
                 spaceCodeOrNull = space_code),
            class = "json_class", json_class = "PlateIdentifier")
}