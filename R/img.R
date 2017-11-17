
#' @title List image datasets
#'
#' @description For a plate, which can be specified as a (vector of)
#' string(s), for which the PlateIdentifier objects then are constructed using
#' [create_plate_id], or for a (list of) PlateIdentifier object(s), list image
#' datasets.
#' 
#' @inheritParams logout_openbis
#' @param plate_id Either a character vector holding plate barcodes or a list
#' of/single PlateIdentifier object(s).
#' 
#' @return A list/single object of type ImageDatasetReference.
#' 
#' @export
#' 
list_img_datasets <- function(token,
                              plate_id,
                              ...) {

  if (all(sapply(plate_id, is.character)))
    plate_id <- lapply(plate_id, create_plate_id, token = token)
  else if (has_json_class(plate_id, "PlateIdentifier"))
    plate_id <- list(plate_id)

  assert_that(is.list(plate_id),
              all(sapply(plate_id, has_json_class, "PlateIdentifier")))

  query_openbis("listImageDatasets", list(token, plate_id),
                "IScreeningApiServer", ...)
}