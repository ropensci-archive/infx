
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
#' @param type A switch to choose from raw or segmentation image datasets.
#' @param most_recent Logical switch, if TRUE only the most recent dataset is
#' returned.
#' 
#' @return A list/single object of type ImageDatasetReference.
#' 
#' @export
#' 
list_img_datasets <- function(token,
                              plate_id,
                              type = c("raw", "segmentation"),
                              most_recent = TRUE) {

  type <- match.arg(type)
  type <- switch(type,
                 raw = "listRawImageDatasets",
                 segmentation = "listSegmentationImageDatasets")

  if (all(sapply(plate_id, is.character)))
    plate_id <- lapply(plate_id, create_plate_id, token = token)
  else if (has_json_class(plate_id, "PlateIdentifier"))
    plate_id <- list(plate_id)

  assert_that(is.list(plate_id),
              all(sapply(plate_id, has_json_class, "PlateIdentifier")))

  res <- query_openbis(type, list(token, plate_id), "IScreeningApiServer")

  if (most_recent)
    res[[which.max(sapply(res, `[[`, "registrationDate"))]]
  else
    res
}