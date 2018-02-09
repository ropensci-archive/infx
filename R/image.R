
#' List image metadata
#'
#' Experiment metadata can be fetched using `list_image_metadata()`,
#' which accepts either a set of `Experiment`, `ExperimentIdentifier` or
#' `ImageDatasetReference` objects and returns all corresponding meta data as
#' `json_vec` of either `ExperimentImageMetadata`, in case experiment objects
#' were used or `ImageDatasetMetadata` objects, in case
#' `ImageDatasetReference` objects were used for calling
#' `list_image_metadata()`.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned experiments, e.g. a set of
#' `ExperimentIdentifier` or `Project` objects.
#' @param ... Generic compatibility
#' 
#' @export
#' 
list_image_metadata <- function(token, x, ...)
  UseMethod("list_image_metadata", x)

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.ExperimentIdentifier <- function(token, x, ...) {

  res <- lapply(as_json_vec(x), function(y)
    request_openbis("getExperimentImageMetadata", list(token, y),
                    "IScreeningApiServer"))

  as_json_vec(do.call(c, res))
}

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.Experiment <- function(token, x, ...)
  list_image_metadata(token, exp_to_expid(x))

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.ImageDatasetReference <- function(token, x, ...)
  request_openbis("listImageMetadata", list(token, as_json_vec(x)),
                  "IDssServiceRpcScreening")

#' Fetch images
#'
#' Download Base64 encoded images as strings.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned images
#' @param ... Generic compatibility
#' @param channels A character vector of imaging channels
#' @param well_positions A (list of) `WellPosition` objects. If the object
#' passed as argument x already contains well position information this can
#' be NULL.
#' @param image_size Either a single `ImageSize` object or NULL, in which case
#' images are returned in full size.
#' 
#' @export
#' 
fetch_images <- function(token, x, ...)
  UseMethod("fetch_images", x)

fetch_img_for_ds <- function(token,
                             x,
                             channels,
                             well_positions = NULL,
                             image_size = NULL,
                             ...) {

  x <- as_json_vec(remove_null(x))

  max_len <- max(length(x), length(channels))

  if (length(x) != max_len)
    x <- rep(x, max_len)
  if (length(channels) != max_len)
    channels <- rep(channels, max_len)

  assert_that(is.character(channels),
              length(x) == length(channels))

  if (!is.null(image_size)) {
    image_size <- as_json_class(image_size)
    assert_that(has_subclass(image_size, "ImageSize"))
  } else {
    image_size <- NA
  }

  if (!is.null(well_positions)) {
    well_positions <- as_json_vec(well_positions)
    assert_that(has_subclass(well_positions, "WellPosition"))
  }

  params <- if (!is.null(well_positions)) {
    mapply(function(a, b) list(token, a,  well_positions, b, image_size),
           x, channels, SIMPLIFY = FALSE)
  } else {
    mapply(function(a, b) list(token, a, b, image_size),
           x, channels, SIMPLIFY = FALSE)
  }

  lapply(params, function(param) {
    dat <- request_openbis("loadImagesBase64", param,
                           "IDssServiceRpcScreening")
    dat <- lapply(dat,
                  function(y) magick::image_read(base64enc::base64decode(y)))

    if (length(dat) == 0 && is.null(well_positions))
      warning("when not specifying well positions, the supplied dataset ",
              "should be associated with a well and not with a plate.")

    c(stats::setNames(param[-1],
                      if (is.null(well_positions)) c("data_set", "channel")
                      else c("data_set", "well_positions", "channel")),
      data = list(dat)
    )
  })
}

#' @rdname fetch_images
#' @export
#' 
fetch_images.DatasetIdentifier <- fetch_img_for_ds

#' @rdname fetch_images
#' @export
#' 
fetch_images.DatasetReference <- fetch_img_for_ds

#' @rdname fetch_images
#' @export
#' 
fetch_images.ImageDatasetReference <- fetch_img_for_ds

#' @rdname fetch_images
#' @export
#' 
fetch_images.MicroscopyImageReference <- function(token,
                                                  x,
                                                  well_positions = NULL,
                                                  image_size = NULL,
                                                  ...) {
  x <- as_json_vec(x)

  drop <- duplicated(as.data.frame(t(sapply(x, `[`, c("datasetCode",
                                                      "channel")))))
  x <- x[!drop]

  channels <- sapply(x, `[[`, "channel")

  fetch_img_for_ds(token, x, channels, well_positions, image_size, ...)
}
