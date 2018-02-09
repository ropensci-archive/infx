
#' List image metadata
#'
#' Experiment level image meta data is listed using `list_image_metadata()`,
#' which accepts either a (set of) `Experiment` or `ExperimentIdentifier`
#' object(s) and returns all corresponding meta data as `json_vec` of
#' `ExperimentImageMetadata` (objects).
#' 
#' Experiment level image meta data can be listed by passing a (set of)
#' object(s), which implement the `IDatasetIdentifier` interface and are
#' connected to image datasets (this rules out feature vector datasets). Two
#' different types of meta data objects are returned, depending on the `type`
#' argument: if it is set to `metadata` (default), objects of type
#' `ImageDatasetMetadata` and it it is set to `format`, objects of type
#' `DatasetImageRepresentationFormats` are returned.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit the number of returned experiments, e.g. a set of
#' `ExperimentIdentifier` or `Project` objects.
#' @param type Switch to specify the type of meta data objects to be returned.
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

fetch_img_meta <- function(token, x, type = c("metadata", "format"), ...) {

  fun <- switch(match.arg(type),
                metadata = "listImageMetadata",
                format = "listAvailableImageRepresentationFormats")

  request_openbis(fun, list(token, as_json_vec(x)), "IDssServiceRpcScreening")
}

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.DatasetIdentifier <- fetch_img_meta

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.DatasetReference <- fetch_img_meta

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.ImageDatasetReference <- fetch_img_meta

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.MicroscopyImageReference <- fetch_img_meta

#' @rdname list_image_metadata
#' @export
#' 
list_image_metadata.PlateImageReference <- fetch_img_meta


#' Fetch images
#'
#' Dispatch is - as per openBis implementation - possible on any object that
#' implements the `IDatasetIdentifier` interface. However objects referencing
#' feature are not connected to image datasets and therefore dispatch on
#' `FeatureVectorDatasetReference` and `FeatureVectorDatasetWellReference`
#' is not enabled. The highest level of control over which images are retrieves
#' is achieved with `PlateImageReference` objects, which specify an image
#' dataset, a well, a tile and a channel.
#' 
#' `MicroscopyImageReference` objects contain channel information (as well as
#' tile information, which is not taken into account though). Therefore a
#' (list of) `WellPosition` object(s) has/have to be specified, for which then
#' all tiles are fetched for the given imaging channel. If the inputed list of
#' `MicroscopyImageReference` objects contain instances that only differ in
#' tile number, redundancies are filtered out. An API call is necessary for
#' each non-redundant object.
#' 
#' Finally, `DatasetIdentifier`, `DatasetReference` and `ImageDatasetReference`
#' objects are all handled identically. For each of the specified datasets,
#' an imaging channel has to be provided and whenever the dataset is associated
#' with an entire plate, a (list of) `WellPosition` object(s) as well. If the
#' dataset is associated with a single well, the `well_positions` can be left
#' at its default value (NULL). If several datasets are passed, an API call is
#' necessary per dataset. Possible redundancies are not checked for.
#' 
#' Images are retrieved as Base64 encoded strings, which are converted to
#' binary using [base64enc::base64decode()] and subsequently read by
#' [magick::image_read()]. Attached to the image(s) is the information with
#' based on which they/it were/was retrieved , including dataset object, well
#' positions (where applicable) and channel (where applicable). This results
#' in a list with length corresponding to the number of API calls that were
#' necessary.
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
#' @param force_png Logical switch for making sure the returned image is a
#' png. If NULL or FALSE, the image is returned in the format it is stored.
#' 
#' @section TODO: For dispatch on `PlateImageReference` objects, currently the
#' only options controlling the returned images are an argument for image size
#' and a flag for forcing the returned format to png. OpenBis also supports
#' pre-defined image transformations to be applied to the images before they
#' are sent to the requesting party. These transformations can be requested by
#' a code (options are listed in `ImageRepresentationFormat` objects or in
#' `ImageChannel` objects attached to `ImageDatasetMetadata` objects). However,
#' as no such transformations appear to be defined, this is currently not
#' implemented.
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

#' @rdname fetch_images
#' @export
#' 
fetch_images.PlateImageReference <- function(token,
                                             x,
                                             image_size = NULL,
                                             force_png = FALSE,
                                             ...) {
  x <- as_json_vec(x)

  assert_that(is.logical(force_png),
              length(force_png) == 1L)

  if (!force_png && is.null(image_size)) {

    res <- request_openbis("loadImagesBase64", list(token, x),
                           "IDssServiceRpcScreening")

  } else {

    settings <- json_class(desiredImageFormatPng = force_png,
                           class = "LoadImageConfiguration")

    if (!is.null(image_size)) {
      image_size <- as_json_class(image_size)
      assert_that(has_subclass(image_size, "ImageSize"))
      settings[["desiredImageSize"]] <- image_size
    }

    res <- request_openbis("loadImagesBase64", list(token, x, settings),
                           "IDssServiceRpcScreening")

    res <- request_openbis("listAvailableImageRepresentationFormats",
                           list(token, x),
                           "IDssServiceRpcScreening")
  }

  assert_that(length(res) == length(x))

  mapply(function(a, b) {
    list(data_set = b, data = magick::image_read(base64enc::base64decode(a)))
  }, res, x, SIMPLIFY = FALSE)
}
