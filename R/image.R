
#' List image meta data and download images
#' 
#' Experiment level image meta data can be listed by passing experiment
#' representing objects (`Experiment` or `ExperimentIdentifier`) to
#' `list_image_metadata()` and data set level image meta data can be retrieved
#' by passing data set identifying objects which can be associated with image
#' data sets (data set id and data set reference objects). Images themselves
#' can be retrieved using `fetch_images()`. As with meta data listing, this
#' function can be dispatched on objects referencing or identifying data sets
#' associated with image data.
#' 
#' Data set level image meta data can be listed by passing a objects, which
#' implement the `IDatasetIdentifier` interface and are connected to image
#' data sets (this rules out feature data set references and leaves
#' `DatasetIdentifier`, `DatasetReference`, `ImageDatasetReference`,
#' `MicroscopyImageReference` and `PlateImageReference` objects). Two
#' different types of meta data objects are returned, depending on the `type`
#' argument: if it is set to `metadata` (default), objects of type
#' `ImageDatasetMetadata` and it it is set to `format`, objects of type
#' `DatasetImageRepresentationFormats` are returned. For experiment-level
#' image meta data, `ExperimentImageMetadata` objects are returned.
#'
#' Dispatch of `fetch_images()` is available for the same object types as data
#' set-level image meta data listing: `DatasetIdentifier`, `DatasetReference`,
#' `ImageDatasetReference`, `MicroscopyImageReference` and
#' `PlateImageReference`. The highest level of control over which images are
#' retrieved is achieved with `PlateImageReference` objects, which specify an
#' image data set, a well, a tile and a channel. The returned image format can
#' be modified by either passing an `ImageRepresentationFormat` object as the
#' `format` argument, by passing a single/list of format selection criterion
#' objects, which will be used to filter the available image representation
#' format objects or by specifying one or both of the `image_size` (expects an
#' `ImageSize` object) and `force_png` (logical switch) arguments.
#' 
#' `MicroscopyImageReference` objects contain channel information (as well as
#' tile information, which is not taken into account though). Therefore a
#' (list of) `WellPosition` object(s) has/have to be specified, for which then
#' all tiles are fetched for the given imaging channel. If the passed list of
#' `MicroscopyImageReference` objects contain instances that only differ in
#' tile number, redundancies are filtered out. An API call is necessary for
#' each non-redundant object.
#' 
#' Finally, `DatasetIdentifier`, `DatasetReference` and `ImageDatasetReference`
#' objects are all handled identically. For each of the specified data sets,
#' an imaging channel has to be provided and whenever the data set is
#' associated with an entire plate, a (list of) `WellPosition` object(s) as
#' well. If the data set is associated with a single well, the
#' `well_positions` can be left at its default value (NULL). If several data
#' sets are passed, an API call is necessary per data set. Possible
#' redundancies are not filtered.
#' 
#' Images are retrieved as Base64 encoded strings, which are converted to
#' binary using [base64enc::base64decode()] and subsequently read by
#' [magick::image_read()]. Attached to the images is the information, based
#' on which they were retrieved, including data set object, well positions
#' (where applicable) and channel (where applicable). This results in a list
#' with length corresponding to the number of API calls that were necessary.
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
#' @param thumbnails Logical switch; if TRUE, thumbnail images are retrieved
#' in which case the arguments `well_positions` and `image_size` are expected
#' to be at their default values.
#' @param force_png Logical switch for making sure the returned image is a
#' png. If NULL or FALSE, the image is returned in the format it is stored.
#' @param format If not NULL, a single `ImageRepresentationFormat` object.
#' Cannot be combined with non-default `image_size`, `force_png` and `format`
#' arguments.
#' 
#' @section TODO 1: For dispatch on `PlateImageReference` objects, currently
#' the only options controlling the returned images are an argument for image
#' size and a flag for forcing the returned format to png. OpenBis also
#' supports  pre-defined image transformations to be applied to the images
#' before they are sent to the requesting party. These transformations can be
#' requested by a code (options are listed in `ImageRepresentationFormat`
#' objects or in `ImageChannel` objects attached to `ImageDatasetMetadata`
#' objects). However, as no such transformations appear to be defined, this is
#' currently not implemented.
#' 
#' @section TODO 2: When filtering `ImageRepresentationFormat` objects
#' associated with a data set, only `SizeCriterion` objects can be used. The
#' remaining criteria (`ColorDepthCriterion`, `FileTypeCriterion` and
#' `OriginalCriterion`) are currently disabled as they extend the abstract
#' class `AbstractFormatSelectionCriterion`, which causes an issue with JSON
#' deserialization.
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "loadImagesBase64")}
#' * \Sexpr{infx::docs_link("dsrs", "loadThumbnailImagesBase64")}
#' 
#' @rdname list_fetch_images
#' 
#' @examples
#' \dontrun{
#'   tok <- login_openbis("rdgr2014", "IXPubReview")
#' 
#'   # search for a sample object corresponding to plate KB2-03-1I
#'   samp <- search_openbis(tok,
#'                          search_criteria(
#'                            attribute_clause("code",
#'                                             "/INFECTX_PUBLISHED/KB2-03-1I")
#'                          ),
#'                          target_object = "sample")
#'   # for the plate sample object, list raw image data set references
#'   ds_ref <- list_references(tok, samp)
#' 
#'   # the returned image dataset reference can be used to list image meta data
#'   img_meta <- list_image_metadata(tok, ds_ref)
#'   channels <- img_meta[[1]][["channelCodes"]]
#' 
#'   imgs <- fetch_images(tok, ds_ref,
#'                        channels = channels[[1]],
#'                        well_positions = well_pos(1, 1),
#'                        image_size = json_class(width = 300, height = 300,
#'                                                class = "ImageSize"))
#'   # this yields 9 images, one per tile
#'   length(imgs[[1]]) == img_meta[[1]][["numberOfTiles"]]
#'   # and each image is scaled to fit within 300 x 300 pixels
#'   magick::image_info(imgs[[1]][[1]])
#' 
#'   # if not the entire well is of interest, but only certain tiles
#'   img_ref <- list_references(tok, ds_ref,
#'                              wells = well_pos(1, 1),
#'                              channels = channels[[1]])
#'   # this yields 9 objects, one reference per tile
#'   length(img_ref)
#'   # select a tile, for example the center one
#'   img <- fetch_images(tok, img_ref[[5]],
#'                       image_size = json_class(width = 300, height = 300,
#'                                               class = "ImageSize"))
#'   identical(as.raster(img[[1]]), as.raster(imgs[[1]][[5]]))
#' }
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
                             thumbnails = FALSE,
                             ...) {

  x <- as_json_vec(remove_null(x))

  if (thumbnails) {

    assert_that(is.null(well_positions), is.null(image_size),
                is.character(channels))

    channels <- as.list(channels)

    fun <- "loadThumbnailImagesBase64"

  } else {

    max_len <- max(length(x), length(channels))

    if (length(x) != max_len)
      x <- rep(x, max_len)
    if (length(channels) != max_len)
      channels <- rep(channels, max_len)

    assert_that(is.character(channels),
                length(x) == length(channels))

    fun <- "loadImagesBase64"

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
  }

  params <- if (thumbnails) {
    lapply(x, function(a) list(token, a, channels))
  } else if (!is.null(well_positions)) {
    mapply(function(a, b) list(token, a,  well_positions, b, image_size),
           x, channels, SIMPLIFY = FALSE)
  } else {
    mapply(function(a, b) list(token, a, b, image_size),
           x, channels, SIMPLIFY = FALSE)
  }

  res <- make_requests(api_url("dsrs"), fun, params, finally = process_imgs,
                       ...)

  mapply(function(dat, param) {

    if (length(dat) == 0) {
      warning("no images found for the given data set.")
      dat <- list()
    }

    attr(dat, "data_set") <- param[[2]]
    if (is.null(well_positions))
      attr(dat, "channel") <- param[[3]]
    else {
      attr(dat, "well_positions") <- param[[3]]
      attr(dat, "channel") <- param[[4]]
    }

    dat
  }, res, params, SIMPLIFY = FALSE)
}

#' @rdname list_fetch_images
#' @export
#' 
fetch_images.DatasetIdentifier <- fetch_img_for_ds

#' @rdname list_fetch_images
#' @export
#' 
fetch_images.DatasetReference <- fetch_img_for_ds

#' @rdname list_fetch_images
#' @export
#' 
fetch_images.ImageDatasetReference <- fetch_img_for_ds

#' @rdname list_fetch_images
#' @export
#' 
fetch_images.MicroscopyImageReference <- function(token,
                                                  x,
                                                  well_positions = NULL,
                                                  image_size = NULL,
                                                  thumbnails = FALSE,
                                                  ...) {
  x <- as_json_vec(x)

  drop <- duplicated(as.data.frame(t(sapply(x, `[`, c("datasetCode",
                                                      "channel")))))
  x <- x[!drop]

  channels <- sapply(x, `[[`, "channel")

  fetch_img_for_ds(token, x, channels, well_positions, image_size,
                   thumbnails, ...)
}

#' @rdname list_fetch_images
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrs", "loadPhysicalThumbnailsBase64")}
#' @export
#' 
fetch_images.PlateImageReference <- function(token,
                                             x,
                                             image_size = NULL,
                                             force_png = FALSE,
                                             format = NULL,
                                             thumbnails = FALSE,
                                             ...) {
  x <- as_json_vec(x)

  assert_that(is.logical(force_png), length(force_png) == 1L,
              is.logical(thumbnails), length(thumbnails) == 1L)

  if (thumbnails) {

    assert_that(is.null(image_size),
                !force_png)

    if (is.null(format)) {
      agruments <- list(sessionToken = token, imageReferences = x)
      fun <- "loadThumbnailImagesBase64"
    } else {
      assert_that(has_subclass(format, "ImageRepresentationFormat"))
      agruments <- list(token, x, as_json_class(remove_null(format)))
      fun <- "loadPhysicalThumbnailsBase64"
    }

  } else if (force_png || !is.null(image_size)) {

    assert_that(!thumbnails,
                is.null(format))

    fun <- "loadImagesBase64"

    if (force_png && is.null(image_size))
      agruments <- list(sessionToken = token, imageReferences = x,
                        convertToPng = force_png)
    else if (!force_png  && !is.null(image_size))
      agruments <- list(sessionToken = token, imageReferences = x,
                        size = image_size)
    else {
      settings <- json_class(desiredImageFormatPng = force_png,
                             desiredImageSize = image_size,
                             class = "LoadImageConfiguration")
      agruments <- list(sessionToken = token, imageReferences = x,
                        configuration = settings)
    }
  } else if (!is.null(format)) {

    assert_that(!thumbnails,
                !force_png,
                is.null(image_size))

    fun <- "loadImagesBase64"

    format_criteria <- c("SizeCriterion")

    if (has_subclass(format, "ImageRepresentationFormat"))
      agruments <- list(sessionToken = token, imageReferences = x,
                        format = as_json_class(remove_null(format)))
    else if (is.list(format)) {

      if (is_json_class(format))
        format <- list(format)

      assert_that(all(sapply(format, function(form) {
        any(sapply(format_criteria,
                   function(crit) has_subclass(form, crit)))
      })))

      agruments <- list(sessionToken = token, imageReferences = x,
                        criteria = format)
    }
  } else {

    fun <- "loadImagesBase64"
    agruments <- list(sessionToken = token, imageReferences = x)
  }

  res <- make_request(api_url("dsrs"), fun, agruments, finally = process_imgs,
                      ...)

  if (length(res) == 0L)
    res <- rep(list(NULL), length(x))

  assert_that(length(res) == length(x))

  mapply(function(dat, param) {

    if (length(dat) == 0) {
      warning("no images found for the given data set.")
      dat <- list()
    }

    attr(dat, "data_set") <- param
    dat
  }, res, x, SIMPLIFY = FALSE)
}

process_imgs <- function(imgs)
  lapply(imgs$result,
         function(x) magick::image_read(base64enc::base64decode(x)))

#' @param type Switch to specify the type of meta data objects to be returned.
#' 
#' @rdname list_fetch_images
#' 
#' @export
#' 
list_image_metadata <- function(token, x, ...)
  UseMethod("list_image_metadata", x)

#' @rdname list_fetch_images
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("sas", "getExperimentImageMetadata")}
#' * \Sexpr{infx::docs_link("dsrs", "listImageMetadata")}
#' * \Sexpr{infx::docs_link("dsrs", "listAvailableImageRepresentationFormats")}
#' 
#' @export
#' 
list_image_metadata.ExperimentIdentifier <- function(token, x, ...) {

  params <- lapply(as_json_vec(x), function(y) list(token, y))

  res <- make_requests(api_url("sas"), "getExperimentImageMetadata", params,
                       ...)
  as_json_vec(do.call(c, res))
}

#' @rdname list_fetch_images
#' @export
#' 
list_image_metadata.Experiment <- function(token, x, ...)
  list_image_metadata(token, exp_to_expid(x), ...)

fetch_img_meta <- function(token, x, type = c("metadata", "format"), ...) {

  fun <- switch(match.arg(type),
                metadata = "listImageMetadata",
                format = "listAvailableImageRepresentationFormats")

  make_request(api_url("dsrs"), fun, list(token, as_json_vec(x)), ...)
}

#' @rdname list_fetch_images
#' @export
#' 
list_image_metadata.DatasetIdentifier <- fetch_img_meta

#' @rdname list_fetch_images
#' @export
#' 
list_image_metadata.DatasetReference <- fetch_img_meta

#' @rdname list_fetch_images
#' @export
#' 
list_image_metadata.ImageDatasetReference <- fetch_img_meta

#' @rdname list_fetch_images
#' @export
#' 
list_image_metadata.MicroscopyImageReference <- fetch_img_meta

#' @rdname list_fetch_images
#' @export
#' 
list_image_metadata.PlateImageReference <- fetch_img_meta
