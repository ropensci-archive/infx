context("image")

test_that("experiment metadata can be listed", {

  check_skip()

  meta_1 <- list_image_metadata(tok, exp_ids[[1]])
  expect_s3_class(meta_1, "ExperimentImageMetadata")
  expect_s3_class(meta_1, "json_vec")
  expect_gte(length(meta_1), 1L)
  for (i in seq_along(meta_1)) {
    expect_s3_class(meta_1[[i]], "ExperimentImageMetadata")
    expect_s3_class(meta_1[[i]], "json_class")
  }

  meta_2 <- list_image_metadata(tok, exp_ids[1:2])
  expect_s3_class(meta_2, "ExperimentImageMetadata")
  expect_s3_class(meta_2, "json_vec")
  expect_gte(length(meta_2), length(meta_1))
  for (i in seq_along(meta_2)) {
    expect_s3_class(meta_2[[i]], "ExperimentImageMetadata")
    expect_s3_class(meta_2[[i]], "json_class")
  }

  expect_identical(list_image_metadata(tok, experiments[[1]]), meta_1)
  expect_identical(list_image_metadata(tok, experiments[1:2]), meta_2)

  img_ds <- list_references(tok, plates[1:2])

  meta_1 <- list_image_metadata(tok, img_ds[[1]])
  expect_s3_class(meta_1, "ImageDatasetMetadata")
  expect_s3_class(meta_1, "json_class")
  expect_true(has_fields(meta_1, "imageDataset"))
  expect_s3_class(meta_1[["imageDataset"]], "ImageDatasetReference")
  expect_s3_class(meta_1[["imageDataset"]], "json_class")

  meta_2 <- list_image_metadata(tok, img_ds[1:2])
  expect_s3_class(meta_2, "ImageDatasetMetadata")
  expect_s3_class(meta_2, "json_vec")
  expect_length(meta_2, 2L)
  for (i in seq_along(meta_2)) {
    expect_s3_class(meta_2[[i]], "ImageDatasetMetadata")
    expect_s3_class(meta_2[[i]], "json_class")
    expect_true(has_fields(meta_2[[i]], "imageDataset"))
    expect_s3_class(meta_2[[i]][["imageDataset"]], "ImageDatasetReference")
    expect_s3_class(meta_2[[i]][["imageDataset"]], "json_class")
  }

  meta_1 <- list_image_metadata(tok, img_ds[[1]], "format")
  expect_s3_class(meta_1, "DatasetImageRepresentationFormats")
  expect_s3_class(meta_1, "json_class")
  expect_true(has_fields(meta_1, c("dataset", "imageRepresentationFormats")))
  expect_s3_class(meta_1[["dataset"]], "ImageDatasetReference")
  expect_s3_class(meta_1[["dataset"]], "json_class")
  expect_gte(length(meta_1), 1L)
  expect_s3_class(meta_1[["imageRepresentationFormats"]],
                  "ImageRepresentationFormat")
  expect_s3_class(meta_1[["imageRepresentationFormats"]], "json_vec")
  for (i in seq_along(meta_1[["imageRepresentationFormats"]])) {
    expect_s3_class(meta_1[["imageRepresentationFormats"]][[i]],
                    "ImageRepresentationFormat")
    expect_s3_class(meta_1[["imageRepresentationFormats"]][[i]], "json_class")
  }

  meta_2 <- list_image_metadata(tok, img_ds[1:2], "format")
  expect_s3_class(meta_2, "DatasetImageRepresentationFormats")
  expect_s3_class(meta_2, "json_vec")
  expect_length(meta_2, 2L)
  for (i in seq_along(meta_2)) {
    expect_s3_class(meta_2[[i]], "DatasetImageRepresentationFormats")
    expect_s3_class(meta_2[[i]], "json_class")
    expect_true(has_fields(meta_2[[i]],
                c("dataset", "imageRepresentationFormats")))
    expect_s3_class(meta_2[[i]][["dataset"]], "ImageDatasetReference")
    expect_s3_class(meta_2[[i]][["dataset"]], "json_class")
    expect_gte(length(meta_2[[i]]), 1L)
    expect_s3_class(meta_2[[i]][["imageRepresentationFormats"]],
                    "ImageRepresentationFormat")
    expect_s3_class(meta_2[[i]][["imageRepresentationFormats"]], "json_vec")
    for (j in seq_along(meta_2[[i]][["imageRepresentationFormats"]])) {
      expect_s3_class(meta_2[[i]][["imageRepresentationFormats"]][[j]],
                      "ImageRepresentationFormat")
      expect_s3_class(meta_2[[i]][["imageRepresentationFormats"]][[j]],
                      "json_class")
    }
  }
})

test_that("image data can be fetched", {

  check_skip()

  ds_ids <- list_dataset_ids(tok, datasets[1:2])
  well_pos <- as_json_vec(lapply(wells, `[[`, "wellPosition"))
  img_size <- json_class(width = 300, height = 300, class = "ImageSize")

  # test plate DatasetIdentifier with HCS_IMAGE_CONTAINER_RAW type
  img_111 <- fetch_images(tok, ds_ids[[1]], "DAPI", well_pos[[1]], img_size)
  expect_length(img_111, 1L)
  expect_length(img_111[[1]], 9L)
  for (i in seq_along(img_111[[1]])) {
    expect_is(img_111[[1]][[i]], "magick-image")
    expect_equal(magick::image_info(img_111[[1]][[i]])[["width"]], 300L)
  }

  # test plate DatasetIdentifier with CLUSTER_JOB_LOGS type
  expect_error(fetch_images(tok, ds_ids[[2]], "DAPI",
                              well_pos[[1]], img_size),
               paste("Dataset", ".+", "not", "found", "in", "the", "imaging",
                     "database", sep = "\\s+"))

  img_ds <- list_references(tok, plates[1:2])

  # test plate ImageDatasetReference for two plates
  img_211 <- fetch_images(tok, img_ds[1:2], "DAPI", well_pos[[1]], img_size)
  expect_length(img_211, 2L)
  for (i in seq_along(img_211)) {
    expect_length(img_211[[i]], 9L)
    for (j in seq_along(img_211[[i]])) {
      expect_is(img_211[[i]][[j]], "magick-image")
      expect_equal(magick::image_info(img_211[[i]][[j]])[["width"]], 300L)
    }
  }

  # test plate ImageDatasetReference for two wells
  img_112 <- fetch_images(tok, img_ds[[1]], "GFP", well_pos[1:2], img_size)
  expect_length(img_112, 1L)
  expect_length(img_112[[1]], 18L)
  for (i in seq_along(img_112[[1]])) {
    expect_is(img_112[[1]][[i]], "magick-image")
    expect_equal(magick::image_info(img_112[[1]][[i]])[["width"]], 300L)
  }

  # test plate ImageDatasetReference for no specific wells
  expect_warning(img_110 <- fetch_images(tok, img_ds[[1]], "DAPI", NULL,
                                         img_size),
                 "no images found")
  expect_length(img_110, 1L)
  expect_length(img_110[[1]], 0L)

  # test plate ImageDatasetReference for thumbnails
  expect_warning(img_110 <- fetch_images(tok, img_ds[[1]], "DAPI",
                                         thumbnails = TRUE),
                 "no images found")
  expect_length(img_110, 1L)
  expect_length(img_110[[1]], 0L)

  # test plate ImageDatasetReference for thumbnails
  expect_warning(img_120 <- fetch_images(tok, img_ds[[1]], c("DAPI", "GFP"),
                                         thumbnails = TRUE),
                 "no images found")
  expect_length(img_120, 1L)
  expect_length(img_120[[1]], 0L)

  # test plate ImageDatasetReference for thumbnails
  expect_warning(img_210 <- fetch_images(tok, img_ds[1:2], "DAPI",
                                         thumbnails = TRUE),
                 "no images found")
  expect_length(img_210, 2L)
  for (i in seq_along(img_210))
    expect_length(img_210[[i]], 0L)

  # test plate ImageDatasetReference for thumbnails
  expect_warning(img_220 <- fetch_images(tok, img_ds[1:2], c("DAPI", "GFP"),
                                         thumbnails = TRUE),
                 "no images found")
  expect_length(img_220, 2L)
  for (i in seq_along(img_220))
    expect_length(img_220[[i]], 0L)

  micro_ref <- list_references(tok, ds_ids[[1]], channels = c("DAPI", "GFP"))

  # test MicroscopyImageReference for two channels
  img_121 <- fetch_images(tok, micro_ref, well_pos[[1]], img_size)
  expect_length(img_121, 2L)
  for (i in seq_along(img_121)) {
    expect_length(img_121[[i]], 9L)
    for (j in seq_along(img_121[[i]])) {
      expect_is(img_121[[i]][[j]], "magick-image")
      expect_equal(magick::image_info(img_121[[i]][[j]])[["width"]], 300L)
    }
  }

  micro_ref <- list_references(tok, ds_ids[[1]], channels = "DAPI")

  # test MicroscopyImageReference for full images
  img_111 <- fetch_images(tok, micro_ref[[1]], well_pos[[1]])
  expect_length(img_111, 1L)
  expect_length(img_111[[1]], 9L)
  for (i in seq_along(img_111[[1]])) {
    expect_is(img_111[[1]][[i]], "magick-image")
    expect_gt(magick::image_info(img_111[[1]][[i]])[["width"]], 300L)
  }

  # test PlateImageReference
  pi_ref <- list_references(tok, ds_ids[[1]], wells = well_pos[[1]],
                            channels = c("DAPI", "GFP"))

  img_1 <- fetch_images(tok, pi_ref[[1]])
  expect_length(img_1, 1L)
  expect_length(img_1[[1]], 1L)
  expect_is(img_1[[1]], "magick-image")
  expect_gt(magick::image_info(img_1[[1]])[["width"]], 300L)

  img_2 <- fetch_images(tok, pi_ref[1:2])
  expect_length(img_2, 2L)
  for (i in seq_along(img_2)) {
    expect_length(img_2[[i]], 1L)
    expect_is(img_2[[i]], "magick-image")
    expect_gt(magick::image_info(img_2[[i]])[["width"]], 300L)
  }

  img_1 <- fetch_images(tok, pi_ref[[1]], force_png = TRUE)
  expect_length(img_1, 1L)
  expect_length(img_1[[1]], 1L)
  expect_is(img_1[[1]], "magick-image")
  expect_equal(magick::image_info(img_1[[1]])[["format"]], "PNG")
  expect_gt(magick::image_info(img_1[[1]])[["width"]], 300L)

  img_1 <- fetch_images(tok, pi_ref[[1]], image_size = img_size)
  expect_length(img_1, 1L)
  expect_length(img_1[[1]], 1L)
  expect_is(img_1[[1]], "magick-image")
  expect_equal(magick::image_info(img_1[[1]])[["width"]], 300L)

  img_1 <- fetch_images(tok, pi_ref[[1]], image_size = img_size,
                        force_png = TRUE)
  expect_length(img_1, 1L)
  expect_length(img_1[[1]], 1L)
  expect_is(img_1[[1]], "magick-image")
  expect_equal(magick::image_info(img_1[[1]])[["width"]], 300L)
  expect_equal(magick::image_info(img_1[[1]])[["format"]], "PNG")

  format <- list_image_metadata(tok, img_ds[[1]], "format")
  img_rep <- format[["imageRepresentationFormats"]][[2]]

  img_1 <- fetch_images(tok, pi_ref[[1]], format = img_rep)
  expect_length(img_1, 1L)
  expect_length(img_1[[1]], 1L)
  expect_is(img_1[[1]], "magick-image")
  expect_lt(magick::image_info(img_1[[1]])[["width"]], 300L)

  size <- json_class(width = 100,
                     height = 100,
                     type = "SMALLEST_COVERING_BOUNDING_BOX",
                     class = "SizeCriterion")

  img_1 <- fetch_images(tok, pi_ref[[1]], format = size)
  expect_length(img_1, 1L)
  expect_length(img_1[[1]], 1L)
  expect_is(img_1[[1]], "magick-image")
  expect_lt(magick::image_info(img_1[[1]])[["width"]], 300L)

  expect_error(fetch_images(tok, pi_ref[[1]], format = img_rep,
                            image_size = img_size))

  expect_warning(thumb_1 <- fetch_images(tok, pi_ref[[1]], thumbnails = TRUE),
                 "no images found")
  expect_length(thumb_1, 1L)
  expect_length(thumb_1[[1]], 0L)

  expect_warning(thumb_2 <- fetch_images(tok, pi_ref[1:2], thumbnails = TRUE),
                 "no images found")
  expect_length(thumb_2, 2L)
  for (i in seq_along(thumb_2))
    expect_length(thumb_2[[i]],  0L)

  expect_error(fetch_images(tok, pi_ref[[1]], format = img_rep,
                            thumbnails = TRUE),
                 "Couldn't fetch the image as raw content")
})
