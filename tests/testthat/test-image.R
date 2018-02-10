context("image")

test_that("experiment metadata can be listed", {

  check_skip()

  meta_1 <- list_image_metadata(tok, exp_ids[[1]])
  expect_is(meta_1, "ExperimentImageMetadata")
  expect_is(meta_1, "json_vec")
  expect_identical(get_subclass(meta_1), "ExperimentImageMetadata")
  expect_true(all(sapply(meta_1, has_subclass,
                         "ExperimentImageMetadata")))
  expect_gte(length(meta_1), 1L)

  meta_2 <- list_image_metadata(tok, exp_ids[1:2])
  expect_is(meta_2, "ExperimentImageMetadata")
  expect_is(meta_2, "json_vec")
  expect_identical(get_subclass(meta_2), "ExperimentImageMetadata")
  expect_true(all(sapply(meta_2, has_subclass,
                         "ExperimentImageMetadata")))
  expect_gte(length(meta_2), length(meta_1))

  expect_identical(list_image_metadata(tok, experiments[[1]]), meta_1)
  expect_identical(list_image_metadata(tok, experiments[1:2]), meta_2)

  img_ds <- list_references(tok, plates[1:2])

  meta_1 <- list_image_metadata(tok, img_ds[[1]])
  expect_is(meta_1, "ImageDatasetMetadata")
  expect_is(meta_1, "json_vec")
  expect_identical(get_subclass(meta_1), "ImageDatasetMetadata")
  expect_true(all(sapply(meta_1, has_subclass,
                         "ImageDatasetMetadata")))
  expect_equal(length(meta_1), 1L)

  meta_2 <- list_image_metadata(tok, img_ds[1:2])
  expect_is(meta_2, "ImageDatasetMetadata")
  expect_is(meta_2, "json_vec")
  expect_identical(get_subclass(meta_2), "ImageDatasetMetadata")
  expect_true(all(sapply(meta_2, has_subclass,
                         "ImageDatasetMetadata")))
  expect_equal(length(meta_2), 2L)

  meta_1 <- list_image_metadata(tok, img_ds[[1]], "format")
  expect_is(meta_1, "DatasetImageRepresentationFormats")
  expect_is(meta_1, "json_vec")
  expect_identical(get_subclass(meta_1), "DatasetImageRepresentationFormats")
  expect_true(all(sapply(meta_1, has_subclass,
                         "DatasetImageRepresentationFormats")))
  expect_equal(length(meta_1), 1L)

  meta_2 <- list_image_metadata(tok, img_ds[1:2], "format")
  expect_is(meta_2, "DatasetImageRepresentationFormats")
  expect_is(meta_2, "json_vec")
  expect_identical(get_subclass(meta_2), "DatasetImageRepresentationFormats")
  expect_true(all(sapply(meta_2, has_subclass,
                         "DatasetImageRepresentationFormats")))
  expect_equal(length(meta_2), 2L)
})

test_that("image data can be fetched", {

  check_skip()

  ds_ids <- list_dataset_ids(tok, datasets[1:2])
  well_pos <- as_json_vec(lapply(wells, `[[`, "wellPosition"))
  img_size <- json_class(width = 300, height = 300, class = "ImageSize")

  # test plate DatasetIdentifier with HCS_IMAGE_CONTAINER_RAW type
  img_111 <- fetch_images(tok, ds_ids[[1]], "DAPI", well_pos[[1]], img_size)
  expect_equal(length(img_111), 1L)
  expect_equal(length(img_111[[1]][["data"]]), 9L)
  expect_true(all(sapply(img_111[[1]][["data"]], class) == "magick-image"))
  expect_true(all(sapply(lapply(img_111[[1]][["data"]], magick::image_info),
                  `[[`, "width") == 300L))

  # test plate DatasetIdentifier with CLUSTER_JOB_LOGS type
  expect_error(fetch_images(tok, ds_ids[[2]], "DAPI", well_pos[[1]], img_size))

  img_ds <- list_references(tok, plates[1:2])

  # test plate ImageDatasetReference for two plates
  img_211 <- fetch_images(tok, img_ds[1:2], "DAPI", well_pos[[1]], img_size)
  expect_equal(length(img_211), 2L)
  expect_true(all(sapply(lapply(img_211, `[[`, "data"), length) == 9L))
  expect_true(all(sapply(unlist(lapply(img_211, `[[`, "data")),
                         class) == "magick-image"))
  expect_true(all(sapply(lapply(img_211[[1]][["data"]], magick::image_info),
                  `[[`, "width") == 300L))

  # test plate ImageDatasetReference for two wells
  img_112 <- fetch_images(tok, img_ds[[1]], "GFP", well_pos[1:2], img_size)
  expect_equal(length(img_112), 1L)
  expect_equal(length(img_112[[1]][["data"]]), 18L)
  expect_true(all(sapply(img_112[[1]][["data"]], class) == "magick-image"))
  expect_true(all(sapply(lapply(img_112[[1]][["data"]], magick::image_info),
                  `[[`, "width") == 300L))

  # test plate ImageDatasetReference for no specific wells
  expect_warning(img_110 <- fetch_images(tok, img_ds[[1]], "DAPI", NULL,
                                         img_size))
  expect_equal(length(img_110), 1L)
  expect_equal(length(img_110[[1]][["data"]]), 0L)

  micro_ref <- list_references(tok, ds_ids[[1]], channels = c("DAPI", "GFP"))

  # test MicroscopyImageReference for two channels
  img_121 <- fetch_images(tok, micro_ref, well_pos[[1]], img_size)
  expect_equal(length(img_121), 2L)
  expect_true(all(sapply(lapply(img_121, `[[`, "data"), length) == 9L))
  expect_true(all(sapply(unlist(lapply(img_121, `[[`, "data")),
                         class) == "magick-image"))
  expect_true(all(sapply(lapply(img_121[[1]][["data"]], magick::image_info),
                  `[[`, "width") == 300L))

  micro_ref <- list_references(tok, ds_ids[[1]], channels = "DAPI")

  # test MicroscopyImageReference for full images
  img_111 <- fetch_images(tok, micro_ref[[1]], well_pos[[1]])
  expect_equal(length(img_111), 1L)
  expect_equal(length(img_111[[1]][["data"]]), 9L)
  expect_true(all(sapply(img_111[[1]][["data"]], class) == "magick-image"))
  expect_true(all(sapply(lapply(img_111[[1]][["data"]], magick::image_info),
                  `[[`, "width") > 300L))

  # test PlateImageReference
  pi_ref <- list_references(tok, ds_ids[[1]], wells = well_pos[[1]],
                            channels = c("DAPI", "GFP"))

  img_1 <- fetch_images(tok, pi_ref[[1]])
  expect_equal(length(img_1), 1L)
  expect_equal(length(img_1[[1]][["data"]]), 1L)
  expect_true(class(img_1[[1]][["data"]]) == "magick-image")
  expect_true(magick::image_info(img_1[[1]][["data"]])[["width"]] > 300L)

  img_2 <- fetch_images(tok, pi_ref[1:2])
  expect_equal(length(img_2), 2L)
  expect_true(all(sapply(lapply(img_2, `[[`, "data"), length) == 1L))
  expect_true(all(sapply(lapply(img_2, `[[`, "data"),
                         class) == "magick-image"))
  expect_true(all(sapply(lapply(lapply(img_2, `[[`, "data"),
                         magick::image_info), `[[`, "width") > 300L))

  img_1 <- fetch_images(tok, pi_ref[[1]], force_png = TRUE)
  expect_equal(length(img_1), 1L)
  expect_equal(length(img_1[[1]][["data"]]), 1L)
  expect_true(class(img_1[[1]][["data"]]) == "magick-image")
  expect_true(magick::image_info(img_1[[1]][["data"]])[["format"]] == "PNG")

  img_1 <- fetch_images(tok, pi_ref[[1]], image_size = img_size)
  expect_equal(length(img_1), 1L)
  expect_equal(length(img_1[[1]][["data"]]), 1L)
  expect_true(class(img_1[[1]][["data"]]) == "magick-image")
  expect_true(magick::image_info(img_1[[1]][["data"]])[["width"]] == 300L)

  img_1 <- fetch_images(tok, pi_ref[[1]], image_size = img_size,
                        force_png = TRUE)
  expect_equal(length(img_1), 1L)
  expect_equal(length(img_1[[1]][["data"]]), 1L)
  expect_true(class(img_1[[1]][["data"]]) == "magick-image")
  expect_true(magick::image_info(img_1[[1]][["data"]])[["width"]] == 300L)
  expect_true(magick::image_info(img_1[[1]][["data"]])[["format"]] == "PNG")

  format <- list_image_metadata(tok, img_ds[[1]], "format")
  img_rep <- format[[1]][["imageRepresentationFormats"]][[2]]

  img_1 <- fetch_images(tok, pi_ref[[1]], format = img_rep)
  expect_equal(length(img_1), 1L)
  expect_equal(length(img_1[[1]][["data"]]), 1L)
  expect_true(class(img_1[[1]][["data"]]) == "magick-image")
  expect_true(magick::image_info(img_1[[1]][["data"]])[["width"]] < 300L)

  size <- json_class(width = 100,
                     height = 100,
                     type = "SMALLEST_COVERING_BOUNDING_BOX",
                     class = "SizeCriterion")

  img_1 <- fetch_images(tok, pi_ref[[1]], format = size)
  expect_equal(length(img_1), 1L)
  expect_equal(length(img_1[[1]][["data"]]), 1L)
  expect_true(class(img_1[[1]][["data"]]) == "magick-image")
  expect_true(magick::image_info(img_1[[1]][["data"]])[["width"]] < 300L)

  expect_error(fetch_images(tok, pi_ref[[1]], format = img_rep,
                            image_size = img_size))
})
