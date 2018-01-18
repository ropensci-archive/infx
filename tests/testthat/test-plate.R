context("plate")

test_that("plates can be listed", {
  plate <- list_plates(tok)
  expect_is(plate, "Plate")
  expect_is(plate, "json_vec")
  expect_identical(get_common_subclass(plate), "Plate")
  expect_true(all(sapply(plate, has_json_subclass, "Plate")))
  expect_gte(length(plate), 1L)

  exp_ids <- list_experiment_ids(tok)
  experiments <- list_experiments(tok, exp_ids[1:2])

  plate_1 <- list_plates(tok, exp_ids[[1]])
  expect_is(plate_1, "Plate")
  expect_is(plate_1, "json_vec")
  expect_identical(get_common_subclass(plate_1), "Plate")
  expect_true(all(sapply(plate_1, has_json_subclass, "Plate")))
  expect_gte(length(plate_1), 1L)

  plate_2 <- list_plates(tok, exp_ids[c(1, 2)])
  expect_is(plate_2, "Plate")
  expect_is(plate_2, "json_vec")
  expect_identical(get_common_subclass(plate_2), "Plate")
  expect_true(all(sapply(plate_2, has_json_subclass, "Plate")))
  expect_gte(length(plate_2), length(plate_1))

  expect_identical(plate_1, list_plates(tok, experiments[[1]]))
  expect_identical(plate_2, list_plates(tok, experiments[1:2]))
})
