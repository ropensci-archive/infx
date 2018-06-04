
tok <- login_openbis()

projects <- list_projects(tok)

exp_ids <- list_experiment_ids(tok)
experiments <- list_experiments(tok, exp_ids[1:2])

samples <- list_samples(tok, exp_ids[[1]])

plates <- list_plates(tok, exp_ids[[1]])
plate_meta <- list_plate_metadata(tok, plates[1:2])
wells <- list_wells(tok, plates[[1]])

datasets <- list_datasets(tok, samples[[1]])

expect_attr <- function(object, attr) {

  act <- quasi_label(rlang::enquo(object))

  act$attrs <- names(attributes(object))

  expect(
    attr %in% act$attrs,
    sprintf("%s is not among attributes of %s: %s.",
            attr, act$lab, paste(act$attrs, collapse = ", "))
  )

  invisible(act$val)
}
