
#' @title Read single cell data
#'
#' @description Calling [R.matlab::readMat] single cell feature files in
#' Matlab format, as produced by CellProfiler are read and checked for the
#' correct structure. The Matlab file is expected to contain a nested list
#' where each node corresponds to an image and contains a list which is either
#' holding a single value or a vector of values.  
#' 
#' @param data The data to be read.
#' 
#' @return A vector holding all data. The attribute \code{length} can be used
#' to split the linearized data into bins corresponding to images.
#' 
#' @export
#' 
read_data <- function(data) {

  is_int <- function(x) {
    isTRUE(all.equal(x, suppressWarnings(as.integer(x)),
                     check.attributes = FALSE))
  }

  assert_that(is.raw(data))

  data <- R.matlab::readMat(data)

  assert_that(is.list(data), length(data) == 1L)
  data <- data[["handles"]]
  info <- character()
  repeat {
    if (length(data) > 1L) break
    info <- c(info, unlist(attr(data, "dimnames")))
    assert_that(is.list(data), all(attr(data, "dim") == c(rep(1L, 3))))
    data <- data[[1]]
  }
  assert_that(length(info) == 3L, info[1] == "Measurements")

  data <- unlist(data, recursive = FALSE)
  assert_that(is.list(data), !any(sapply(data, is.list)))

  dims <- t(sapply(data, dim))
  assert_that(!any(dims[, 2] > 1L))

  res <- unlist(data, recursive = FALSE)

  if (is.numeric(res)) {
    if (is_int(res)) res <- as.integer(res)
    else if (all(sapply(data, attr, "Csingle"))) attr(res, "Csingle") <- TRUE
  }

  attr(res, "lengths") <- as.integer(dims[, 1])
  attr(res, "object") <- info[2]
  attr(res, "feature") <- info[3]

  res
}

#' @title Read public meta data
#'
#' @description Read public meta data downloaded from openBis using
#' [read_delim].
#' 
#' @param dat The data (raw vector) to be read.
#' @param ... All further arguments are passed to [read_delim].
#' 
#' @return A tibble holding all read meta data or a subset thereof, depending
#' on the column specification.
#' 
#' @export
#' 
read_pub_meta <- function(dat, ...) {

  assert_that(is.list(dat),
              length(dat) == 1L,
              is.raw(dat[[1]]),
              grepl("\\.csv\\.zip$", names(dat)))

  # as per https://stackoverflow.com/a/3053883, need to write zip to disk
  dir <- tempfile()
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE))

  writeBin(dat[[1]], file.path(dir, names(dat)))

  read_delim(file.path(dir, names(dat)), delim = ";", ...)
}

#' @title Read full meta data
#'
#' @description Read full meta data downloaded from openBis using
#' [read_delim].
#' 
#' @inheritParams read_pub_meta
#' @param ... All further arguments are passed to [read_delim]. If an
#' argument should behave differently for each table, a named list is expected
#' with names
#' \enumerate{
#'   \item \code{\"well_annotation\"}
#'   \item \code{\"sequence_information_sirna\"}
#'   \item \code{\"sequence_information_mirna\"}
#'   \item \code{\"sequence_information_esirna\"}
#'   \item \code{\"sequence_information_compound\"}
#'   \item \code{\"pool_contained_compound_lookup\"}
#' }
#' for this argument in order to match to the respective table.
#' 
#' @return List of tibbles holding all read meta data or a subset thereof,
#' depending on the column specification.
#' 
#' @export
#' 
read_full_meta <- function(dat, ...) {

  assert_that(is.list(dat),
              length(dat) == 6L,
              all(sapply(dat, is.raw)),
              all(grepl("\\.tsv\\.gz$", names(dat))))

  names(dat) <- tolower(sub("\\..+$", "", names(dat)))

  tbls <- c("well_annotation", "sequence_information_sirna",
            "sequence_information_mirna", "sequence_information_esirna",
            "sequence_information_compound", "pool_contained_compound_lookup")

  assert_that(setequal(names(dat), tbls))

  args <- lapply(dat, function(x) list(file = gzcon(rawConnection(x))))

  rest <- c(list(delim = "\t"), list(...))

  indiv <- sapply(rest, length) > 1L &
    sapply(rest, is.list) &
    !sapply(rest, inherits, "col_spec")
  assert_that(all(sapply(rest[indiv], function(x) setequal(names(x), tbls))))

  args <- lapply(stats::setNames(names(args), names(args)), function(x)
      c(args[[x]], rest[!indiv], lapply(rest[indiv], `[[`, x)))

  lapply(args, function(x) do.call(read_delim, x))
}

#' @title Read a delimited text file
#'
#' @description A wrapper around [readr::read_delim()], which provides a
#' different default value for col_types (all columns are read as character
#' instead of auto-detect).
#' 
#' @param file,col_types,... All are passed to [readr::read_delim()].
#' 
#' @return A tibble holding all read meta data or a subset thereof, depending
#' on the column specification.
#' 
#' @export
#' 
read_delim <- function(file,
                       ...,
                       col_types = readr::cols(
                         .default = readr::col_character()
                       )) {

  readr::read_delim(file, ..., col_types = col_types)

}