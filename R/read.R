
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

#' @title Read meta data
#'
#' @description Read meta data downloaded from openBis using
#' [readr::read_delim]. The delimiting character is either ";" for public meta
#' data or "\\t" for dumped compound database files.
#' 
#' @param file The filename of the file to be read.
#' @param type A switch for the type of meta data to be downloaded.
#' @param col_types A column specification to be passed to [readr::read_delim].
#' @param ... All further arguments are passed to [readr::read_delim].
#' 
#' @return A tibble holding all read meta data or a subset thereof, depending
#' on the column specification.
#' 
#' @export
#' 
read_meta <- function(file,
                      type = c("full", "public"),
                      col_types = readr::cols(
                        .default = readr::col_character()),
                      ...) {

  assert_that(is.list(file),
              length(file) == 1,
              is.raw(file[[1]]))

  fn <- tempfile()
  dir.create(fn)
  on.exit(unlink(fn, recursive = TRUE))

  writeBin(file[[1]], file.path(fn, names(file)))

  readr::read_delim(file.path(fn, names(file)),
                    if (type == "public") ";" else "\t",
                    col_types = col_types, ...)
}