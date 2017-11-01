
#' @title Read single cell data
#'
#' @description Calling [R.matlab::readMat] single cell feature files in
#' Matlab format, as produced by CellProfiler are read and checked for the
#' correct structure. The Matlab file is expected to contain a nested list
#' where each node corresponds to an image and contains a list which is either
#' holding a single value or a vector of values.  
#' 
#' @param file The filename of the file to be read.
#' 
#' @return A vector holding all data. The attribute \code{length} can be used
#' to split the linearized data into bind corresponding to images.
#' 
#' @export
#' 
read_data <- function(file) {

  is_int <- function(x) {
    isTRUE(all.equal(x, suppressWarnings(as.integer(x)),
                     check.attributes = FALSE))
  }

  stopifnot(length(file) == 1,
            file.exists(file),
            grepl(".mat$", file))

  dat <- R.matlab::readMat(file)

  object <- unlist(strsplit(basename(file), "\\."))[1]
  feature <- unlist(strsplit(basename(file), "\\."))[2]

  # test first 4 levels of .mat file
  for (test in c("handles", "Measurements", object, gsub("_", ".", feature))) {
    hits <- c(attributes(dat)$names, attributes(dat)$dimnames) %in% test
    stopifnot(sum(hits) == 1)
    dat <- dat[[which(hits)]]
  }

  dat <- unlist(dat, recursive = FALSE)
  stopifnot(is.list(dat),
            !any(sapply(dat, is.list)))

  dims <- t(sapply(dat, dim))
  stopifnot(!any(dims[, 2] > 1))

  res <- unlist(dat, recursive = FALSE)

  if (is.numeric(res)) {
    if (is_int(res)) res <- as.integer(res)
    else if (all(sapply(dat, attr, "Csingle"))) attr(res, "Csingle") <- TRUE
  }

  attr(res, "lengths") <- as.integer(dims[, 1])
  attr(res, "object") <- tolower(object)
  attr(res, "feature") <- gsub("(_[1-9])?$", "", feature)

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

  readr::read_delim(file,
                    if (type == "public") ";" else "\t",
                    col_types = col_types, ...)
}