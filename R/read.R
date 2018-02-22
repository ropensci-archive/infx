
#' Read single cell data
#'
#' Calling [R.matlab::readMat()] single cell feature files in Matlab format, as
#' produced by CellProfiler are read and checked for the correct structure.
#' The Matlab file is expected to contain a nested list where each node
#' corresponds to an image and contains a list which is either holding a
#' single value or a vector of values.  
#' 
#' @param data The data to be read.
#' 
#' @rdname read_files
#' @export
#' 
read_cc_mat <- function(data) {

  is_int <- function(x) {
    if (is.numeric(x))
      isTRUE(all.equal(x, suppressWarnings(as.integer(x)),
                       check.attributes = FALSE))
    else
      FALSE
  }

  as_int <- function(x) if (is_int(x)) as.integer(x) else x

  reduce_nesting <- function(x) {
    if (is.list(x) && length(x) == 1L)
      x <- x[[1]]
    if (is.list(x))
      lapply(x, reduce_nesting)
    else
      drop(x)
  }

  extract_data <- function(x, y = character()) {
    if (is.list(x) && length(x) == 1L &&
        has_attr(x, "dim") && has_attr(x, "dimnames"))
      extract_data(x[[1]], c(y, unlist(attr(x, "dimnames"))))
    else {
      x <- reduce_nesting(x)
      attr(x, "info") <- y
      x
    }
  }

  adj_mat <- function(well) {
    if (is.list(well) && all(sapply(well, is.numeric))) {
      lengths <- sapply(well, length)
      entries <- unlist(well, recursive = FALSE)
      if (is_int(entries)) {
        well
      } else if (max(lengths) == 0L) {
        Matrix::Matrix(nrow = 0, ncol = 0)
      } else
        Matrix::sparseMatrix(rep(seq_along(lengths), lengths),
                             entries,
                             symmetric = TRUE, dims = rep(length(well), 2))
    } else
      well
  }

  tryCatch({

    dat <- R.matlab::readMat(data, fixNames = FALSE, drop = "singletonLists")
    dat <- extract_data(dat[["handles"]])

    info <- setdiff(attr(dat, "info"), "Measurements")
    assert_that(length(info) == 2L, is.list(dat))

    if (info[1] == "Neighbors")
      dat <- lapply(dat, adj_mat)

    dat <- lapply(dat, as_int)

    attr(dat, "object") <- info[1]
    attr(dat, "feature") <- info[2]

    dat
  },
  error = function(e) {
    warning("a read error occurred ",
            if (exists("info")) paste0("(", info[1], ": ", info[2], ") "),
            ":\n  ", e)
    data
  })
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
              grepl("\\.csv\\.gz$", names(dat)))

  read_delim(gzcon(rawConnection(dat[[1]])), delim = "\t", ...)
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