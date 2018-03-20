
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
read_mat_files <- function(data) {

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
      info <- setdiff(y, "Measurements")
      assert_that(length(info) == 2L)
      attr(x, "object") <- info[1]
      attr(x, "feature") <- info[2]
      x
    }
  }

  tryCatch({
    dat <- R.matlab::readMat(data, fixNames = FALSE, drop = "singletonLists")
    extract_data(dat[["handles"]])
  },
  error = function(e) {
    warning("a read error occurred:\n  ", e)
    data
  })
}
