
#' List files
#'
#' The function `list_files()` lists files associated with one or more
#' dataset(s), which can be specified with codes or `DataSet`/
#' `DatasetIdentifier` objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit search for datasets/files with.
#' @param path A (vector of) file path(s) to be searched within a dataset.
#' @param recursive A (vector of) logicals, indicating whether to list files
#' recursively.
#' @param ... Generic compatibility.
#' 
#' @export
#' 
list_files <- function(token, x, ...)
  UseMethod("list_files", x)

#' @rdname list_files
#' @export
#' 
list_files.character <- function(token, x, path = "", recursive = TRUE, ...) {

  check_rep <- function(vec, len) {
    if (length(vec) == 1L)
      vec <- rep(vec, len)
    assert_that(length(vec) == len || length(vec) == 1L)
    vec
  }

  max_length <- max(length(x), length(path), length(recursive))

  x <- check_rep(x, max_length)
  path <- check_rep(path, max_length)
  recursive <- check_rep(recursive, max_length)

  res <- mapply(function(a, b, c) {
    request_openbis("listFilesForDataSet", list(token, a, b, c),
                    "IDssServiceRpcGeneric")
  }, x, path, recursive)

  as_json_vec(do.call(c, res))
}

#' @rdname list_files
#' @export
#' 
list_files.DataSet <- function(token, x, path = "", recursive = TRUE, ...)
  list_files(token, dataset_code(x), path, recursive)

#' @rdname list_files
#' @export
#' 
list_files.DatasetIdentifier <- function(token,
                                         x,
                                         path = "",
                                         recursive = TRUE,
                                         ...) {

  list_files(token, dataset_code(x), path, recursive)
}

#' @rdname list_files
#' @export
#' 
list_files.DataSetFileDTO <- function(token, x, ...) {

  res <- lapply(as_json_vec(x), function(y) {
    request_openbis("listFilesForDataSet", list(token, y),
                    "IDssServiceRpcGeneric")
  })

  as_json_vec(do.call(c, res))
}
