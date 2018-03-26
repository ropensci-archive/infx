
#' List and download files
#'
#' The function `list_files()` lists files associated with one or more
#' dataset(s). Datasets can be specified as character vector of dataset codes
#' and therefore all objects for which a [dataset_code()] method exists can
#' be used to select datasets. In addition to these dataset-like objects,
#' dispatch on `DataSetFileDTO` objects is possible as well.
#' 
#' Furthermore, the file search can be limited to a certain path within the
#' dataset and the search can be carried out recursively or non-recursively.
#' A separate API call is necessary for each of the objects the dispatch
#' occurs on. In case a set of objects is passed, the search-tuning arguments
#' `path` and `recursive` have to be euther of length 1 or of the same length
#' as `x`.
#' 
#' The function `fetch_files()` downloads files associated to a dataset.
#' Whenever dispatch occurs on a set of datasets (can either be a character
#' vector or any object for which a [dataset_code()] method exists), the set
#' of files to be downloaded can either be passed as the `files` argument or
#' all available files for that dataset are listed using [list_files()]
#' (folders themselves are removed), and this set of files is filtered if a
#' regular expression is passed as argument `file_regex`. The resulting set
#' of `FileInfoDssDTO` objects alongside the corresponding dataset ids are then
#' fetched, using `fetch_files()`. All named arguments passed as `...` are
#' forwarded to [list_files()] and the `FileInfoDssDTO`-specific
#' `fetch_files()` method.
#' 
#' In addition to datasets, dispatch can be on `FileInfoDssDTO` or
#' `DataSetFileDTO` objects. In case of `FileInfoDssDTO` objects being passed,
#' an additional character vector specifying the corresponding dataset ids is
#' required, as `FileInfoDssDTO` objects do not contain any dataset identifying
#' information. This character vector of dataset ids may be of length 1 or of
#' the same length as the number of `FileInfoDssDTO` objects. Finally,
#' `DataSetFileDTO` objects contain both path and dataset information so a
#' single object uniquely identifies a file in a dataset.
#' 
#' File fetching may be carried out in serial or in parallel fashion,
#' controlled by the `n_con` argument. If values `FALSE` or any integer `<= 1L`
#' are passed, downloads are performed non-concurrently and otherwise the
#' number of simultaneous connections is controlled by the integer passed as
#' `n_con`.
#' 
#' The actual file fetching is done by `fetch_files_serial`/
#' `fetch_files_parallel`, both of which accept a set of urls either as a
#' character vector or a list of `call` objects (see [base::call()]). This is
#' because file urls in openBIS have a limited lifetime and therefore must be
#' used shortly after being created. In case a download fails, it is retried
#' again up to the number of times specified as `n_try`. A vector of file sizes
#' may be passed which is used to make sure the file was downloaded entirely.
#' Finally, a function with a single argument can be passed as the argument
#' `done`, which takes the downloaded data as input and does some processing.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit search for datasets/files with.
#' @param ... Generic compatibility.
#' 
#' @rdname list_fetch_files
#' 
#' @export
#' 
list_files <- function(token, x, ...)
  UseMethod("list_files", x)

#' @rdname list_fetch_files
#' 
#' @param path A (vector of) file path(s) to be searched within a dataset.
#' @param recursive A (vector of) logicals, indicating whether to list files
#' recursively.
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("dsrg", "listFilesForDataSet")}
#' 
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

  if (max_length > 1L) {
    x <- check_rep(x, max_length)
    path <- check_rep(path, max_length)
    recursive <- check_rep(recursive, max_length)
  }

  params <- mapply(function(a, b, c) list(token, a, b, c), x, path, recursive,
                   SIMPLIFY = FALSE, USE.NAMES = FALSE)

  res <- make_requests(api_url("dsrg"), "listFilesForDataSet", params)

  res <- Map(function(dat, code) {
    attr(dat, "data_set") <- code
    dat
  }, unlist(res, recursive = FALSE), rep(x, sapply(res, length)))

  as_json_vec(do.call(c, res))
}

list_dataset_files <- function(token, x, path = "", recursive = TRUE, ...)
  list_files(token, dataset_code(x), path, recursive)

#' @rdname list_fetch_files
#' @export
#' 
list_files.DataSet <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.DatasetIdentifier <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.DatasetReference <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.FeatureVectorDatasetReference <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.FeatureVectorDatasetWellReference <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.ImageDatasetReference <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.MicroscopyImageReference <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.PlateImageReference <- list_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
list_files.DataSetFileDTO <- function(token, x, ...) {

  x <- as_json_vec(x)

  params <- lapply(x, function(y) list(token, y))

  res <- make_requests(api_url("dsrg"), "listFilesForDataSet", params)

  res <- Map(function(dat, code) {
    attr(dat, "data_set") <- code
    dat
  }, unlist(res, recursive = FALSE), rep(dataset_code(x), sapply(res, length)))

  as_json_vec(do.call(c, res))
}

#' @rdname list_fetch_files
#' 
#' @param files Optional set of `FileInfoDssDTO` objects. If NULL, all files
#' corresponding to the specified datasets are assumed.
#' @param file_regex Regular expression applied to filenames. 
#' 
#' @export
#' 
fetch_files <- function(token, x, ...)
  UseMethod("fetch_files", x)

fetch_dataset_files <- function(token,
                                x,
                                files = NULL,
                                file_regex = NULL,
                                ...) {

  if (is.null(files)) {
    files <- list_files(token, x, ...)
    files <- files[!sapply(files, `[[`, "isDirectory")]
  } else
    assert_that(has_subclass(files, "FileInfoDssDTO"),
                all(sapply(files, has_attr, "data_set")))

  if (!is.null(file_regex)) {
    assert_that(is.string(file_regex))
    files <- files[grepl(file_regex, sapply(files, `[[`, "pathInDataSet"))]
  }

  fetch_files(token, files, sapply(files, attr, "data_set"), ...)
}

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.character <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DataSet <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DatasetIdentifier <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DatasetReference <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.FeatureVectorDatasetReference <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.FeatureVectorDatasetWellReference <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.ImageDatasetReference <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.MicroscopyImageReference <- fetch_dataset_files

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.PlateImageReference <- fetch_dataset_files

#' @param n_con The number of simultaneous connections.
#' @param finally A function that is applied to the result of a successful
#' download.
#' 
#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DataSetFileDTO <- function(token,
                                       x,
                                       n_con = 5L,
                                       finally = identity,
                                       ...) {

  x <- as_json_vec(x)

  assert_that(length(n_con) == 1L, as.integer(n_con) == n_con)
  n_con <- min(as.integer(n_con), length(x))

  url_calls <- lapply(x, function(y) call("list_download_urls", token, y))

  file_sizes <- as.list(rep(NA, length(url_calls)))

  res <- if (length(url_calls) > 1L && n_con > 1L)
    do_requests_parallel(url_calls, file_sizes, n_con, 
                         chunked = TRUE,
                         create_handle = create_download_handle,
                         check = check_download_result,
                         finally = finally, ...)
  else
    do_requests_serial(url_calls, file_sizes,
                       create_handle = create_download_handle,
                       check = check_download_result,
                       finally = finally, ...)

  Map(function(dat, f) {
    attributes(dat) <- c(attributes(dat), list(file = f))
    dat
  }, res, x)
}

#' @param data_sets Either a single dataset object (anything that has a
#' `dataset_code()` method) or a set of objects of the same length as `x`.
#' 
#' @rdname list_fetch_files
#' @export
#' 
fetch_files.FileInfoDssDTO <- function(token,
                                       x,
                                       data_sets,
                                       n_con = 5L,
                                       finally = identity,
                                       ...) {

  x <- as_json_vec(x)

  assert_that(length(n_con) == 1L, as.integer(n_con) == n_con)
  n_con <- min(as.integer(n_con), length(x))

  if (!is.character(data_sets))
    data_sets <- dataset_code(data_sets)

  max_length <- max(length(x), length(data_sets))

  if (max_length > 1L) {

    if (length(x) == 1L)
      x <- rep(x, max_length)

    if (length(data_sets) == 1L)
      data_sets <- rep(data_sets, max_length)

    assert_that(length(x) == length(data_sets))
  }

  dirs <- sapply(x, `[[`, "isDirectory")
  if (any(dirs)) {
    warning("cannot fetch directories, dropping paths\n  ",
            paste(sapply(x[dirs], `[[`, "pathInDataSet"), collapse = "\n  "))
    x <- x[!dirs]
    data_sets <- data_sets[!dirs]
  }

  url_calls <- mapply(function(a, b) call("list_download_urls", token, a, b),
                      data_sets, sapply(x, `[[`, "pathInDataSet"),
                      SIMPLIFY = FALSE, USE.NAMES = FALSE)

  file_sizes <- lapply(x, `[[`, "fileSize")

  res <- if (length(url_calls) > 1L && n_con > 1L)
    do_requests_parallel(url_calls, file_sizes, n_con, 
                         chunked = TRUE,
                         create_handle = create_download_handle,
                         check = check_download_result,
                         finally = finally, ...)
  else
    do_requests_serial(url_calls, file_sizes,
                       create_handle = create_download_handle,
                       check = check_download_result,
                       finally = finally, ...)


  Map(function(dat, ds, f) {
    attributes(dat) <- c(attributes(dat), list(data_set = ds, file = f))
    dat
  }, res, data_sets, x)
}

create_download_handle <- function(size) {
  if (!is.na(size))
    assert_that(as.integer(size) == size)
  curl::new_handle()
}

check_download_result <- function(resp, size) {

  if (resp$status_code != 200) {

    warning("request returned with code ", resp$status_code)
    simpleError("retry")

  } else if (!is.na(size) && length(resp$content) != size) {

    warning("download incomplete: missing ", size - length(resp$content),
            " bytes")
    simpleError("retry")

  } else {

    resp$content
  }
}
