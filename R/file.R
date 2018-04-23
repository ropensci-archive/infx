
#' List and download files
#' 
#' A dataset in openBIS represents a collection of files. The function
#' `list_files()` lists files associated with one or more datasets by
#' returning a set of `FileInfoDssDTO` objects. As this object type does not
#' contain information on data set association, the data set code is saved
#' as `data_set` attribute with each `FileInfoDssDTO` object. Data set files
#' can be fetched using `fetch_files()`, which can either retrieve all
#' associated files or use file path information, for example from
#' `FileInfoDssDTO` objects to only download a subset of files.
#' 
#' Data sets for `list_files()` can be specified as character vector of
#' dataset codes and therefore all objects for which the internal method
#' [dataset_code()] exists can be used to select datasets. This includes data
#' set and data set id objects as well as the various flavors of data set
#' reference objects. In addition to these dataset-representing objects,
#' dispatch on `DataSetFileDTO` objects is possible as well.
#' 
#' File listing can be limited to a certain path within the dataset and the
#' search can be carried out recursively or non-recursively. In case a set of
#' objects is passed, the search-tuning arguments `path` and `recursive` have
#' to be either of length 1 or of the same length as `x`. If dispatch occurs
#' on `DataSetFileDTO` objects, the `path` and `recursive` arguments are not
#' needed, as this information is already encoded in the objects passed as `x`.
#' A separate API call is necessary for each of the objects the dispatch
#' occurs on. 
#' 
#' The function `fetch_files()` downloads files associated with a dataset.
#' In order to identify a file, both a data set code and a file path, relative
#' to the data set root, are required. `fetch_files()` can be called in a
#' variety of ways and internally uses a double dispatch mechanism, first
#' resolving the data set codes and then calling the non-exported function
#' `fetch_ds_files()` which dispatches on file path objects.
#' 
#' Data set code information can either be communicated using any of the
#' objects understood by [dataset_code()] (including data set, data set id and
#' data set reference objects) or directly as a character vector, passed as
#' `x` argument. In case data set code information is omitted (passed to `x`
#' as `NULL`), the objects encoding file paths have to specify the
#' corresponding data sets. Furthermore, `DataSetFileDTO` objects may be
#' passed as `x` argument to `fetch_files()`, which will internally call
#' `fetch_files()` again, setting the argument `x` to `NULL` and pass the
#' `DataSetFileDTO` objects as files argument. Finally, if `FileInfoDssDTO`
#' are passed to `fetch_files()` as `x` argument, an optional argument
#' `data_sets` may be specified (it defaults to `NULL`) and as above,
#' `fetch_files()` is called again with these two arguments rearranged.
#' 
#' The internal generic function `fetch_ds_files()` can be dispatched on
#' several objects again. When no files are specified (`NULL` is passed as
#' `files` argument to `fetch_files()`), All available files for the given
#' data sets are queried. This list can be filtered using the `file_regex()`
#' argument which can be a single regular expression and is applied to file
#' paths. File paths can be specified as character vector, `FileInfoDssDTO` or
#' `DataSetFileDTO` objects. If dispatch occurs on `FileInfoDssDTO`, and no
#' data set code information is available (`NULL` passed as `x` or `data_sets`
#' argument to `fetch_files()`) each `FileInfoDssDTO` must contain a `data_set`
#' attribute. Additionally, downloaded files are checked for completeness, as
#' these objects contain file sizes. If dispatch occurs on `DataSetFileDTO`
#' objects or a character vector, this sanity check is not possible. 
#' 
#' Files can only be retrieved after previously having created a corresponding
#' download url using [list_download_urls()]. As file urls in openBIS have a
#' limited lifetime and therefore must be used shortly after being created. A
#' list of `call` objects (see [base::call()]) is created and passed to either
#' [do_requests_serial()] or [do_requests_parallel()]. Whether file fetching
#' is carried out in serial or parallel is controlled by the `n_con` argument.
#' In case a download fails, it is retried again up to the number of times
#' specified as `n_try`. Finally, a function with a single argument can be
#' passed as the argument `done`, which takes the downloaded data as input and
#' does some processing.
#' 
#' A function for reading the binary data retrieved from openBIS can be
#' supplied to `fetch_files()` as `reader` argument. Single cell feature files
#' as produced by CellProfiler, are stored as Matlab v5.0 `.mat` files and
#' the function `read_mat_files()` reads such files using [R.matlab::readMat()]
#' and checks for certain expected attributes and simplifies the read
#' structure.
#' 
#' The list returned by `read_mat_files()` is arranged such that each node
#' corresponds to a single image and contains a list which is either holding a
#' single value or a vector of values. For a plate with 16 rows, 24 columns
#' and 3 x 3 imaging sites this will yield a list of length 3456. Index
#' linearization is in row-major fashion for both wells and sites.
#' Furthermore, imaging sites come first such that in this example, the first
#' three list entries correspond to image row 1 (left to right) of well A1,
#' the next three entries correspond to row 2 of well A1, images 10 through 12
#' correspond to row 1 of well A2, etc. Well A2 is located in row 1, column 2
#' of a plate.
#' 
#' @inheritParams logout_openbis
#' @param x Object to limit search for datasets/files with.
#' @param ... Generic compatibility. Extra arguments will be passed to
#' [make_requests()] or [do_requests_serial()]/[do_requests_parallel()].
#' 
#' @rdname list_fetch_files
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis("rdgr2014", "IXPubReview")
#' 
#'   # search for a cell profiler feature data set from plate KB2-03-1I
#'   search <- search_criteria(
#'     attribute_clause("type", "HCS_ANALYSIS_CELL_FEATURES_CC_MAT"),
#'     sub_criteria = search_sub_criteria(
#'       search_criteria(attribute_clause("code",
#'                                        "/INFECTX_PUBLISHED/KB2-03-1I")),
#'       type = "sample"
#'     )
#'   )
#'   ds <- search_openbis(tok, search)
#' 
#'   # list all files of this data set
#'   all_files <- list_files(tok, ds)
#'   length(all_files)
#' 
#'   # select some of the files, e.g. all count features per image
#'   some_files <- all_files[grepl("Image\\.Count_",
#'                                 get_field(all_files, "pathInDataSet"))]
#'   length(some_files)
#' 
#'   # download the selected files
#'   data <- fetch_files(tok, some_files)
#' 
#'   # the same can be achieved by passing a file_regex argument to
#'   # fetch_files(), which internally calls list_files() and filters files
#'   identical(data, fetch_files(tok, ds, file_regex = "Image\\.Count_"))
#' 
#'   # all returned data is raw, the reader argument can be used to supply
#'   # a function that processes the downloaded data
#'   sapply(data, class)
#'   data <- fetch_files(tok, some_files, reader = read_mat_files)
#'   sapply(data, class)
#' 
#'   logout_openbis(tok)
#' }
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

  res <- make_requests(api_url("dsrg", attr(token, "host_url"), ...),
                       "listFilesForDataSet",
                       params,
                       ...)

  as_json_vec(
    Map(set_attr,
        unlist(res, recursive = FALSE),
        rep(x, sapply(res, length)),
        MoreArgs = list(attr_name = "data_set")),
    simplify = TRUE
  )
}

list_dataset_files <- function(token, x, path = "", recursive = TRUE, ...)
  list_files(token, dataset_code(x), path, recursive, ...)

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

  res <- make_requests(api_url("dsrg", attr(token, "host_url"), ...),
                       "listFilesForDataSet",
                       params,
                       ...)

  as_json_vec(
    Map(set_attr,
        unlist(res, recursive = FALSE),
        rep(dataset_code(x), sapply(res, length)),
        MoreArgs = list(attr_name = "data_set")),
    simplify = TRUE
  )
}

#' @rdname list_fetch_files
#' @export
#' 
fetch_files <- function(token, x, ...)
  UseMethod("fetch_files", x)

#' @param files Optional set of `FileInfoDssDTO` objects. If NULL, all files
#' corresponding to the specified datasets are assumed. This file list can be
#' filtered, by passing a regular expression as `file_regex` argument via
#' `...`.
#' @param n_con The number of simultaneous connections.
#' @param reader A function to read the downloaded data. Is forwarded as
#' finally argument to [do_requests_serial()]/[do_requests_parallel()].
#' 
#' @rdname list_fetch_files
#' @export
#' 
fetch_files.character <- function(token,
                                  x,
                                  files = NULL,
                                  n_con = 5L,
                                  reader = identity,
                                  ...) {

  assert_that(length(n_con) == 1L, as.integer(n_con) == n_con)

  if (!is.null(files) && length(files) != length(x)) {

    max_len <- max(length(files), length(x))

    if (max_len > 1L) {
      if (length(files) == 1L)
        files <- rep(files, max_len)
      if (length(x) == 1L)
        x <- rep(x, max_len)
    }

    assert_that(length(x) == length(files))
  }

  n_con <- min(as.integer(n_con), length(x))

  fetch_ds_files(token, files, data_sets = x, n_con = n_con,
                 reader = reader, ...)
}

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.NULL <- function(token,
                             x,
                             files,
                             n_con = 5L,
                             reader = identity,
                             ...) {

  assert_that(length(n_con) == 1L, as.integer(n_con) == n_con)
  n_con <- min(as.integer(n_con), length(files))

  fetch_ds_files(token, files, n_con = n_con, reader = reader, ...)
}

fetch_ds <- function(token, x, ...)
  fetch_files(token, dataset_code(x), ...)

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DataSet <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DatasetIdentifier <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DatasetReference <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.FeatureVectorDatasetReference <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.FeatureVectorDatasetWellReference <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.ImageDatasetReference <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.MicroscopyImageReference <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.PlateImageReference <- fetch_ds

#' @rdname list_fetch_files
#' @export
#' 
fetch_files.DataSetFileDTO <- function(token, x, ...)
  fetch_files(token, NULL, x, ...)

#' @param data_sets Either a single dataset object (anything that has a
#' `dataset_code()` method) or a set of objects of the same length as `x`. If
#' `NULL` (default), each `FileInfoDssDTO` object passed as `x` is expected
#' to contain a `data_set` attribute.
#' 
#' @rdname list_fetch_files
#' @export
#' 
fetch_files.FileInfoDssDTO <- function(token, x, data_sets = NULL, ...)
  fetch_files(token, data_sets, x, ...)

fetch_ds_files <- function(token, x, ...)
  UseMethod("fetch_ds_files", x)

fetch_ds_files.NULL <- function(token,
                                x,
                                data_sets,
                                file_regex = NULL,
                                ...) {

  files <- list_files(token, data_sets, ...)
  files <- files[!get_field(files, "isDirectory")]

  if (!is.null(file_regex)) {
    assert_that(is.string(file_regex))
    files <- files[grepl(file_regex, get_field(files, "pathInDataSet"))]
  }

  fetch_files(token, sapply(files, attr, "data_set"), files, ...)
}

fetch_ds_files.character <- function(token,
                                     x,
                                     data_sets,
                                     n_con,
                                     reader,
                                     ...) {

  assert_that(is.character(data_sets),
              length(data_sets) == length(x))

  url_calls <- Map(function(ds, path)
                     as.call(list(list_download_urls, token, ds, path, ...)),
                   data_sets, x)

  file_sizes <- as.list(rep(NA, length(url_calls)))

  res <- if (length(url_calls) > 1L && n_con > 1L)
    do_requests_parallel(url_calls, file_sizes, n_con, 
                         create_handle = create_file_handle,
                         check = check_file_result,
                         finally = reader,
                         ...)
  else
    do_requests_serial(url_calls, file_sizes,
                       create_handle = create_file_handle,
                       check = check_file_result,
                       finally = reader,
                       ...)

  Map(function(dat, ds, f) {
    attributes(dat) <- c(attributes(dat), list(data_set = ds, file = f))
    dat
  }, res, data_sets, x)
}

fetch_ds_files.DataSetFileDTO <- function(token,
                                          x,
                                          n_con,
                                          reader,
                                          ...) {

  x <- as_json_vec(x)

  url_calls <- lapply(x, function(y)
                           as.call(list(list_download_urls, token, y, ...)))

  file_sizes <- as.list(rep(NA, length(url_calls)))

  res <- if (length(url_calls) > 1L && n_con > 1L)
    do_requests_parallel(url_calls, file_sizes, n_con, 
                         create_handle = create_file_handle,
                         check = check_file_result,
                         finally = reader,
                         ...)
  else
    do_requests_serial(url_calls, file_sizes,
                       create_handle = create_file_handle,
                       check = check_file_result,
                       finally = reader,
                       ...)

  Map(function(dat, f) {
    attributes(dat) <- c(attributes(dat), list(file = f))
    dat
  }, res, x)
}

fetch_ds_files.FileInfoDssDTO <- function(token,
                                          x,
                                          data_sets = NULL,
                                          n_con,
                                          reader,
                                          ...) {

  x <- as_json_vec(x)

  if (is.null(data_sets)) {
    assert_that(all(sapply(x, has_attr, "data_set")))
    data_sets <- sapply(x, attr, "data_set")
  }

  assert_that(is.character(data_sets),
              length(data_sets) == length(x))

  dirs <- get_field(x, "isDirectory")
  if (any(dirs)) {
    warning("cannot fetch directories, dropping paths\n  ",
            paste(get_field(x[dirs], "pathInDataSet"), collapse = "\n  "))
    x <- x[!dirs]
    data_sets <- data_sets[!dirs]
  }

  url_calls <- Map(function(a, b)
                     as.call(list(list_download_urls, token, a, b, ...)),
                   data_sets, get_field(x, "pathInDataSet"))

  file_sizes <- get_field(x, "fileSize")

  res <- if (length(url_calls) > 1L && n_con > 1L)
    do_requests_parallel(url_calls, file_sizes, n_con, 
                         create_handle = create_file_handle,
                         check = check_file_result,
                         finally = reader,
                         ...)
  else
    do_requests_serial(url_calls, file_sizes,
                       create_handle = create_file_handle,
                       check = check_file_result,
                       finally = reader,
                       ...)

  Map(function(dat, ds, f) {
    attributes(dat) <- c(attributes(dat), list(data_set = ds, file = f))
    dat
  }, res, data_sets, x)
}

create_file_handle <- function(size) {
  if (!is.na(size))
    assert_that(as.integer(size) == size)
  curl::new_handle()
}

check_file_result <- function(resp, size) {

  if (resp$status_code != 200) {

    simpleError(paste0("request returned with code ", resp$status_code))

  } else if (!is.na(size) && length(resp$content) != size) {

    simpleError(paste0("download incomplete: missing ",
                       size - length(resp$content), " bytes"))

  } else {

    resp$content
  }
}

#' @param data The data to be read.
#' 
#' @rdname list_fetch_files
#' @export
#' 
read_mat_files <- function(data) {

  reduce_nesting <- function(x) {
    if (is.list(x) && length(x) == 1L)
      x <- x[[1L]]
    if (is.list(x))
      lapply(x, reduce_nesting)
    else
      drop(x)
  }

  extract_data <- function(x, y = character()) {
    if (is.list(x) && length(x) == 1L &&
        has_attr(x, "dim") && has_attr(x, "dimnames"))
      extract_data(x[[1L]], c(y, unlist(attr(x, "dimnames"))))
    else {
      x <- reduce_nesting(x)
      info <- setdiff(y, "Measurements")
      assert_that(length(info) == 2L)
      attr(x, "object") <- info[1L]
      attr(x, "feature") <- info[2L]
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
