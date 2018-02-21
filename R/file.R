
#' List files
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

  if (max_length > 1L) {
    x <- check_rep(x, max_length)
    path <- check_rep(path, max_length)
    recursive <- check_rep(recursive, max_length)
  }

  res <- mapply(function(a, b, c) {
    request_openbis("listFilesForDataSet", list(token, a, b, c),
                    "IDssServiceRpcGeneric")
  }, x, path, recursive, SIMPLIFY = FALSE, USE.NAMES = FALSE)

  stats::setNames(as_json_vec(do.call(c, res)),
                  rep(x, sapply(res, length)))
}

list_dataset_files <- function(token, x, path = "", recursive = TRUE, ...)
  list_files(token, dataset_code(x), path, recursive)

#' @rdname list_files
#' @export
#' 
list_files.DataSet <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.DatasetIdentifier <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.DatasetReference <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.FeatureVectorDatasetReference <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.FeatureVectorDatasetWellReference <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.ImageDatasetReference <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.MicroscopyImageReference <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.PlateImageReference <- list_dataset_files

#' @rdname list_files
#' @export
#' 
list_files.DataSetFileDTO <- function(token, x, ...) {

  x <- as_json_vec(x)

  res <- lapply(x, function(y) {
    request_openbis("listFilesForDataSet", list(token, y),
                    "IDssServiceRpcGeneric")
  })

  stats::setNames(as_json_vec(do.call(c, res)),
                  rep(dataset_code(x), sapply(res, length)))
}

#' Fetch files
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
#' @param x Object to specify which files to download.
#' @param ... Generic compatibility. May be passed to [list_files()] and/or
#' `fetch_files_serial`/`fetch_files_parallel`.
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
                all(grepl("[0-9]+-[0-9]+", names(files))))

  if (!is.null(file_regex)) {
    assert_that(is.string(file_regex))
    files <- files[grepl(file_regex, sapply(files, `[[`, "pathInDataSet"))]
  }

  fetch_files(token, files, names(files), ...)
}

#' @rdname fetch_files
#' @export
#' 
fetch_files.character <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.DataSet <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.DatasetIdentifier <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.DatasetReference <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.FeatureVectorDatasetReference <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.FeatureVectorDatasetWellReference <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.ImageDatasetReference <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.MicroscopyImageReference <- fetch_dataset_files

#' @rdname fetch_files
#' @export
#' 
fetch_files.PlateImageReference <- fetch_dataset_files

#' @param n_con The number of simultaneous connections.
#' 
#' @rdname fetch_files
#' @export
#' 
fetch_files.DataSetFileDTO <- function(token,
                                       x,
                                       n_con = 5L,
                                       ...) {

  x <- as_json_vec(x)

  assert_that(length(n_con) == 1L, as.integer(n_con) == n_con)
  n_con <- min(as.integer(n_con), length(x))

  url_calls <- lapply(x, function(y) call("list_download_urls", token, y))

  res <- if (n_con <= 1L)
    fetch_files_serial(url_calls, ...)
  else
    fetch_files_parallel(url_calls, n_con = n_con, ...)

  mapply(list, file = x, data = res, SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

#' @param data_sets Either a single dataset object (anything that has a
#' `dataset_code()` method) or a set of objects of the same length as `x`.
#' 
#' @rdname fetch_files
#' @export
#' 
fetch_files.FileInfoDssDTO <- function(token,
                                       x,
                                       data_sets,
                                       n_con = 5L,
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

  res <- if (n_con <= 1L)
    fetch_files_serial(url_calls,
                       file_sizes = sapply(x, `[[`, "fileSize"),
                       ...)
  else
    fetch_files_parallel(url_calls,
                         file_sizes = sapply(x, `[[`, "fileSize"),
                         n_con = n_con,
                         ...)

  mapply(list, data_set = data_sets, file = x, data = res,
         SIMPLIFY = FALSE, USE.NAMES = FALSE)
}

#' @param urls Either a caracter vector or a list of calls that each yields an
#' url when `eval`d.
#' @param n_try The number of tries for each url.
#' @param file_sizes A vector of expected file sizes or NULL.
#' @param done A function with a single argument which is applied to each
#' downloaded file.
#' 
#' @rdname fetch_files
#' @export
#' 
fetch_files_serial <- function(urls,
                               n_try = 2L,
                               file_sizes = NULL,
                               done = identity,
                               ...) {

  assert_that(is.function(done),
              length(n_try) == 1L, as.integer(n_try) == n_try,
              all(is.character(urls) | sapply(urls, is.call)))

  if (is.null(file_sizes))
    sizes <- rep(NA, length(urls))
  else {
    assert_that(all(as.integer(file_sizes) == file_sizes),
                length(file_sizes) == length(urls))
    sizes <- as.integer(file_sizes)
  }

  if (length(urls) > 1L) {
    tot <- if (is.null(file_sizes))
      length(urls)
    else
      sum(sizes, na.rm = TRUE)

    pb <- progress::progress_bar$new(
      format = paste0("downloading [:bar] :percent in :elapsed"),
      total = tot)

    pb$tick(0)
  }

  res <- vector("list", length(urls))

  repeat {

    to_do <- sapply(res, is.null)
    if (sum(to_do) == 0L)
      break

    n_try <- n_try - 1L
    if (n_try < 0L)
      stop("data could not be fetched successfully.")

    res[to_do] <- mapply(function(a, b) {
      resp <- curl::curl_fetch_memory(eval(a))
      if (resp$status_code != 200)
        NULL
      else if (!is.na(b) && length(resp$content) != b)
        NULL
      else {
        resp <- done(resp$content)
        if (length(urls) > 1L && !is.na(b) && b > 0L)
          pb$tick(if (is.null(file_sizes)) 1L else b)
        resp
      }
    }, urls[to_do], sizes[to_do], SIMPLIFY = FALSE, USE.NAMES = FALSE)
  }

  assert_that(all(sapply(res, Negate(is.null))))

  res
}

#' @rdname fetch_files
#' @export
#' 
fetch_files_parallel <- function(urls,
                                 n_try = 2L,
                                 file_sizes = NULL,
                                 done = identity,
                                 n_con = 5L,
                                 ...) {

  add_download <- function(i) {

    if (i < 1L || i > length(urls))
      return(invisible(NULL))

    tries[i] <<- tries[i] - 1L
    if (tries[i] < 0L)
      stop("could not download file within ", n_try, " tries.")

    curl::curl_fetch_multi(
      url = eval(urls[[i]]),
      pool = pool,
      done = function(x) {
        if (x$status_code != 200)
          add_download(i)
        else if (!is.na(sizes[i]) && length(x$content) != sizes[i])
          add_download(i)
        else {
          add_download(i + n_con)
          res[[i]] <<- done(x$content)
          if (length(urls) > 1L && !is.na(sizes[i]) && sizes[i] > 0L)
            pb$tick(if (is.null(file_sizes)) 1L else sizes[i])
        }
      },
      fail = function(x) add_download(i)
    )

    invisible(NULL)
  }

  assert_that(is.function(done),
              length(n_try) == 1L, as.integer(n_try) == n_try,
              length(n_con) == 1L, as.integer(n_con) == n_con,
              all(is.character(urls) | sapply(urls, is.call)))

  if (is.null(file_sizes))
    sizes <- rep(NA, length(urls))
  else {
    assert_that(all(as.integer(file_sizes) == file_sizes),
                length(file_sizes) == length(urls))
    sizes <- as.integer(file_sizes)
  }

  if (length(urls) > 1L) {
    tot <- if (is.null(file_sizes))
      length(urls)
    else
      sum(sizes, na.rm = TRUE)

    pb <- progress::progress_bar$new(
      format = paste0("downloading [:bar] :percent in :elapsed"),
      total = tot)

    pb$tick(0)
  }

  res <- vector("list", length(urls))
  tries <- rep(n_try, length(urls))

  pool <- curl::new_pool(host_con = n_con)

  for (j in seq.int(n_con))
    add_download(j)

  curl::multi_run(pool = pool)

  assert_that(all(sapply(res, Negate(is.null))))

  res
}
