
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
#' The function `fetch_files()` downloads files specified by a set of
#' `FileInfoDssDTO` objects alongside one or several dataset objects.
#' 
#' @inheritParams logout_openbis
#' @param x Object to specify which files to download.
#' @param ... Generic compatibility.
#' 
#' @export
#' 
fetch_files <- function(token, x, ...)
  UseMethod("fetch_files", x)

#' @param data_sets Either a single dataset object (anything that has a
#' `dataset_code()` method) or a set of objects of the same length as `x`.
#' @param n_con The number of simultaneous connections.
#' 
#' @rdname fetch_files
#' @export
#' 
fetch_files.FileInfoDssDTO <- function(token,
                                       x,
                                       data_sets,
                                       n_con = 5L,
                                       ...) {

  assert_that(length(n_con) == 1L, as.integer(n_con) == n_con)
  n_con <- as.integer(n_con)

  x <- as_json_vec(x)

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

  mapply(function(a, b, c) list(data_set = b, file = c, data = a),
         res, data_sets, x, SIMPLIFY = FALSE, USE.NAMES = FALSE)
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
              length(n_try) == 1L, as.integer(n_try) == n_try)

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
        if (x$status_code != 200) {
          add_download(i)
        } else if (!is.na(sizes[i]) && length(x$content) != sizes[i]) {
          add_download(i)
        } else {
          add_download(i + n_con)
          res[[i]] <<- done(x$content)
          if (length(urls) > 1L && !is.na(sizes[i]) && sizes[i] > 0L)
            pb$tick(if (is.null(file_sizes)) 1L else sizes[i])
        }
      },
      fail = function(x) {
        message("fail: ", x)
        add_download(i)
      }
    )

    invisible(NULL)
  }

  assert_that(is.function(done),
              length(n_try) == 1L, as.integer(n_try) == n_try,
              length(n_con) == 1L, as.integer(n_con) == n_con)

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
