
#' @title Helper querying openBis API
#'
#' @description Issues a POST request to the JSON-RPC based openBis API v1.
#' Documentation is available at \code{https://wiki-bsse.ethz.ch/display/
#' openBISDoc/openBIS+JSON+API}.
#' 
#' @param method The method name
#' @param params A list structure holding the arguments which, converted to
#' JSON, will be used to call the supplied method.
#' @param api The location of the JS libraries handling the request. Is
#' appended to the supplied url.
#' @param url The base url, the request is sent to.
#' 
#' @return A list/data.frame holding the response from openBis.
#' 
do_openbis <- function(method,
                       params,
                       api = "openbis/openbis/rmi-general-information-v1.json",
                       url = "https://infectx.biozentrum.unibas.ch") {

  req <- list(id = "1",
              jsonrpc = "2.0",
              method = method,
              params = params)

  res <- httr::POST(paste(url, api, sep = "/"), body = req, encode = "json")

  assert_that(res$status_code == 200)

  res$content <- jsonlite::fromJSON(rawToChar(res$content))

  if (!is.null(res$content$error))
    stop("Error:\n", paste(names(res$content$error), res$content$error,
                           sep = ": ", collapse = "\n"))
  else
    res$content$result
}

#' @title Generate a login token
#'
#' @description Create a login token for openBis API calls. Upon garbage
#' collection of the token, the user is logged out.
#' 
#' @param user,pwd Login credentials for an openBis instance.
#' @param auto_disconnect Logical switch for automatically closing the
#' connection upon garbage collection of the token.
#' @param ... Further arguments are passed to [do_openbis].
#' 
#' @return The login token to be used for further API interactions.
#' 
#' @export
#' 
login_openbis <- function(user,
                          pwd,
                          auto_disconnect = TRUE,
                          ...) {

  disco <- function(tok, ...) {
    dots <- list(...)
    reg.finalizer(environment(),
                  function(...) do.call(logout_openbis, c(tok, dots)),
                  onexit = TRUE)
    environment()
  }

  token <- do_openbis("tryToAuthenticateForAllServices", list(user, pwd), ...)

  assert_that(!is.null(token), msg = "Login failed.")

  if (auto_disconnect)  {
    attr(token, "finaliser") <- disco(token, ...)
  }

  token
}

#' @title Logout from openBis
#'
#' @description Using a token as created by [login_openbis], the corresponding
#' session is closed and the token is rendered invalid.
#' 
#' @param token Login token as created by [login_openbis].
#' @param ... Further arguments are passed to [do_openbis].
#' 
#' @return NULL (invisibly)
#' 
#' @export
#' 
logout_openbis <- function(token, ...)
  invisible(do_openbis("logout", list(token), ...))

#' @title Check validity of token
#'
#' @description A token as created by [login_openbis] is tested for its
#' validity.
#' 
#' @param token Login token as created by [login_openbis].
#' @param ... Further arguments are passed to [do_openbis].
#' 
#' @return Scalar logical.
#' 
#' @export
#' 
is_token_valid <- function(token, ...)
  do_openbis("isSessionActive", list(token), ...)


#' @title Download datasets from an openBis instance
#'
#' @description This is a wrapper for the BeeDataSetDownloader.jar application
#' supplied by the package, responsible for downloading data from the InfectX
#' openBis instance.
#' 
#' @param username,password OpneBis login credentials
#' @param data_type Specify the type of dataset to be downloaded
#' @param data_id If applicable, for example for a single aggregate file,
#' directly select the single dataset to download (optional)
#' @param plate_regex If datasets corresponding to one or several plates are
#' targeted, they can be selected via a regular expression (optional)
#' @param file_regex A regular expression applied to filenames can be used to
#' filter the selection (optional)
#' @param out_dir Directory used to save the downloaded data to (default: the
#' current working dir)
#' @param result_class A string, indicating the result class to restrict to.
#' NULL to skip.
#' @param newest Logical, indicating whether to only return the newest
#' version as a matched dataset per plate (default is TRUE).
#' @param verbosity The verbosity level of BeeDataSetDownloader (an integer in
#' [0-25]) 
#' 
#' @return A character vector holding all std output of BeeDataSetDownloader at
#' the specified verbosity level.
#' 
fetch_openbis <- function(username, password,
                          data_type = NULL,
                          data_id = NULL,
                          plate_regex = NULL,
                          file_regex = ".*",
                          out_dir = getwd(),
                          result_class = "stable",
                          newest = TRUE,
                          verbosity = 6) {

  # input validation
  verbosity <- as.integer(verbosity)
  stopifnot(is.character(username), length(username) == 1,
            is.character(password), length(password) == 1,
            is.character(file_regex), length(file_regex) == 1,
            length(out_dir) == 1, dir.exists(out_dir),
            is.logical(newest), length(newest) == 1,
            length(verbosity) == 1, verbosity <= 25 & verbosity >= 0)
  if (!is.null(result_class) && !result_class %in% c("stable"))
    stop("currently the only supported result class is \"stable\".")

  jarloc <- system.file("java", "openBisDownloader.jar",
                        package = utils::packageName())

  arguments <- c(
    "-jar", jarloc,
    "--verbose", paste0("'", verbosity, "'"),
    "--user", paste0("'", username, "'"),
    "--password", paste0("'", password, "'"),
    if (!is.null(data_type))
      "--type", paste0("'", data_type, "'"),
    if (!is.null(result_class))
      c("--result-class", paste0("'", result_class, "'")),
    if (!is.null(data_id))
      c("--datasetid", paste0("'", data_id, "'")),
    if (!is.null(plate_regex))
      c("--plateid", paste0("'", plate_regex, "'")),
    if (newest)
      "--newest",
    "--files", paste0("'", file_regex, "'"),
    "--outputdir", paste0("'", out_dir, "'"))

  message("Fetching data from openBIS...", appendLF = FALSE)

  ret <- suppressWarnings(system2(command = "java", args = arguments,
                                  stdout = TRUE, stderr = TRUE))

  if ( (!is.null(attr(ret, "status")) && attr(ret, "status") != 0) ||
       sum(grepl("Login successful", ret, fixed = TRUE)) != 1L ||
       sum(grepl("Module BeeDataSetDownloader finished successfully", ret,
                 fixed = TRUE)) != 1L) {
    message("")
    stop(paste(ret, collapse = "\n"))
  }

  message(" done.")

  ret
}

#' @title Fetch data from openBis.
#'
#' @description Calling [fetch_openbis()], this function downloads data and
#' returns the downloaded filenames. It optionally makes sure the data is
#' removed again upon garbage collection of the returned filename reference.
#' 
#' @param auto_rm A logical switch; if TRUE, all downloaded files will be
#' removed upon garbage collection of the returned character vector.
#' @param ... Arguments passed to [fetch_openbis()]
#' 
#' @return A vector of downloaded filenames.
#' 
#' @export
#' 
fetch_data <- function(auto_rm = TRUE,
                       ...) {

  dots <- list(...)

  if (!is.null(dots$verbosity))
    warning("ignoring the argument \"verbosity\".")
  dots$verbosity <- 6

  if (is.null(dots$out_dir))
    dots$out_dir <- tempfile()

  if (dir.exists(dots$out_dir)) {
    files <- list.files(dots$out_dir, recursive = TRUE)
    if (length(files) > 1) {
      message("found ", length(files), " files under\n  ",
              normalizePath(dots$out_dir), "\n  ",
              "please remove/change cacheDir if this is not what is desired.")
      return(files)
    } else {
      stop("could not find any files under\n  ", normalizePath(dots$out_dir),
           "\n  please remove/change cacheDir.")
    }
  }

  dir.create(dots$out_dir)

  do.call(fetch_openbis, dots)

  files <- list.files(dots$out_dir, recursive = TRUE, full.names = TRUE)

  stopifnot(length(files) >= 1)

  if (auto_rm) {
    attr(files, "finaliser") <- (function(dir) {
      reg.finalizer(environment(), function(...) {
        message("Auto-removing downloaded files from\n", dir)
        unlink(dir, recursive = TRUE)
      }, onexit = TRUE)
      environment()
    })(dots$out_dir)
  }

  files
}


#' @title Fetch single cell data
#'
#' @description Calling [fetch_openbis()], this function downloads single cell
#' datasets corresponding to a plate barcode and filtered by an optional file
#' name regular expression.
#' 
#' @param plate_name The plate barcode of interest.
#' @param ... Arguments passed to [fetch_data()]
#' 
#' @return A vector of downloaded filenames.
#' 
#' @export
#' 
fetch_plate <- function(plate_name,
                        ...) {

  dots <- list(...)

  if (!is.null(dots$plate_regex))
    warning("ignoring the argument \"plate_regex\".")
  dots$plate_regex <- paste0("^/.*/.*/.*/", plate_name, "$")

  if (!is.null(dots$data_type))
    warning("ignoring the argument \"data_type\".")
  dots$data_type <- "HCS_ANALYSIS_CELL_FEATURES_CC_MAT"

  if (is.null(dots$file_regex))
    dots$file_regex <- ".*\\.mat$"

  do.call(fetch_data, dots)
}

#' @title Fetch InfectX meta data
#'
#' @description A wrapper around [fetch_data()], this function essentially
#' provides default values for downloading meta data from openBis. Two formats
#' of meta data are currently available: dumps of SQLite databased holding the
#' entire set of experimental meta data, as well as a CSV sheet, containing
#' only the published subset.
#' 
#' @param type A switch for the type of meta data to be downloaded.
#' @param ... Passed to [fetch_data()].
#' 
#' @return A vector of downloaded filenames
#' 
#' @export
#' 
fetch_meta <- function(type = c("full", "public"),
                       ...) {

  type <- match.arg(type)
  dots <- list(...)

  if (!is.null(dots$data_type))
    warning("ignoring the argument \"data_type\".")
  dots$data_type <- "HCS_ANALYSIS_WELL_REPORT_CSV"

  if (type == "full") {

    if (!is.null(dots$file_regex))
      warning("ignoring the argument \"file_regex\".")
    dots$file_regex <- ".*.tsv.gz$"

    if (!is.null(dots$plate_regex))
      warning("ignoring the argument \"plate_regex\".")
    dots$plate_regex <- "/INFECTX/_COMMON/REPORTS/DUMMYSTORAGEFORREPORTS"

  } else {

    if (!is.null(dots$data_id))
      warning("ignoring the argument \"data_id\".")
    dots$data_id <- "20140609103658114-3045667"
  }

  do.call(fetch_data, dots)
}
