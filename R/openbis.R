
#' @title Fetch datasets from an openBis instance
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
#' @export
#' 
fetch_openbis <- function(username = "rdgr2014",
                          password = "IXPubReview",
                          data_type = NULL,
                          data_id = NULL,
                          plate_regex = NULL,
                          file_regex = ".*",
                          out_dir = getwd(),
                          result_class = "stable",
                          newest = TRUE,
                          verbosity = 6) {

  success <- function(x) {
    x[4] == "Login successful" &&
      x[length(x)] == paste0("Module BeeDataSetDownloader ",
                             "finished successfully")
  }

  try_call <- function(x) {
    ret <- suppressWarnings(system2(command = "java", args = x,
                                    stdout = TRUE, stderr = TRUE))
    if (!is.null(attr(ret, "status")) && attr(ret, "status") != 0) {
      message("")
      stop(paste(ret, collapse = "\n"))
    }
    ret
  }

  # input validation
  if (!is.null(result_class) && !result_class %in% c("stable"))
    warning("currently the only supported result class is \"stable\".")
  verbosity <- as.integer(verbosity)
  stopifnot(verbosity <= 25 & verbosity >= 0)

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
  ret <- try_call(arguments)
  if (!success(ret))
    ret <- try_call(arguments)
  stopifnot(success(ret))
  message(" done.")

  ret
}

#' @title Fetch single cell data
#'
#' @description Using [fetch_openbis()], this function downloads single cell
#' datasets corresponding to a plate barcode and filtered by an optional file
#' name regular expression. The downloaded data is either saved to a user
#' specified directory or to a temporary location which can be auto removed.
#' 
#' @param plate_name The plate barcode of interest.
#' @param auto_rm A logical switch; if TRUE, all downloaded files will be
#' removed upon garbage collection of the returned character vector.
#' @param ... Arguments passed to [fetch_openbis()]
#' 
#' @return A vector of downloaded filenames.
#' 
#' @export
#' 
fetch_plate <- function(plate_name,
                        auto_rm = TRUE,
                        ...) {

  dots <- list(...)

  stopifnot(is.null(dots$plate_regex))
  dots$plate_regex <- paste0("^/.*/.*/.*/", plate_name, "$")

  if (!is.null(dots$data_type))
    warning("ignoring the argument \"data_type\".")
  dots$data_type <- "HCS_ANALYSIS_CELL_FEATURES_CC_MAT"

  if (is.null(dots$file_regex))
    dots$file_regex <- ".*\\.mat$"
  if (is.null(dots$out_dir))
    dots$out_dir <- tempfile()

  if (dir.exists(dots$out_dir)) {
    files <- list.files(dots$out_dir, pattern = dots$file_regex,
                        recursive = TRUE)
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

  out <- do.call(fetch_openbis, dots)

  files <- out[grepl("^The download '.+' does not exist, will download.$",
                     out)]
  stopifnot(length(files) >= 1)
  files <- gsub("The download '", "",
                gsub("' does not exist, will download.$", "", files))

  stopifnot(all(sapply(files, file.exists)))

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
