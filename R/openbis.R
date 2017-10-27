
#' @title Fetch datasets from an openBis instance
#'
#' @description This is a wrapper for the BeeDataSetDownloader.jar application
#' supplied by the package, responsible for downloading data from the InfectX
#' openBis instance.
#' 
#' @param username,password OpneBis login credentials
#' @param data_type Sepcify the type of dataset to be downloaded
#' @param data_id If applicable, for example for a single aggregate file,
#' directly select the single dataset to download (optional)
#' @param plate_id If datasets corresponding to one or several plates are
#' targeted, they can be selected via a regular expression (optional)
#' @param file_regex A regular expression appled to filenames can be used to
#' filter the selection (optional)
#' @param out_dir Directory used to save the downloaded data to (default: the
#' current working dir)
#' @param result_class A string, indicating the result class to restrict to.
#' NULL to skip.
#' @param newest Logical, indicating whether to only return the newest
#' version af a matched dataset per plate (default is TRUE).
#' @param verbosity The verbosity level of BeeDataSetDownloader (an integer in
#' [0-25]) 
#' 
#' @return A character vector holding all std output of BeeDataSetDownloader at
#' the specified verbosity level.
#' 
fetch_obis <- function(username = rdgr2014,
                       password = IXPubReview,
                       data_type,
                       data_id = NULL,
                       plate_id = NULL,
                       file_regex = ".*",
                       out_dir = getwd(),
                       result_class = "stable",
                       newest = TRUE,
                       verbosity = 6) {

  # input validation
  if (!is.null(result_class) && !result_class %in% c("stable"))
    warning("currently the only supported result class is \"stable\".")
  if (is.null(data_id) & is.null(plate_id))
    stop("either \"data_id\" or \"plate_id\" have to be not NULL")
  verbosity <- as.integer(verbosity)
  stopifnot(verbosity <= 25 & verbosity >= 0)

  jarloc <- system.file("java", "openBisDownloader.jar",
                        package = packageName())

  arguments <- c(
    "-jar", jarloc,
    "--verbose", paste0("'", verbosity, "'"),
    "--user", paste0("'", username, "'"),
    "--password", paste0("'", password, "'"),
    "--type", paste0("'", data_type, "'"),
    if (!is.null(result_class))
      c("--result-class", paste0("'", result_class, "'")),
    if (!is.null(data_id)) c("--datasetid", paste0("'", data_id, "'")),
    if (!is.null(plate_id)) c("--plateid", paste0("'", plate_id, "'")),
    if (newest) "--newest",
    "--files", paste0("'", file_regex, "'"),
    "--outputdir", paste0("'", out_dir, "'"))

  message("Fetching data from openBIS...", appendLF = FALSE)
  ret <- system2(command = "java", args = arguments, stdout = TRUE,
                 stderr = TRUE)
  message(" done.")
  ret
}
