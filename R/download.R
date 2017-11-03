
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
#' @inheritParams logout_openbis
#' 
#' @return Scalar logical.
#' 
#' @export
#' 
is_token_valid <- function(token, ...)
  do_openbis("isSessionActive", list(token), ...)

#' @title List plates
#'
#' @description For a login token, list all available plates.
#' 
#' @inheritParams logout_openbis
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_plates <- function(token, ...)
  do_openbis("listPlates", list(token),
             "openbis/openbis/rmi-screening-api-v1.json", ...)

#' @title List projects
#'
#' @description For a login token, list all available projects.
#' 
#' @inheritParams logout_openbis
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_projects <- function(token, ...)
  do_openbis("listProjects", list(token), ...)

#' @title List experiment types
#'
#' @description For a login token, list all available experiment types.
#' 
#' @inheritParams logout_openbis
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_experiment_types <- function(token, ...)
  do_openbis("listExperimentTypes", list(token), ...)

#' @title List plates
#'
#' @description For a login token, list all available plates.
#' 
#' @inheritParams logout_openbis
#' @param projects Data.frame holding objects for which experiments are to be
#' listed. If NULL, all experiments are returned.
#' @param exp_type Character vector, specifying the desired experiment type.
#' If NULL, all experiments are returned.
#' 
#' @return List/data.frame, containing (among others), columns \"plateCode\"
#' and \"spaceCodeOrNull\".
#' 
#' @export
#' 
list_experiments <- function(token,
                             projects = NULL,
                             exp_type = NULL,
                             ...) {

  if (!is.null(projects))
    assert_that(is.data.frame(projects),
                all(c("spaceCode", "code") %in% names(projects)),
                nrow(projects) >= 1L)
  else
    projects <- list_projects(token, ...)

  if (!is.null(exp_type))
    assert_that(is.character(exp_type),
                length(exp_type) >= 1L)
  else
    exp_type <- list_experiment_types(token, ...)[["code"]]

  proj <- mapply(function(x, y) list(`@type` = "Project", spaceCode = x,
                                     code = y),
                 projects[["spaceCode"]], projects[["code"]], SIMPLIFY = FALSE,
                 USE.NAMES = FALSE)

  res <- lapply(exp_type, function(type)
    do_openbis("listExperiments", list(token, proj, type), ...))

  do.call(rbind, res)
}

#' @title Get sample object of plate
#'
#' @description Given a plate id (barcode), the corresponding plate space is
#' determined using [list_plates] and the sample object representing the given
#' plate is queried.
#' 
#' @inheritParams logout_openbis
#' @param plate_id Plate barcode.
#' 
#' @return List/data.frame, containing (among others), columns \"id\",
#' \"permId\", \"identifier\", \"properties\", \"retrievedFetchOptions\".
#' 
#' @export
#' 
get_plate_sample <- function(token,
                             plate_id,
                             ...) {

  plates <- list_plates(token, ...)
  stopifnot(sum(plates[["plateCode"]] == plate_id) == 1L)

  space_code <- plates[plates[["plateCode"]] == plate_id, "spaceCodeOrNull"]

  do_openbis("getPlateSample",
             list(token, list(`@type` = "PlateIdentifier",
                              plateCode = plate_id,
                              spaceCodeOrNull = space_code)),
             "openbis/openbis/rmi-screening-api-v1.json", ...)
}

#' @title Get data sets for a plate
#'
#' @description Given a plate id (barcode), the corresponding sample object is
#' fetched using [get_plate_sample] and all available datasets for this sample
#' are queried.
#' 
#' @inheritParams get_plate_sample
#' 
#' @return List/data.frame, containing (among others), columns \"code\",
#' \"dataSetTypeCode\".
#' 
#' @export
#' 
list_plate_datasets <- function(token,
                                plate_id,
                                ...) {

  sample <- get_plate_sample(token, plate_id, ...)
  stopifnot(length(sample$id) == 1L)

  do_openbis("listDataSetsForSample",
             list(token,
                  sample[c("id", "permId", "identifier", "properties",
                           "retrievedFetchOptions")],
                  TRUE), ...)
}

#' @title Get data sets for a set of experiments
#'
#' @description Given a set of experiments as a data.frame, the corresponding
#' data sets are queried. Can be slow if several experiments are included in
#' the query.
#' 
#' @inheritParams logout_openbis
#' @param experiment A data.frame representing a set of experiments.
#' 
#' @return List/data.frame, containing (among others), columns \"code\",
#' \"dataSetTypeCode\".
#' 
#' @export
#' 
list_exp_datasets <- function(token,
                              experiment,
                              ...) {

  assert_that(is.data.frame(experiment),
              all(c("id", "permId", "identifier", "properties",
                    "experimentTypeCode") %in% names(experiment)),
              nrow(experiment) >= 1L)

  do_openbis("listDataSetsForExperiments",
             list(token,
                  experiment[c("id", "permId", "identifier", "properties",
                               "experimentTypeCode")],
                  list("CHILDREN")), ...)
}

#' @title Get files for a data set
#'
#' @description Given a data set code, the corresponding files are queried.
#' 
#' @inheritParams logout_openbis
#' @param data_id Data set code.
#' @param folder Folder to which the file search is restricted.
#' 
#' @return List/data.frame, containing (among others), columns
#' \"pathInDataSet\", \"pathInListing\".
#' 
#' @export
#' 
list_files <- function(token,
                       data_id,
                       folder = "original",
                       ...) {

  do_openbis("listFilesForDataSet",
             list(token, data_id, folder, TRUE),
             "datastore_server/rmi-dss-api-v1.json", ...)
}

#' @title Get download link for file
#'
#' @description Given a data set code and a file path, a download link is
#' generated, which has to be consumed immediately.
#' 
#' @inheritParams logout_openbis
#' @param data_id Data set code.
#' @param file File path for which the link is generated.
#' 
#' @return Url linking to file (expires immediately).
#' 
#' @export
#' 
get_download <- function(token,
                         data_id,
                         file,
                         ...) {

  do_openbis("getDownloadUrlForFileForDataSet",
             list(token, data_id, file),
             "datastore_server/rmi-dss-api-v1.json", ...)
}

#' @title Download files
#'
#' @description Given a data set code and a set of file paths (as returned
#' from [list_files]), a download link is generated for each file by
#' [get_download]. The downloads are preformed asynchronously.
#' 
#' @inheritParams get_download
#' @param files Files objects as produced by [list_files] describing the files
#' to be downloaded.
#' @param rep The number of times failed downloads are repeated.
#' 
#' @return A list of raw vectors holding the downloaded data.
#' 
#' @export
#' 
do_download <- function(token,
                        data_id,
                        files,
                        rep = 1,
                        ...) {

  assert_that(is.data.frame(files),
              all(c("pathInDataSet", "pathInListing", "isDirectory",
                    "crc32Checksum", "fileSize") %in% names(files)),
              nrow(files) <= 10L)

  res <- apply(files[!files[["isDirectory"]], ], 1, function(x)
    list(path  = x[["pathInDataSet"]], size = x[["fileSize"]],
         download = NULL))

  repeat {

    indexes <- which(sapply(res, function(x) is.null(x$download)))
    if (length(indexes) == 0) break
    if (rep < 0) stop("data could not be fetched successfully.")
    rep <- rep - 1

    if (length(indexes) == 1) {

      res[[indexes]]$download <- curl::curl_fetch_memory(
        get_download(token, data_id, res[[indexes]]$path, ...))

    } else {

      pool <- curl::new_pool()

      lapply(indexes, function(x) {
        curl::curl_fetch_multi(
          url    = get_download(token, data_id, res[[x]]$path, ...),
          handle = curl::new_handle(),
          pool   = pool,
          done   = function(res) res[[x]]$download <<- res,
          fail   = function(msg) {
            warning("request to ", res[[x]]$path, " failed:\n", msg)
            res[[x]]$download <<- NULL
          })
        invisible(NULL)
      })

      out <- curl::multi_run(pool = pool)
      if (out$success != length(indexes)) {
        warning(out$error, " download(s) failed; retrying.")
      }
    }

    res[indexes] <- lapply(res[indexes], function(x) {

      if (is.null(x$download))
        warning("request to ", x$path, " failed completely; retrying.")
      else if (x$download$status_code != 200) {

        warning("request to ", x$path, " failed with code ",
                x$download$status_code, "; retrying.")
        x$download <- NULL

      } else if (length(x$download$content) != as.integer(x$size)) {

        warning("request to ", x$path, " did not complete; retrying.")
        x$download <- NULL
      }

      x
    })
  }

  stats::setNames(lapply(res, function(x) x$download$content),
                  basename(sapply(res, `[[`, "path")))
}

#' @title Fetch single cell data
#'
#' @description Download single cell datasets corresponding to a plate barcode
#' and filtered by a regular expression applied to file names. 
#' 
#' @param file_regex The plate barcode of interest.
#' @inheritParams get_plate_sample
#' 
#' @return A list of downloaded file data (raw).
#' 
#' @export
#'
fetch_plate <- function(token,
                        plate_id,
                        file_regex,
                        ...) {

  ds <- list_plate_datasets(token, plate_id, ...)
  ds <- ds[ds[["dataSetTypeCode"]] == "HCS_ANALYSIS_CELL_FEATURES_CC_MAT", ]
  ds <- ds[which.max(ds[["registrationDetails"]][["registrationDate"]]), ]
  assert_that(nrow(ds) == 1L)

  files <- list_files(token, ds[["code"]], ...)
  files <- files[!files[["isDirectory"]] &
                 grepl(file_regex, basename(files[["pathInDataSet"]])), ]
  assert_that(nrow(files) >= 1L)

  n_bins <- ceiling(nrow(files) / 10)
  bin_size <- ceiling(nrow(files) / n_bins)

  cut <- rep(1:n_bins, each = bin_size)[seq_len(nrow(files))]

  res <- lapply(split(files, cut), function(x, ...)
    do_download(token, ds[["code"]], x, ...), ...)

  stats::setNames(unlist(res, recursive = FALSE),
                  unlist(sapply(res, names), recursive = FALSE))
}

#' @title Fetch InfectX meta data
#'
#' @description This function essentially provides default values for
#' downloading meta data from openBis. Two formats of meta data are currently
#' available: dumps of SQLite databased holding the entire set of experimental
#' meta data, as well as a CSV sheet, containing only the published subset.
#' 
#' @param type A switch for the type of meta data to be downloaded.
#' @inheritParams logout_openbis
#' 
#' @return A list of downloaded files (raw).
#' 
#' @export
#' 
fetch_meta <- function(token,
                       type = c("full", "public"),
                       ...) {

  type <- match.arg(type)

  exp <- list_experiments(token,
                          data.frame(spaceCode = ifelse(type == "full",
                                                        "INFECTX",
                                                        "INFECTX_PUBLISHED"),
                                     code = "_COMMON"), ...)
  exp <- exp[exp[["code"]] == ifelse(type == "full",
                                     "REPORTS", "AGGREGATEFILES"), ]
  assert_that(nrow(exp) == 1L)

  ds <- list_exp_datasets(token, exp, ...)

  if (type == "full")
    ds <- ds[ds[["dataSetTypeCode"]] == "HCS_ANALYSIS_WELL_REPORT_CSV" &
             grepl("CompoundDatabaseDump", ds[["properties"]][["NAME"]]), ]
  else
    ds <- ds[ds[["dataSetTypeCode"]] == "HCS_ANALYSIS_WELL_REPORT_CSV", ]

  ds <- ds[which.max(ds[["registrationDetails"]][["registrationDate"]]), ]
  assert_that(nrow(ds) == 1L)

  files <- list_files(token, ds[["code"]], ...)
  assert_that(nrow(files) >= 1L)

  do_download(token, ds[["code"]], files, ...)
}
