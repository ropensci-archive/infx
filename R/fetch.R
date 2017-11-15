
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
get_download <- function(token, data_id, file, ...)
  query_openbis("getDownloadUrlForFileForDataSet", list(token, data_id, file),
                "IDssServiceRpcGeneric", ...)

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
#' @section TODO: use file checksums instead of length for checking file
#' integrity; see https://stackoverflow.com/q/21558777, can't rawToChar, might
#' have to write to disk?
#' 
#' @export
#' 
do_download <- function(token,
                        data_id,
                        files,
                        rep = 1) {

  if (is_json_class(files)) files <- list(files)

  assert_that(all(sapply(files, has_json_class, "FileInfoDssDTO")),
              length(files) <= 10L)

  res <- lapply(files[!sapply(files, `[[`, "isDirectory")], `[`,
                c("pathInDataSet", "fileSize"))

  repeat {

    indexes <- which(sapply(res, function(x) is.null(x$download)))
    if (length(indexes) == 0) break
    if (rep < 0) stop("data could not be fetched successfully.")
    rep <- rep - 1

    pool <- curl::new_pool()

    lapply(indexes, function(x) {
      curl::curl_fetch_multi(
        url    = get_download(token, data_id, res[[x]][["pathInDataSet"]]),
        handle = curl::new_handle(),
        pool   = pool,
        done   = function(dld) {
          if (dld$status_code != 200) {
            warning("request to ", basename(res[[x]][["pathInDataSet"]]),
                    " failed with code ", dld$status_code, "; retrying.")
            res[[x]]$download <<- NULL
          } else if (length(dld$content) !=
                       as.integer(res[[x]][["fileSize"]])) {
            warning("request to ", basename(res[[x]][["pathInDataSet"]]),
                    " did not complete; retrying.")
            res[[x]]$download <<- NULL
          } else {
            res[[x]]$download <<- dld$content
          }
        },
        fail   = function(msg) {
          warning("request to ", basename(res[[x]][["pathInDataSet"]]),
                  " failed:\n", msg)
          res[[x]]$download <<- NULL
        })
      invisible(NULL)
    })

    out <- curl::multi_run(pool = pool)
    if (out$success != length(indexes)) {
      warning(out$error, " download(s) failed; retrying.")
    }
  }

  stats::setNames(lapply(res, `[[`, "download"),
                  basename(sapply(res, `[[`, "pathInDataSet")))
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
                        file_regex) {

  ds <- list_plate_datasets(token, plate_id)

  ds <- extract_dataset(ds, type = "HCS_ANALYSIS_CELL_FEATURES_CC_MAT",
                        most_recent = TRUE)[[1]]

  files <- list_files(token, ds[["code"]])
  sapply(files, `[[`, "isDirectory")
  files <- files[!sapply(files, `[[`, "isDirectory") &
                 grepl(file_regex,
                       basename(sapply(files, `[[`, "pathInDataSet")))]
  assert_that(length(files) >= 1L)

  n_bins <- ceiling(length(files) / 5)
  bin_size <- ceiling(length(files) / n_bins)

  cut <- rep(1:n_bins, each = bin_size)[seq_len(length(files))]

  if (n_bins > 1) {
    tot <- sum(as.integer(sapply(files, `[[`, "fileSize")))
    pb <- progress::progress_bar$new(
      format = paste0("downloading [:bar] :percent in :elapsed (:bytes of ",
                      format(structure(tot, class = "object_size"),
                             units = "auto"), ")"),
      total = tot)
    pb$tick(0)
  } else pb <- NULL

  res <- lapply(split(files, cut), function(x) {
    dat <- lapply(do_download(token, ds[["code"]], x), function(y) {
      tryCatch(read_data(y), error = function(e) NULL)
    })
    if (!is.null(pb)) pb$tick(sum(as.integer(sapply(files, `[[`, "fileSize"))))
    dat[!sapply(dat, is.null)]
  })

  stats::setNames(unlist(res, recursive = FALSE),
                  unlist(lapply(res, names), recursive = FALSE))
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
                          structure(list(
                            spaceCode = ifelse(type == "full", "INFECTX",
                                               "INFECTX_PUBLISHED"),
                            code = "_COMMON"),
                            class = "json_class", json_class = "Project"))

  exp <- exp[sapply(exp, `[[`, "code") == ifelse(type == "full",
                                                 "REPORTS", "AGGREGATEFILES")]
  assert_that(length(exp) == 1L)

  ds <- list_exp_datasets(token, exp)

  ds <- extract_dataset(ds, type = "HCS_ANALYSIS_WELL_REPORT_CSV")

  if (type == "full")
    ds <- ds[sapply(lapply(ds, `[[`, "properties"), `[[`, "NAME") ==
               "CompoundDatabaseDump"]

  ds <- extract_dataset(ds, most_recent = TRUE)[[1]]

  files <- list_files(token, ds[["code"]])
  assert_that(length(files) >= 1L)

  res <- do_download(token, ds[["code"]], files)

  if (type == "full")
    read_full_meta(res, ...)
  else
    read_pub_meta(res, ...)
}

#' @title Extract DataSets from a list
#'
#' @description Finds and extracts the DataSets from a list of DataSets that
#' match the argument type and/or are the most recent available.
#' 
#' @param ds The list of DataSets to operate on.
#' @param type A string holding the type to be matched.
#' @param most_recent Logical switch whether to select the most recent DataSet.
#' 
#' @return A (subsetted) list of DataSets.
#' 
extract_dataset <- function(ds, type = NULL, most_recent = FALSE) {

  if (is_json_class(ds)) ds <- list(ds)

  assert_that(all(sapply(ds, has_json_class, "DataSet")),
              length(ds) >= 1L)

  if (!is.null(type)) {
    assert_that(is.character(type), length(type) == 1L)
    ds <- ds[sapply(ds, `[[`, "dataSetTypeCode") == type]
    assert_that(length(ds) >= 1L)
  }

  if (most_recent) {
    ds <- ds[which.max(sapply(lapply(ds, `[[`, "registrationDetails"), `[[`,
                              "registrationDate"))]
    assert_that(length(ds) == 1L)
  }

  ds
}