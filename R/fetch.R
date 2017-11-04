
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
#' integrity; see https://goo.gl/4w7PMV, can't rawToChar, might have to write
#' to disk?
#' 
#' @export
#' 
do_download <- function(token,
                        data_id,
                        files,
                        rep = 1) {

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

    pool <- curl::new_pool()

    lapply(indexes, function(x) {
      curl::curl_fetch_multi(
        url    = get_download(token, data_id, res[[x]]$path),
        handle = curl::new_handle(),
        pool   = pool,
        done   = function(dld) {
          if (dld$status_code != 200) {
            warning("request to ", basename(res[[x]]$path),
                    " failed with code ", dld$status_code, "; retrying.")
            res[[x]]$download <<- NULL
          } else if (length(dld$content) != as.integer(res[[x]]$size)) {
            warning("request to ", basename(res[[x]]$path),
                    " did not complete; retrying.")
            res[[x]]$download <<- NULL
          } else {
            res[[x]]$download <<- dld$content
          }
        },
        fail   = function(msg) {
          warning("request to ", basename(res[[x]]$path), " failed:\n", msg)
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
                        file_regex) {

  ds <- list_plate_datasets(token, plate_id)
  ds <- ds[ds[["dataSetTypeCode"]] == "HCS_ANALYSIS_CELL_FEATURES_CC_MAT", ]
  ds <- ds[which.max(ds[["registrationDetails"]][["registrationDate"]]), ]
  assert_that(nrow(ds) == 1L)

  files <- list_files(token, ds[["code"]])
  files <- files[!files[["isDirectory"]] &
                 grepl(file_regex, basename(files[["pathInDataSet"]])), ]
  assert_that(nrow(files) >= 1L)

  n_bins <- ceiling(nrow(files) / 5)
  bin_size <- ceiling(nrow(files) / n_bins)

  cut <- rep(1:n_bins, each = bin_size)[seq_len(nrow(files))]

  if (n_bins > 1) {
    tot <- sum(as.integer(files[["fileSize"]]))
    pb <- progress::progress_bar$new(
      format = paste("downloading [:bar] :percent in :elapsed (:bytes of ",
                     format(structure(tot, class = "object_size"),
                            units = "auto"), ")"),
      total = tot)
    pb$tick(0)
  } else pb <- NULL

  res <- lapply(split(files, cut), function(x) {
    dat <- lapply(do_download(token, ds[["code"]], x), function(y) {
      tryCatch(read_data(y), error = function(e) NULL)
    })
    if (!is.null(pb)) pb$tick(sum(as.integer(x[["fileSize"]])))
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
