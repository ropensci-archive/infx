
#' @title Helper querying openBis API
#'
#' @description Issues a POST request to the JSON-RPC based openBis API v1,
#' using [make_request]. Documentation is available at
#' \code{https://wiki-bsse.ethz.ch/display/openBISDoc/openBIS+JSON+API}.
#' 
#' @inheritParams make_request
#' @param api The api section the used method is part of. This is used to
#' construct the url the request is sent to.
#' @param host The base url, the request is sent to.
#' 
#' @return A list/data.frame holding the response from openBis.
#' 
query_openbis <- function(method,
                          params,
                          api = c("IGeneralInformationService",
                                  "IGeneralInformationChangingService",
                                  "IQueryApiServer",
                                  "IWebInformationService",
                                  "IDssServiceRpcGeneric",
                                  "IScreeningApiServer",
                                  "IDssServiceRpcScreening"),
                          host = "https://infectx.biozentrum.unibas.ch") {

  api <- match.arg(api)

  url <- switch(api,
                IGeneralInformationService =
                  "openbis/openbis/rmi-general-information-v1.json",
                IGeneralInformationChangingService =
                  "openbis/openbis/rmi-general-information-changing-v1.json",
                IQueryApiServer =
                  "openbis/openbis/rmi-query-v1.json",
                IWebInformationService =
                  "openbis/openbis/rmi-web-information-v1.json",
                IDssServiceRpcGeneric =
                  "datastore_server/rmi-dss-api-v1.json",
                IScreeningApiServer =
                  "openbis/openbis/rmi-screening-api-v1.json",
                IDssServiceRpcScreening =
                  "rmi-datastore-server-screening-api-v1.json")

  make_request(paste(host, url, sep = "/"), method, params)
}

#' @title Generate a login token
#'
#' @description Create a login token for openBis API calls. Upon garbage
#' collection of the token, the user is logged out.
#' 
#' @param user,pwd Login credentials for an openBis instance.
#' @param auto_disconnect Logical switch for automatically closing the
#' connection upon garbage collection of the token.
#' @param ... Further arguments are passed to [query_openbis].
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

  token <- unlist(query_openbis("tryToAuthenticateForAllServices",
                                list(user, pwd), ...))

  assert_that(is.character(token), length(token) == 1L, msg = "Login failed.")

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
#' @param ... Further arguments are passed to [query_openbis].
#' 
#' @return NULL (invisibly)
#' 
#' @export
#' 
logout_openbis <- function(token, ...)
  invisible(unlist(query_openbis("logout", list(token), ...)))

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
  unlist(query_openbis("isSessionActive", list(token), ...))

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
  query_openbis("listPlates", list(token), "IScreeningApiServer", ...)

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
  query_openbis("listProjects", list(token), ...)

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
  query_openbis("listExperimentTypes", list(token), ...)

#' @title List plates
#'
#' @description For a login token, list all available plates.
#' 
#' @inheritParams logout_openbis
#' @param projects List holding Project objects for which experiments are to be
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

  if (!is.null(projects)) {
    if (has_json_class(projects, "Project")) projects <- list(projects)
    assert_that(is.list(projects),
                all(sapply(projects, has_json_class, "Project")),
                length(projects) >= 1L)
  } else
    projects <- list_projects(token, ...)

  if (!is.null(exp_type)) {
    if (is.list(exp_type))
      exp_type <- sapply(exp_type, `[[`, "code")
    assert_that(is.character(exp_type),
                length(exp_type) >= 1L)
  } else
    exp_type <- sapply(list_experiment_types(token, ...), `[[`, "code")

  proj <- lapply(projects, `[`, c("spaceCode", "code"))

  res <- lapply(exp_type, function(type)
    query_openbis("listExperiments", list(token, proj, type), ...))

  do.call(c, res)
}

#' @title Get sample object of plate
#'
#' @description Given a plate id (barcode), the corresponding plate space is
#' determined using [list_plates] and the sample object representing the given
#' plate is queried.
#' 
#' @inheritParams logout_openbis
#' @param plate_id Plate barcode.
#' @param space_code The space code of the plate; it NULL, it is determined
#' automatically.
#' 
#' @return List, containing (among others), entries \"id\", \"permId\",
#' \"identifier\", \"properties\", \"retrievedFetchOptions\".
#' 
#' @export
#' 
get_plate_sample <- function(token,
                             plate_id,
                             space_code = NULL,
                             ...) {

  assert_that(is.character(plate_id), length(plate_id) == 1L)

  if (is.null(space_code)) {
    plates <- list_plates(token, ...)
    plate_match <- sapply(plates, `[[`, "plateCode") == plate_id
    assert_that(sum(plate_match) == 1L)

    space_code <- plates[[which(plate_match)]][["spaceCodeOrNull"]]
  }

  assert_that(is.character(space_code), length(space_code) == 1L)

  plate_id <- structure(list(plateCode = plate_id,
                             spaceCodeOrNull = space_code),
                        class = "json_class", json_class = "PlateIdentifier")

  query_openbis("getPlateSample", list(token, plate_id),
                "IScreeningApiServer", ...)
}

#' @title Get data sets for a plate
#'
#' @description Given a plate id (barcode), the corresponding sample object is
#' fetched using [get_plate_sample] and all available datasets for this sample
#' are queried.
#' 
#' @inheritParams get_plate_sample
#' 
#' @return List, containing (among others), fields \"code\", and
#' \"dataSetTypeCode\".
#' 
#' @export
#' 
list_plate_datasets <- function(token,
                                plate_id,
                                ...) {

  sample <- get_plate_sample(token, plate_id, ...)

  assert_that(has_json_class(sample, "Sample"),
              length(sample[["id"]]) == 1L)

  query_openbis("listDataSetsForSample",
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
#' @return List, containing (among others), entries \"code\" and
#' \"dataSetTypeCode\".
#' 
#' @export
#' 
list_exp_datasets <- function(token,
                              experiment,
                              ...) {

  if (has_json_class(experiment, "Experiment")) experiment <- list(experiment)

  assert_that(all(sapply(experiment, has_json_class, "Experiment")),
              length(experiment) >= 1L)

  query_openbis("listDataSetsForExperiments",
                list(token,
                     lapply(experiment, `[`,
                            c("id", "permId", "identifier", "properties",
                              "experimentTypeCode")),
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
list_files <- function(token, data_id, folder = "original", ...)
  query_openbis("listFilesForDataSet", list(token, data_id, folder, TRUE),
                "IDssServiceRpcGeneric", ...)

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
