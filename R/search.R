
#' Search for objects
#'
#' Searching openBis can be done by creating a `SearchCriteria` object and
#' passing that to `search_openbis()`, alongside specifying what type of
#' object is being searched for. In case of `Sample`s being searched for,
#' a further argument, `fetch_options`, can be specified for controlling the
#' search, which can contain one or more of the strings
#'   * `ancestors`: Ask for all ancestors.
#'   * `basic`: Samples will have only basic attributes (id, code, type, space
#'              code, experiment identifier, registrator, registration date,
#'              modification date) but no properties.
#'   * `children`: Samples contain also their children samples.
#'   * `contained`: Ask for contained samples.
#'   * `descendants`: Ask for all descendants.
#'   * `metaprojects`: Ask for metaprojects this sample belongs to.
#'   * `parents`: Samples contain also their parent samples.
#'   * `properties`: Samples contain basic attributes and all properties.
#' 
#' A `SearchCriteria` object can be instantiated using the constructor
#' `search_criteria`, which takes one or several match clause objects, see
#' [match_clause()], a search operator specifying whether to match `all` or
#' `any` clauses, and optionally one or several `SearchSubCriteria` objects.
#' `SearchSubCriteria` objects in turn can be created with
#' `search_sub_criteria()`, which takes a single `SearchCriteria` object
#' alongside a string specifying the entities, the sub criterion is applied
#' to. Possibilities are
#'   * `data_set_container`
#'   * `data_set_parent`
#'   * `data_set_child`
#'   * `experiment`
#'   * `sample`
#'   * `sample_container`
#'   * `sample_child`
#'   * `sample_parent`
#' 
#' @inheritParams logout_openbis
#' @param criteria A single `SearchCriteria` object.
#' @param target_object The object type the search is targeted at, i.e.
#' `DataSet`s, `Experiment`s, etc.
#' @param fetch_options If samples are searched for, additional fetch options
#' can be specified.
#' @param ... One or more search clauses.
#' @param operator How to combine search clauses, either `all` or `any` have
#' to be fulfilled.
#' @param sub_criteria Optionally, one or several `SearchSubCriteria` objects
#' can be used to create a `SearchCriteria` object.
#' @param type The entity type, a `SearchSubCriteria` is applied to.
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "searchForDataSets")}
#' * \Sexpr{infx::docs_link("gis", "searchForExperiments")}
#' * \Sexpr{infx::docs_link("gis", "searchForMaterials")}
#' * \Sexpr{infx::docs_link("gis", "searchForSamples")}
#' 
#' @export
#' 
search_openbis <- function(token,
                           criteria,
                           target_object = c("data_set", "experiment",
                                             "material", "sample"),
                           fetch_options = NULL) {

  fun <- switch(match.arg(target_object),
                data_set = "searchForDataSets",
                experiment = "searchForExperiments",
                material = "searchForMaterials",
                sample = "searchForSamples")

  assert_that(is_json_class(criteria),
              has_subclass(criteria, "SearchCriteria"))

  if (!is.null(fetch_options)) {

    assert_that(fun == "searchForSamples")

    fetch_options <- lapply(toupper(fetch_options), match.arg,
                            c("BASIC", "PROPERTIES", "PARENTS", "CHILDREN",
                              "ANCESTORS", "DESCENDANTS", "CONTAINED",
                              "METAPROJECTS"))

    params <- list(token, criteria, stats::setNames(fetch_options, NULL))

  } else {

    params <- list(token, criteria)
  }

  make_request(api_url("gis"), fun, params)
}

#' @rdname search_openbis
#' @export
#' 
search_criteria <- function(...,
                            operator = "all",
                            sub_criteria = NULL) {

  is_clause <- function(x)
    is_json_class(x) && all(get_subclass(x) %in% c("MatchClause",
                                                   "PropertyMatchClause",
                                                   "AnyPropertyMatchClause",
                                                   "AnyFieldMatchClause",
                                                   "AttributeMatchClause",
                                                   "TimeAttributeMatchClause"))

  clauses <- list(...)

  assert_that(all(sapply(clauses, is_clause)))

  operator <- switch(operator,
                     all = "match_all",
                     any = "match_any",
                     operator)
  operator <- match.arg(toupper(operator),
                        c("MATCH_ALL_CLAUSES", "MATCH_ANY_CLAUSES"))

  if (is.null(sub_criteria)) {

    json_class(operator = operator, matchClauses = clauses,
               class = "SearchCriteria")
  } else {

    sub_criteria <- as_json_vec(sub_criteria)

    assert_that(has_subclass(sub_criteria, "SearchSubCriteria"))

    json_class(operator = operator, subCriterias = sub_criteria,
               matchClauses = clauses, class = "SearchCriteria")
  }
}

#' @rdname search_openbis
#' @export
#' 
search_sub_criteria <- function(criteria,
                                type = "sample") {

  type <- match.arg(toupper(type),
                    c("DATA_SET_CONTAINER", "DATA_SET_PARENT",
                      "DATA_SET_CHILD", "EXPERIMENT", "SAMPLE",
                      "SAMPLE_CONTAINER", "SAMPLE_CHILD", "SAMPLE_PARENT"))

  assert_that(is_json_class(criteria),
              has_subclass(criteria, "SearchCriteria"))

  json_class(targetEntityKind = type, criteria = criteria,
             class = "SearchSubCriteria")
}

#' Create a match clause
#' 
#' `SearchCriteria` objects, used for searching openBis, consist of one or
#' several match clauses. For more information on how to execute a search,
#' see [search_openbis()]. A match clause, broadly speaking, consist of a
#' given value, a field to which this value is compared to and a comparison
#' operator (e.g. equality).
#' 
#' In order to determine the possible values that can be supplied to
#' `property_clause()` as `property_code`s, `list_property_types()` can be
#' used. This function returns all property types available throughout the
#' queried openBis instance. As objects of several types
#' (`ControlledVocabularyPropertyType` and `PropertyType`) are returned as
#' property types by the API, the resulting object is a list with each entry
#' corresponding to a type and holding a set of object of the respective type.
#'
#' @param desired_value The value used in the comparison.
#' @param clause_type The type of match clause.
#' @param field_type The type of filed used in the comparison.
#' @param mode The comparison mode, can be `eq` (==), `lte` (<=) or `gte`
#' (>=).  
#' @param ... Further arguments for creating the match clause.
#' 
#' @export
#' 
match_clause <- function(desired_value,
                         clause_type = c("MatchClause",
                                         "PropertyMatchClause",
                                         "AnyPropertyMatchClause",
                                         "AnyFieldMatchClause",
                                         "AttributeMatchClause",
                                         "TimeAttributeMatchClause"),
                         field_type = NULL,
                         mode = "eq",
                         ...) {

  clause_type <- match.arg(clause_type)

  if (is.null(field_type))
    field_type <- switch(clause_type,
                         PropertyMatchClause = "property",
                         AnyPropertyMatchClause = "any_property",
                         AnyFieldMatchClause = "any_field",
                         AttributeMatchClause = "attribute",
                         TimeAttributeMatchClause = "attribute",
                         stop("Specify field_type for MatchClause clauses."))

  field_type <- match.arg(toupper(field_type),
                          c("PROPERTY", "ATTRIBUTE", "ANY_FIELD",
                            "ANY_PROPERTY"))

  mode <- switch(mode,
                 lte = "less_than_or_equal",
                 gte = "greater_than_or_equal",
                 mode)

  mode <- match.arg(toupper(mode),
                    c("EQUALS", "LESS_THAN_OR_EQUAL", "GREATER_THAN_OR_EQUAL"))

  assert_that(length(desired_value) == 1L)

  json_class(fieldType = field_type, desiredValue = desired_value,
             compareMode = mode, ..., class = clause_type)
}

#' @param property_code Code identifying a property to be used for the
#' comparison.
#' 
#' @rdname match_clause
#' @export
#' 
property_clause <- function(desired_value, property_code) {

  assert_that(is.character(property_code), length(property_code) == 1L)

  match_clause(desired_value, "PropertyMatchClause",
               propertyCode = property_code)
}

#' @rdname match_clause
#' @export
#' 
any_property_clause <- function(desired_value)
  match_clause(desired_value, "AnyPropertyMatchClause")

#' @rdname match_clause
#' @export
#' 
any_field_clause <- function(desired_value)
  match_clause(desired_value, "AnyFieldMatchClause")

#' @param attribute Name of the attribute to be used for the comparison.
#' 
#' @rdname match_clause
#' @export
#' 
attribute_clause <- function(desired_value, attribute = "code") {

  attribute <- match.arg(toupper(attribute),
                         c("CODE", "TYPE", "PERM_ID", "SPACE", "PROJECT",
                           "PROJECT_PERM_ID", "METAPROJECT",
                           "REGISTRATOR_USER_ID", "REGISTRATOR_FIRST_NAME",
                           "REGISTRATOR_LAST_NAME", "REGISTRATOR_EMAIL",
                           "MODIFIER_USER_ID", "MODIFIER_FIRST_NAME",
                           "MODIFIER_LAST_NAME", "MODIFIER_EMAIL"))

  match_clause(desired_value, "AttributeMatchClause", attribute = attribute)
}

#' @param desired_date A date used for the comparison.
#' @param timezone A string identifying the timezone of the specified date,
#' examples include "+1", "-5", "0", etc.
#' 
#' @rdname match_clause
#' @export
#' 
time_attribute_clause <- function(desired_date = Sys.Date(),
                                  attribute = "registration",
                                  timezone = 0L,
                                  mode = "eq") {

  attribute <- match.arg(toupper(attribute),
                         c("REGISTRATION_DATE", "MODIFICATION_DATE"))

  assert_that(is.integer(timezone), timezone <= 12L, timezone >= -12L,
              inherits(desired_date, "Date"))

  match_clause(format(desired_date, "%Y-%m-%d"), "TimeAttributeMatchClause",
               mode = mode, timeZone = timezone, attribute = attribute)
}

#' @inheritParams logout_openbis
#' @param with_relations Logical switch indicating whether relations should
#' be returned as well.
#' 
#' @rdname match_clause
#' @export
#' 
list_property_types <- function(token, with_relations = FALSE) {

  assert_that(is.logical(with_relations), length(with_relations) == 1L)

  res <- make_request(api_url("gis"), "listPropertyTypes",
                      list(token, with_relations))

  classes <- sapply(res, get_subclass)

  lapply(split(res, classes), as_json_vec)
}
