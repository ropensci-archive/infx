
#' Assemble and execute openBIS search queries
#' 
#' Searching in openBIS presents a powerful alternative to iterative listing
#' and selecting of objects. As an example, in order to find image data sets
#' associated with an experiment, instead of first listing all experiments,
#' selecting the one of interest, then listing all plates of that experiment,
#' followed by listing image data sets for each of the plates, the requested
#' data sets can be directly retrieved by constructing a search query with
#' `search_criteria()` and executing the search by calling `search_openbis()`.
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
#' `search_criteria()`, which takes one or several match clause objects, a
#' search operator specifying whether to match `all` or `any` clauses, and
#' optionally one or several `SearchSubCriteria` objects. `SearchSubCriteria`
#' objects in turn can be created with `search_sub_criteria()`, which takes a
#' single `SearchCriteria` object alongside a string specifying the entities,
#' the sub criterion is applied to. Possibilities are
#'   * `data_set_container`
#'   * `data_set_parent`
#'   * `data_set_child`
#'   * `experiment`
#'   * `sample`
#'   * `sample_container`
#'   * `sample_child`
#'   * `sample_parent`
#' 
#' `SearchCriteria` objects, used for searching openBis, consist of one or
#' several match clauses. A match clause, broadly speaking, consist of a
#' desired value, a field to which this value is compared to and a comparison
#' operator (e.g. equality). Match clauses can be constructed using any of
#' `attribute_clause()`, `time_attribute_clause()`, `property_clause()`,
#' `any_property_clause()`, and `any_field_clause()`. Attribute match clauses
#' have a fixed set of attributes against which the match is performed:
#'   * time attribute match clauses
#'     - `registration_date`
#'     - `modification_date`
#'   * attribute match clauses
#'     - `code`
#'     - `type`
#'     - `perm_id`
#'     - `space`
#'     - `project`
#'     - `project_perm_id`
#'     - `metaproject`
#'     - `registrator_user_id`
#'     - `registrator_first_name`
#'     - `registrator_last_name`
#'     - `registrator_email`
#'     - `modifier_user_id`
#'     - `modifier_first_name`
#'     - `modifier_last_name`
#'     - `modifier_email`
#' 
#' In order to determine the possible values that can be supplied to
#' `property_clause()` as `property_code`s, `list_property_types()` can be
#' called. This function returns all property types available throughout the
#' queried openBis instance. As objects of several types
#' (`ControlledVocabularyPropertyType` and `PropertyType`) are returned as
#' property types by the API, the resulting object is a list with each entry
#' corresponding to a type and holding a set of object of the respective type.
#' 
#' The comparison operator (default is equality) can be any of the following
#'   * `equals`
#'   * `less_than_or_equal`, with alias `lte`
#'   * `greater_than_or_equal`, with alias `gte`
#' 
#' All of the option matching is not case-sensitive and is performed with
#' [base::match.arg()] and therefore options may be abbreviated (e.g. `eq`
#' instead of `equals`).
#' 
#' @inheritParams logout_openbis
#' @param criteria A single `SearchCriteria` object.
#' @param target_object The object type the search is targeted at, i.e.
#' `DataSet`s, `Experiment`s, etc.
#' @param fetch_options If samples are searched for, additional fetch options
#' can be specified.
#' @param ... For `search_openbis()` passed to [make_request()], for
#' `search_criteria()` a set of match clause objects, and for match clause
#' constructors, the comparison mode can be passed as `mode` argument, which
#' may be `eq` (==), `lte` (<=) or `gte` (>=).
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "searchForDataSets")}
#' * \Sexpr{infx::docs_link("gis", "searchForExperiments")}
#' * \Sexpr{infx::docs_link("gis", "searchForMaterials")}
#' * \Sexpr{infx::docs_link("gis", "searchForSamples")}
#' 
#' @examples
#' \donttest{
#'   tok <- login_openbis()
#'   
#'   # search for an experiment, e.g. ADENO-AU-K1
#'   exp <- search_openbis(tok,
#'                         search_criteria(
#'                           property_clause("pathogen", "Adenovirus"),
#'                           property_clause("library", "Ambion"),
#'                           property_clause("geneset", "Kinome"),
#'                           property_clause("replicate", 1L)
#'                         ),
#'                         target_object = "experiment")
#' 
#'   # the same can be achieved using the code attribute
#'   identical(exp,
#'             search_openbis(tok,
#'                            search_criteria(
#'                              attribute_clause(value = "ADENO-AU-K1")
#'                            ),
#'                            target_object = "experiment"))
#' 
#'   # of using the perm_id attribute
#'   identical(exp,
#'             search_openbis(tok,
#'                            search_criteria(
#'                              attribute_clause("perm_id",
#'                                               "20111223100933426-318017")
#'                            ),
#'                            target_object = "experiment"))
#' 
#'   # a search with no matches returns an empty list
#'   search_openbis(tok,
#'                  search_criteria(attribute_clause(value = "foo_bar")),
#'                  target_object = "experiment")
#' 
#'   # search using sub-criteria: all plate samples of experiment ADENO-DU-K1
#'   sub <- search_sub_criteria(search_criteria(
#'                                attribute_clause(value = "ADENO-DU-K1")
#'                              ),
#'                              type = "experiment")
#'   all <- search_openbis(
#'     tok,
#'     search_criteria(
#'       attribute_clause("type", "PLATE"),
#'       sub_criteria = sub
#'     ),
#'     target_object = "sample"
#'   )
#'   length(as_json_vec(all))
#' 
#'   # now only include ADENO-DU-K1 plate samples registered after Feb 1st 2013
#'   some <- search_openbis(
#'     tok,
#'     search_criteria(
#'       attribute_clause("type", "PLATE"),
#'       time_attribute_clause(value = as.Date("2013-02-01"), mode = "gte"),
#'       sub_criteria = sub
#'     ),
#'     target_object = "sample"
#'   )
#'   length(as_json_vec(some))
#' 
#'   logout_openbis(tok)
#' }
#' 
#' @export
#' 
search_openbis <- function(token,
                           criteria,
                           target_object = c("data_set", "experiment",
                                             "material", "sample"),
                           fetch_options = NULL,
                           ...) {

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

  make_request(api_url("gis", attr(token, "host_url"), ...),
               fun,
               params,
               ...)
}

#' @param operator How to combine search clauses, either `all` or `any` have
#' to be fulfilled.
#' @param sub_criteria Optionally, one or several `SearchSubCriteria` objects
#' can be used to create a `SearchCriteria` object.
#' 
#' @rdname search_openbis
#' @export
#' 
search_criteria <- function(...,
                            operator = "all",
                            sub_criteria = NULL) {

  is_clause <- function(x)
    is_json_class(x) && all(get_subclass(x) %in% c("PropertyMatchClause",
                                                   "AnyPropertyMatchClause",
                                                   "AnyFieldMatchClause",
                                                   "AttributeMatchClause",
                                                   "TimeAttributeMatchClause"))

  clauses <- list(...)

  assert_that(all(vapply(clauses, is_clause, logical(1L))))

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

#' @param type The entity type, a `SearchSubCriteria` is applied to.
#' 
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

match_clause <- function(value,
                         clause_type = c("PropertyMatchClause",
                                         "AnyPropertyMatchClause",
                                         "AnyFieldMatchClause",
                                         "AttributeMatchClause",
                                         "TimeAttributeMatchClause"),
                         mode = "eq",
                         ...) {

  clause_type <- match.arg(clause_type)

  field_type <- switch(clause_type,
                       PropertyMatchClause = "PROPERTY",
                       AnyPropertyMatchClause = "ANY_PROPERTY",
                       AnyFieldMatchClause = "ANY_FIELD",
                       AttributeMatchClause = "ATTRIBUTE",
                       TimeAttributeMatchClause = "ATTRIBUTE")

  mode <- switch(mode,
                 lte = "less_than_or_equal",
                 gte = "greater_than_or_equal",
                 mode)

  mode <- match.arg(toupper(mode),
                    c("EQUALS", "LESS_THAN_OR_EQUAL", "GREATER_THAN_OR_EQUAL"))

  assert_that(length(value) == 1L)

  json_class(fieldType = field_type, desiredValue = value,
             compareMode = mode, ..., class = clause_type)
}

#' @param property_code Code identifying a property to be used for the
#' comparison.
#' @param value The value used in the comparison.
#' 
#' @rdname search_openbis
#' @export
#' 
property_clause <- function(property_code, value, ...) {

  assert_that(is.character(property_code), length(property_code) == 1L)

  match_clause(value, "PropertyMatchClause", ...,
               propertyCode = toupper(property_code))
}

#' @rdname search_openbis
#' @export
#' 
any_property_clause <- function(value, ...)
  match_clause(value, "AnyPropertyMatchClause", ...)

#' @rdname search_openbis
#' @export
#' 
any_field_clause <- function(value, ...)
  match_clause(value, "AnyFieldMatchClause", ...)

#' @param attribute Name of the attribute to be used for the comparison.
#' 
#' @rdname search_openbis
#' @export
#' 
attribute_clause <- function(attribute = "code", value, ...) {

  attribute <- match.arg(toupper(attribute),
                         c("CODE", "TYPE", "PERM_ID", "SPACE", "PROJECT",
                           "PROJECT_PERM_ID", "METAPROJECT",
                           "REGISTRATOR_USER_ID", "REGISTRATOR_FIRST_NAME",
                           "REGISTRATOR_LAST_NAME", "REGISTRATOR_EMAIL",
                           "MODIFIER_USER_ID", "MODIFIER_FIRST_NAME",
                           "MODIFIER_LAST_NAME", "MODIFIER_EMAIL"))

  match_clause(value, "AttributeMatchClause", ...,
               attribute = attribute)
}

#' @param timezone A string identifying the timezone of the specified date,
#' examples include "+1", "-5", "0", etc.
#' 
#' @rdname search_openbis
#' @export
#' 
time_attribute_clause <- function(attribute = "registration",
                                  value = Sys.Date(),
                                  timezone = 0L,
                                  ...) {

  attribute <- match.arg(toupper(attribute),
                         c("REGISTRATION_DATE", "MODIFICATION_DATE"))

  assert_that(is.integer(timezone), timezone <= 12L, timezone >= -12L,
              inherits(value, "Date"))

  match_clause(format(value, "%Y-%m-%d"), "TimeAttributeMatchClause",
               ..., timeZone = timezone, attribute = attribute)
}

#' @param with_relations Logical switch indicating whether relations should
#' be returned as well.
#' 
#' @rdname search_openbis
#' @export
#' 
list_property_types <- function(token, with_relations = FALSE, ...) {

  assert_that(is.logical(with_relations), length(with_relations) == 1L)

  res <- make_request(api_url("gis", attr(token, "host_url"), ...),
                      "listPropertyTypes",
                      list(token, with_relations),
                      ...)

  classes <- vapply(res, get_subclass, character(1L))

  lapply(split(res, classes), as_json_vec)
}
