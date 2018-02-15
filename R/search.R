
#' @export
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

  if (!is.null(fetch_options)) {
    assert_that(fun == "searchForSamples")
    params <- list(token, criteria, match.arg(toupper(fetch_options),
                                              c("BASIC", "PROPERTIES",
                                                "PARENTS", "CHILDREN",
                                                "ANCESTORS", "DESCENDANTS",
                                                "CONTAINED", "METAPROJECTS")))
  } else {
    params <- list(token, criteria)
  }

  request_openbis(fun, params)
}

#' @export
search_criteria <- function(clauses,
                            operator = search_operator(),
                            sub_criteria = NULL) {

  is_clause <- function(x) {
    is_json_class(x) && all(get_subclass(x) %in% c("MatchClause",
                                                   "PropertyMatchClause",
                                                   "AnyPropertyMatchClause",
                                                   "AnyFieldMatchClause",
                                                   "AttributeMatchClause",
                                                   "TimeAttributeMatchClause"))
  }

  if (is_json_class(clauses))
    clauses <- list(clauses)

  assert_that(all(sapply(clauses, is_clause)),
              is_json_class(operator),
              has_subclass(operator, "SearchOperator"))

  if (is.null(sub_criteria))
    json_class(operator = as.character(operator), matchClauses = clauses,
               class = "SearchCriteria")
  else
    json_class(operator = as.character(operator), subCriterias = sub_criteria,
               matchClauses = clauses, class = "SearchCriteria")
}

#' @export
clause <- function(desired_value,
                   clause_type = c("MatchClause",
                                   "PropertyMatchClause",
                                   "AnyPropertyMatchClause",
                                   "AnyFieldMatchClause",
                                   "AttributeMatchClause",
                                   "TimeAttributeMatchClause"),
                   field_type = NULL,
                   field_code = NULL,
                   mode = compare_mode(),
                   ...) {

  clause_type <- match.arg(clause_type)

  if (is.null(field_type)) {
    field_type <- switch(clause_type,
                         PropertyMatchClause = field_type("property"),
                         AnyPropertyMatchClause = field_type("any_property"),
                         AnyFieldMatchClause = field_type("any_field"),
                         AttributeMatchClause = field_type("attribute"),
                         TimeAttributeMatchClause = field_type("attribute"),
                         stop("Specify field_type for MatchClause clauses."))
  }

  assert_that(is.character(desired_value), length(desired_value) == 1L,
              has_subclass(field_type, "MatchClauseFieldType"),
              has_subclass(mode, "CompareMode"))

  if (is.null(field_code)) {
    json_class(fieldType = as.character(as_json_class(field_type)),
               desiredValue = desired_value,
               compareMode = as.character(as_json_class(mode)),
               ...,
               class = clause_type)
  } else {
    assert_that(is.character(field_code), length(field_code) == 1L)
    json_class(fieldType = as.character(as_json_class(field_type)),
               fieldCode = field_code,
               desiredValue = desired_value,
               compareMode = as.character(as_json_class(mode)),
               ...,
               class = clause_type)
  }
}

#' @export
property_clause <- function(desired_value, property_code) {

  assert_that(is.character(property_code), length(property_code) == 1L)

  clause(desired_value, "PropertyMatchClause", propertyCode = property_code)
}

#' @export
any_property_clause <- function(desired_value)
  clause(desired_value, "AnyPropertyMatchClause")

#' @export
any_field_clause <- function(desired_value)
  clause(desired_value, "AnyFieldMatchClause")

#' @export
attribute_clause <- function(desired_value, attribute = attribute()) {

  assert_that(has_subclass(attribute, "MatchClauseAttribute"))

  clause(desired_value, "AttributeMatchClause",
         attribute = as.character(as_json_class(attribute)))
}

#' @export
time_attribute_clause <- function(desired_date = Sys.Date(),
                                  attribute = time_attribute(),
                                  timezone = 0L,
                                  mode = compare_mode()) {

  assert_that(has_subclass(attribute, "MatchClauseTimeAttribute"),
              is.integer(timezone), timezone <= 12L, timezone >= -12L,
              inherits(desired_date, "Date"))

  clause(format(desired_date, "%Y-%m-%d"), "TimeAttributeMatchClause",
         mode = mode, timeZone = timezone,
         attribute = as.character(as_json_class(attribute)))
}

#' @export
field_type <- function(x = "property") {

  json_class(match.arg(toupper(x),
                       c("PROPERTY", "ATTRIBUTE", "ANY_FIELD",
                         "ANY_PROPERTY")),
             class = "MatchClauseFieldType")
}

#' @export
attribute <- function(x = "code") {

  json_class(match.arg(toupper(x),
                       c("CODE", "TYPE", "PERM_ID", "SPACE", "PROJECT",
                         "PROJECT_PERM_ID", "METAPROJECT",
                         "REGISTRATOR_USER_ID", "REGISTRATOR_FIRST_NAME",
                         "REGISTRATOR_LAST_NAME", "REGISTRATOR_EMAIL",
                         "MODIFIER_USER_ID", "MODIFIER_FIRST_NAME",
                         "MODIFIER_LAST_NAME", "MODIFIER_EMAIL")),
             class = "MatchClauseAttribute")
}

#' @export
time_attribute <- function(x = "registration") {

  json_class(match.arg(toupper(x),
                       c("REGISTRATION_DATE", "MODIFICATION_DATE")),
             class = "MatchClauseTimeAttribute")
}

#' @export
compare_mode <- function(x = "eq") {

  mode <- switch(x,
                 eq = "EQUALS",
                 lte = "LESS_THAN_OR_EQUAL",
                 gte = "GREATER_THAN_OR_EQUAL",
                 x)

  json_class(match.arg(toupper(mode),
                       c("EQUALS", "LESS_THAN_OR_EQUAL",
                         "GREATER_THAN_OR_EQUAL")),
             class = "CompareMode")
}

#' @export
search_operator <- function(x = "all") {

  operator <- switch(x,
                     all = "MATCH_ALL_CLAUSES",
                     any = "MATCH_ANY_CLAUSES",
                     x)

  json_class(match.arg(toupper(operator),
                       c("MATCH_ALL_CLAUSES", "MATCH_ANY_CLAUSES")),
             class = "SearchOperator")
}
