
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
                            operator = search_operator("all"),
                            sub_criteria = NULL) {

  if (is_json_class(clauses))
    clauses <- list(clauses)

  clause_types <- c("MatchClause", "PropertyMatchClause",
                    "AnyPropertyMatchClause", "AnyFieldMatchClause",
                    "AttributeMatchClause", "TimeAttributeMatchClause")

  assert_that(all(sapply(clauses, function(x) is_json_class(x) &&
                                    get_subclass(x) %in% clause_types)),
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
match_clause <- function(field_code,
                         desired_value,
                         field_type = c("property", "attribute", "any_field",
                                        "any_property"),
                         compare_mode = c("eq", "lte", "gte")) {

  field_type <- toupper(match.arg(field_type))
  compare_mode <- switch(match.arg(compare_mode),
                         eq = "EQUALS",
                         lte = "LESS_THAN_OR_EQUAL",
                         gte = "GREATER_THAN_OR_EQUAL")

  assert_that(is.character(field_code), length(field_code) == 1L,
              is.character(desired_value), length(desired_value) == 1L)

  json_class(fieldType = field_type, fieldCode = field_code,
             desiredValue = desired_value, compareMode = compare_mode,
             class = "MatchClause")
}

#' @export
property_match_clause <- function(desired_value,
                                  property_code) {

  assert_that(is.character(property_code), length(property_code) == 1L,
              is.character(desired_value), length(desired_value) == 1L)

  json_class(propertyCode = property_code, desiredValue = desired_value,
             class = "PropertyMatchClause")
}

#' @export
any_property_match_clause <- function(desired_value) {

  assert_that(is.character(desired_value), length(desired_value) == 1L)

  json_class(desiredValue = desired_value,
             class = "AnyPropertyMatchClause")
}

#' @export
any_field_match_clause <- function(desired_value) {

  assert_that(is.character(desired_value), length(desired_value) == 1L)

  json_class(desiredValue = desired_value,
             class = "AnyFieldMatchClause")
}

#' @export
attribute_match_clause <- function(desired_value,
                                   attribute = match_clause_attribute()) {

  assert_that(is.character(desired_value), length(desired_value) == 1L,
              has_subclass(attribute, "MatchClauseAttribute"))

  json_class(attribute = as.character(as_json_class(attribute)),
             desiredValue = desired_value,
             class = "AttributeMatchClause")
}

#' @export
time_attribute_match_clause <- function(
  desired_date = Sys.Date(),
  attribute = match_clause_time_attribute(),
  timezone = 0L,
  compare_mode = compare_mode("lte")) {

  assert_that(has_subclass(attribute, "MatchClauseTimeAttribute"),
              has_subclass(compare_mode, "CompareMode"),
              is.integer(timezone), timezone <= 12L, timezone >= -12L,
              inherits(desired_date, "date"))

  json_class(attribute = as.character(as_json_class(attribute)),
             desiredDate = format(desired_date, "%Y-%m-%d"),
             timezone = timezone,
             mode = as.character(as_json_class(compare_mode)),
             class = "TimeAttributeMatchClause")
}

#' @export
match_clause_field_type <- function(field_type = "property") {

  json_class(match.arg(toupper(field_type),
                       c("PROPERTY", "ATTRIBUTE", "ANY_FIELD",
                         "ANY_PROPERTY")),
             class = "MatchClauseFieldType")
}

#' @export
match_clause_attribute <- function(attribute = "code") {

  json_class(match.arg(toupper(attribute),
                       c("CODE", "TYPE", "PERM_ID", "SPACE", "PROJECT",
                         "PROJECT_PERM_ID", "METAPROJECT",
                         "REGISTRATOR_USER_ID", "REGISTRATOR_FIRST_NAME",
                         "REGISTRATOR_LAST_NAME", "REGISTRATOR_EMAIL",
                         "MODIFIER_USER_ID", "MODIFIER_FIRST_NAME",
                         "MODIFIER_LAST_NAME", "MODIFIER_EMAIL")),
             class = "MatchClauseAttribute")
}

#' @export
match_clause_time_attribute <- function(attribute = "registration") {

  json_class(match.arg(toupper(attribute),
                       c("REGISTRATION_DATE", "MODIFICATION_DATE")),
             class = "MatchClauseTimeAttribute")
}

#' @export
compare_mode <- function(mode = "eq") {

  mode <- switch(mode,
                 eq = "EQUALS",
                 lte = "LESS_THAN_OR_EQUAL",
                 gte = "GREATER_THAN_OR_EQUAL",
                 mode)

  json_class(match.arg(toupper(mode),
                       c("EQUALS", "LESS_THAN_OR_EQUAL",
                         "GREATER_THAN_OR_EQUAL")),
             class = "CompareMode")
}

#' @export
search_operator <- function(operator = "all") {

  operator <- switch(operator,
                     all = "MATCH_ALL_CLAUSES",
                     any = "MATCH_ANY_CLAUSES",
                     operator)

  json_class(match.arg(toupper(operator),
                       c("MATCH_ALL_CLAUSES", "MATCH_ANY_CLAUSES")),
             class = "SearchOperator")
}
