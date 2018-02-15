
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
                            operator = "all",
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

  assert_that(all(sapply(clauses, is_clause)))

  operator <- select_search_operator(operator)

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
                   mode = "eq",
                   ...) {

  clause_type <- match.arg(clause_type)

  field_type <- if (is.null(field_type))
    select_field_type(switch(clause_type,
                             PropertyMatchClause = "property",
                             AnyPropertyMatchClause = "any_property",
                             AnyFieldMatchClause = "any_field",
                             AttributeMatchClause = "attribute",
                             TimeAttributeMatchClause = "attribute",
                             stop("Specify field_type for MatchClause.")))
  else
    select_field_type(field_type)

  mode <- select_compare_mode(mode)

  assert_that(is.character(desired_value), length(desired_value) == 1L)

  if (is.null(field_code)) {
    json_class(fieldType = field_type, desiredValue = desired_value,
               compareMode = mode, ..., class = clause_type)
  } else {
    assert_that(is.character(field_code), length(field_code) == 1L)
    json_class(fieldType = field_type, fieldCode = field_code,
               desiredValue = desired_value, compareMode = mode, ...,
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
attribute_clause <- function(desired_value, attribute = "code")
  clause(desired_value, "AttributeMatchClause",
         attribute = select_attribute(attribute))

#' @export
time_attribute_clause <- function(desired_date = Sys.Date(),
                                  attribute = "registration",
                                  timezone = 0L,
                                  mode = "eq") {

  assert_that(is.integer(timezone), timezone <= 12L, timezone >= -12L,
              inherits(desired_date, "Date"))

  clause(format(desired_date, "%Y-%m-%d"), "TimeAttributeMatchClause",
         mode = mode, timeZone = timezone,
         attribute = select_time_attribute(attribute))
}

#' @export
select_field_type <- function(x = "property")
  match.arg(toupper(x),
            c("PROPERTY", "ATTRIBUTE", "ANY_FIELD", "ANY_PROPERTY"))

#' @export
select_attribute <- function(x = "code")
  match.arg(toupper(x),
            c("CODE", "TYPE", "PERM_ID", "SPACE", "PROJECT", "PROJECT_PERM_ID",
              "METAPROJECT", "REGISTRATOR_USER_ID", "REGISTRATOR_FIRST_NAME",
              "REGISTRATOR_LAST_NAME", "REGISTRATOR_EMAIL", "MODIFIER_USER_ID",
              "MODIFIER_FIRST_NAME", "MODIFIER_LAST_NAME", "MODIFIER_EMAIL"))

#' @export
select_time_attribute <- function(x = "registration")
  match.arg(toupper(x), c("REGISTRATION_DATE", "MODIFICATION_DATE"))

#' @export
select_compare_mode <- function(x = "eq") {

  mode <- switch(x,
                 eq = "EQUALS",
                 lte = "LESS_THAN_OR_EQUAL",
                 gte = "GREATER_THAN_OR_EQUAL",
                 x)

  match.arg(toupper(mode),
            c("EQUALS", "LESS_THAN_OR_EQUAL", "GREATER_THAN_OR_EQUAL"))
}

#' @export
select_search_operator <- function(x = "all") {

  operator <- switch(x,
                     all = "MATCH_ALL_CLAUSES",
                     any = "MATCH_ANY_CLAUSES",
                     x)

  match.arg(toupper(operator), c("MATCH_ALL_CLAUSES", "MATCH_ANY_CLAUSES"))
}
