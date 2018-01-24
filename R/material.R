
#' List materials
#'
#' List all materials available on the queried openBIS instance for a given
#' set of material id objects. Unfortunately there is no way for the available
#' API version to list material id objects. These have to be instantiated by
#' the user, using the function `material_id()`. A material id object is
#' defined by a material code and a material type. Current types are
#' 
#' * compound
#' * control
#' * esirna
#' * gene
#' * mirna
#' * mirna_inhibitor
#' * mirna_mimic
#' * pooled_sirna
#' * sirna
#' 
#' The construction of material codes depends on material type, for example for
#' genes, the code is an ENTREZ id.
#' 
#' @inheritParams logout_openbis
#' @param x A (vector of) MaterialIdentifier object(s).
#' 
#' @export
#' 
list_material <- function(token, x) {

  if (!is_json_vec(x))
    x <- as_json_vec(x)

  assert_that(all(sapply(x, has_json_subclass, "MaterialIdentifierGeneric")))

  request_openbis("getMaterialByCodes", list(token, x))
}

#' @rdname list_material
#' @export
#' 
material_id <- function(code, type = "gene") {

  if (length(type) == 1L)
    type <- rep(type, times = length(code))

  all_types <- c("compound",
                 "control",
                 "esirna",
                 "gene",
                 "mirna",
                 "mirna_inhibitor",
                 "mirna_mimic",
                 "pooled_sirna",
                 "sirna")

  assert_that(length(code) == length(type),
              all(type %in% all_types))

  res <- mapply(function(x, y) {
    json_class(materialTypeIdentifier =
                 json_class(materialTypeCode = toupper(y),
                            class = "MaterialTypeIdentifierGeneric"),
               materialCode = x,
               class = "MaterialIdentifierGeneric")
  }, code, type, SIMPLIFY = FALSE)

  new_json_vec(res)
}