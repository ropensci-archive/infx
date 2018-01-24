
#' List materials
#'
#' List all materials available on the queried openBIS instance for a given
#' set of material id objects. Unfortunately there is no way for the available
#' API version to list material id objects. These have to be instantiated by
#' the user, using the function `material_id()`. A material id object is
#' defined by a material code and a material type. Furthermore, two different
#' types of material id objects are used for different API sections:
#' `MaterialIdentifierGeneric` and `MaterialIdentifierScreening`. Current
#' types for generic material object ids are
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
#' and for generic screening object ids
#' 
#' * compound
#' * gene
#' * oligo
#' 
#' The construction of material codes depends on material type. Genes, for
#' example are identified with Entrez gene ids (e.g. 2475 for MTOR), while for
#' compounds, a manufacturer name is used.
#' 
#' @inheritParams logout_openbis
#' @param x A (vector of) MaterialIdentifier object(s).
#' @param type The material type (possible values depend on mode).
#' @param mode Switch between generic and screening material id objects.
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
material_id <- function(code,
                        type = "gene",
                        mode = c("generic", "screening")) {

  mode <- match.arg(mode)

  if (length(type) == 1L)
    type <- rep(type, times = length(code))

  all_types <- switch(mode,
                      generic = c("compound",
                                  "control",
                                  "esirna",
                                  "gene",
                                  "mirna",
                                  "mirna_inhibitor",
                                  "mirna_mimic",
                                  "pooled_sirna",
                                  "sirna"),
                      screening = c("compound",
                                    "gene",
                                    "oligo"))

  assert_that(length(code) == length(type),
              all(type %in% all_types))

  if (mode == "generic")
    suffix <- "IdentifierGeneric"
  else
    suffix <- "IdentifierScreening"

  res <- mapply(function(x, y) {
    json_class(materialTypeIdentifier =
                 json_class(materialTypeCode = toupper(y),
                            class = paste0("MaterialType", suffix)),
               materialCode = x,
               class = paste0("Material", suffix))
  }, code, type, SIMPLIFY = FALSE)

  new_json_vec(res)
}