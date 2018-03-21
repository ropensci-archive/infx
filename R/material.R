
#' List materials
#'
#' List all materials available on the queried openBIS instance for a given
#' set of material id objects. Unfortunately there is no way for the available
#' API version to list material id objects. These have to be instantiated by
#' the user, using the function `material_id()` or by searching with
#' `search_openbis()`.
#' 
#' A material id object is defined by a material code and a material type.
#' Furthermore, two different types of material id objects are used for
#' different API sections: `MaterialIdentifierGeneric` and
#' `MaterialIdentifierScreening`. Current types for generic material object
#' ids are
#'   * compound
#'   * control
#'   * esirna
#'   * gene
#'   * mirna
#'   * mirna_inhibitor
#'   * mirna_mimic
#'   * pooled_sirna
#'   * sirna
#' 
#' and for generic screening object ids
#'   * compound
#'   * gene
#'   * oligo
#' 
#' The construction of material codes depends on material type. Genes, for
#' example are identified with Entrez gene ids (e.g. 2475 for MTOR), while for
#' compounds, a manufacturer name is used.
#' 
#' Whenever `list_material()` is dispatched on a (set of) material id
#' object(s), a (set of) `MaterialGeneric` object(s) is returned. However if
#' the dispatch occurs on plate objects (`Plate`, `PlateIdentifier` or
#' `PlateMetadata`), a (set of) `PlateWellMaterialMapping` objects is returned.
#' If `material_type` is not specified (i.e. `NULL`), the `mapping` field in
#' the returned object might contain `NULL` for each well. While not mentioned
#' in the openBis documentation, it is assumed that this occurs in a one to
#' many mapping scenario (e.g. a well being linked to both an oligo and a
#' gene). This can be overcome by specifying the type of the material of
#' interest (see examples).
#' 
#' @inheritParams logout_openbis
#' @param x A (vector of) MaterialIdentifier object(s).
#' @param code The material code for which an id object is created.
#' @param type The material type (possible values depend on mode).
#' @param mode Switch between generic and screening material id objects.
#' @param material_type A `MaterialTypeIdentifierScreening` object to restrict
#' the material listing to a certain type of materials.
#' @param ... Generic compatibility.
#' 
#' @section openBIS:
#' * \Sexpr{infx::docs_link("gis", "getMaterialByCodes")}
#' * \Sexpr{infx::docs_link("sas", "listPlateMaterialMapping")}
#' 
#' @examples
#' \dontrun{
#'   tok <- login_openbis("username", "password")
#'   exp_ids <- list_experiment_ids(tok)
#'   plates <- list_plates(tok, exp_ids[[1]])
#'
#'   # contains (width x height) times NULL in mapping field
#'   mat <- list_material(tok, plates[[2]])
#'   print(mat[[1]], 2L)
#'   
#'   # we now restrict the listing to genes
#'   genes <- json_class(materialTypeCode = "GENE",
#'                       class = "MaterialTypeIdentifierScreening")
#'   
#'   mat <- list_material(tok, plates[[2]], genes)
#'   print(mat[[1]]$mapping[[1]][[1]], 2L)
#' }
#' 
#' @export
#' 
list_material <- function(token, x, ...)
  UseMethod("list_material", x)

#' @rdname list_material
#' @export
#' 
list_material.MaterialIdentifierGeneric <- function(token, x, ...)
  make_request(api_url("gis"), "getMaterialByCodes",
               list(token, as_json_vec(x)))

#' @rdname list_material
#' @export
#' 
list_material.PlateIdentifier <- function(token,
                                          x,
                                          material_type = NULL,
                                          ...) {

  list_plate_mat_map(token, x, material_type)
}

#' @rdname list_material
#' @export
#' 
list_material.PlateMetadata <- function(token, x, material_type = NULL, ...)
  list_plate_mat_map(token, x, material_type)

#' @rdname list_material
#' @export
#' 
list_material.Plate <- function(token, x, material_type = NULL, ...)
  list_plate_mat_map(token, x, material_type)

list_plate_mat_map <- function(token, x, material_type = NULL) {

  if (!is.null(material_type))
    assert_that(is_json_class(material_type),
                has_subclass(material_type, "MaterialTypeIdentifierScreening"))

  make_request(api_url("sas"), "listPlateMaterialMapping",
               list(token, as_json_vec(x), material_type))
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

  type <- toupper(type)
  all_types <- toupper(all_types)

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

as_screening_mat <- function(x, ...) {
  mats <- lapply(as_json_vec(x), function(y)
    material_id(
      code = get_field(y, "materialCode"),
      type = get_field(y[["materialTypeIdentifier"]], "materialTypeCode"),
      mode = "screening"
    ))
  do.call(c, mats)
}

#' @rdname list_material
#' @export
#' 
as_screening_material <- function(x, ...)
  UseMethod("as_screening_material", x)

#' @rdname list_material
#' @export
#' 
as_screening_material.MaterialGeneric <- as_screening_mat

#' @rdname list_material
#' @export
#' 
as_screening_material.MaterialIdentifierGeneric <- as_screening_mat

#' @rdname list_material
#' @export
#' 
as_screening_material.MaterialIdentifierScreening <- function(x, ...)
  x
