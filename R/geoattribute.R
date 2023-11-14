
#' Define `geoattribute` of a dimension
#'
#' Define a set of attributes as a dimension's `geoattribute`. The set of attribute
#' values must uniquely designate the instances of the given geographic layer.
#'
#' The definition can be done in two ways: Associates the instances of the attributes
#' with the instances of a geographic layer or defines it from the geometry of
#' previously defined geographic attributes. Multiple attributes can be specified in the
#' `attribute` parameter.
#'
#' If defined from a layer (`from_layer` parameter), additionally the attributes
#' used for the join between the tables (dimension and layer tables) must be
#' indicated (`by` parameter).
#'
#' If defined from another attribute, it should have a finer granularity, to
#' obtain the result by grouping its instances.
#'
#'
#' @param db A `star_database` object.
#' @param dimension A string, dimension name.
#' @param attribute A vector, attribute names.
#' @param from_layer A `sf` object.
#' @param by a vector of correspondence of attributes of the dimension with the
#'   `sf` layer structure.
#' @param from_attribute A string, attribute name.
#'
#' @return A `star_database` object.
#'
#' @family star database geographic attributes
#'
#' @examples
#'
#' geo_mrs_db <- mrs_db |>
#'   define_geoattribute()
#'
#' @export
define_geoattribute <-
  function(db,
           dimension,
           attribute,
           from_layer,
           by,
           from_attribute)
    UseMethod("define_geoattribute")

#' @rdname define_geoattribute
#'
#' @export
define_geoattribute.star_database <- function(db,
                                              dimension = NULL,
                                              attribute = NULL,
                                              from_layer = NULL,
                                              by = NULL,
                                              from_attribute = NULL) {
  stopifnot("One dimension must be indicated (only one)." = length(dimension) == 1)
  dimension <- validate_dimension_names(db, dimension)
  attributes <- validate_dimension_attributes(db, dimension, attribute)
  if (is.null(db$geo[[dimension]])) {
    db$geo[[dimension]] <- list()
  }
  geoatt <- get_geoattribute_name(attribute)
  if (is.null(db$geo[[dimension]][[geoatt]])) {
    db$geo[[dimension]][[geoatt]] <- list()
  }
  if (!(is.null(from_layer) | is.null(by))) {
    db <- define_geoattribute_from_layer(db, dimension, attribute, geoatt, from_layer, by)
  } else if (!is.null(from_attribute)) {
    db <- define_geoattribute_from_attribute(db, dimension, attribute, from_attribute)
  } else {
    stop("A geographic layer or geoattribute must be indicated.")
  }
  db
}


#' Define an attribute from a layer
#'
#' Define an attribute from a layer.
#'
#' @importFrom rlang :=
#' @param db A `star_database` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#' @param geoatt A string, geoattribute name.
#' @param from_layer A `sf` object
#' @param by a vector of correspondence of attributes of the dimension with the
#'   `sf` structure.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
define_geoattribute_from_layer <- function(db,
                                           dimension = NULL,
                                           attribute = NULL,
                                           geoatt = NULL,
                                           from_layer = NULL,
                                           by = NULL) {
  stopifnot("We must select the same number of attributes in the dimension as in the layer." = length(attribute) == length(by))
  validate_attributes(colnames(from_layer), by)
  geometry <- get_geometry(from_layer)
  if (!(geometry %in% c("polygon", "point"))) {
    stop(sprintf('from_layer has unsupported geometry: %s.', geometry[1]))
  }
  if (is.null(db$geo[[dimension]][[geoatt]][[geometry]])) {
    db$geo[[dimension]][[geoatt]][[geometry]] <- list()
  }
  from_layer <- from_layer[, by]
  from_layer <- from_layer |>
    dplyr::group_by_at(by) |>
    dplyr::summarize(.groups = "drop")
  names <- names(from_layer)
  for (i in seq_along(by)) {
    j <- which(names == by[i])
    names[j] <- attribute[i]
  }
  names(from_layer) <- names
  db$geo[[dimension]][[geoatt]][[geometry]] <- from_layer
  data_lay <- sf::st_drop_geometry(from_layer)
  data_dim <- unique(db$dimensions[[dimension]]$table[, attribute])
  out <- dplyr::setdiff(data_dim, data_lay)
  if (nrow(out) > 0) {
    warning("Instances of the dimension remain unrelated to the layer. Check them using `get_unrelated_instances()`.")
  }
  db
}

#' Define a geoattribute from another
#'
#' Define a geoattribute from another.
#'
#' @importFrom rlang :=
#' @param db A `star_database` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#' @param from_attribute A string, attribute name.
#' @param additional_attributes A vector, attribute names.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
define_geoattribute_from_attribute <- function(db,
                                               dimension = NULL,
                                               attribute = NULL,
                                               from_attribute = NULL,
                                               additional_attributes = NULL) {
  stopifnot("The from_attribute must be indicated." = !is.null(from_attribute))
  validate_names(names(db$geodimension[[dimension]]), from_attribute, concept = 'from attribute')

  geom <- db$geodimension[[dimension]][[from_attribute]]
  stopifnot("The geom of the from attribute must be defined." = !is.null(geom))

  if (attribute == sprintf("all_%s", dimension)) {
    db$geodimension[[dimension]][[attribute]] <- as.data.frame(geom) |>
      dplyr::mutate(!!attribute := attribute, .before = tidyselect::all_of(from_attribute)) |>
      sf::st_as_sf() |>
      dplyr::group_by_at(attribute) |>
      dplyr::summarize(.groups = "drop")
    attr(db$geodimension[[dimension]][[attribute]], 'n_instances') <- 1
  } else {
    names_geom <- names(geom)
    names_geom <- names_geom[-length(names_geom)]
    atts <- unique(c(attribute, additional_attributes))
    layer <- geom |>
      dplyr::left_join(db$dimension[[dimension]], by = names_geom) |>
      dplyr::select(tidyselect::all_of(atts)) |>
      dplyr::group_by_at(atts) |>
      dplyr::summarize(.groups = "drop")

    db$geodimension[[dimension]][[attribute]] <- layer
    attr(db$geodimension[[dimension]][[attribute]], 'n_instances') <-
      nrow(layer)
  }
  db
}


#' Get geometry
#'
#' Get the geometry of a layer, as it is interpreted to define a `geolevel`
#' object.
#'
#' It will only be valid if one of the three geometries is interpreted: *point*
#' or *polygon*.
#'
#' @param layer A `sf` object.
#'
#' @return A string.
#'
#' @keywords internal
get_geometry <- function(layer) {
  geo <- unique(as.character(sf::st_geometry_type(layer, by_geometry = TRUE)))
  if (length(intersect(geo, c("CIRCULARSTRING", "CURVEPOLYGON", "MULTIPOLYGON", "TRIANGLE", "POLYGON"))) > 0) {
    return("polygon")
  } else if (length(intersect(geo, c("LINESTRING", "MULTILINESTRING", "CURVE", "MULTICURVE", "COMPOUNDCURVE"))) > 0) {
    return("line")
  } else if (length(intersect(geo, c("POINT", "MULTIPOINT"))) > 0) {
    return("point")
  }
  geo
}


#' Get geoattribute name
#'
#' Get the name of the geoattribute from a vector of attribute names
#'
#' @param attribute A vector, attribute names.
#'
#' @return A string.
#'
#' @keywords internal
get_geoattribute_name <- function(attribute) {
  attribute <- snakecase::to_snake_case(attribute)
  attribute <- sort(attribute)
  attribute <- paste(attribute, collapse = "<|>", sep = "")
  attribute
}
