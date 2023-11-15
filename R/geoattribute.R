#' Get geometry
#'
#' Get the geometry of a layer, as it is interpreted to define a `geolevel`
#' object.
#'
#' It will only be valid if one of the two geometries is interpreted: *point*
#' or *polygon*.
#'
#' @param layer A `sf` object.
#'
#' @return A string.
#'
#' @family star database geographic attributes
#'
#' @examples
#'
#' get_geometry()
#'
#' @export
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
#' If other geographic information has previously been associated with that attribute,
#' the new information is considered and previous instances for which no new information
#' is provided are also added.
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
  validate_dimension_names(db, dimension)
  validate_dimension_attributes(db, dimension, attribute)
  if (is.null(db$geo[[dimension]])) {
    db$geo[[dimension]] <- list()
  }
  geoatt <- get_geoattribute_name(attribute)
  if (is.null(db$geo[[dimension]][[geoatt]])) {
    db$geo[[dimension]][[geoatt]] <- list()
  }
  if (!(is.null(from_layer) | is.null(from_attribute))) {
    stop("Either a from_layer or a from_attribute must be indicated, not both.")
  }
  if (!is.null(from_attribute)) {
    validate_dimension_attributes(db, dimension, from_attribute)
    by <- attribute
    from_layer <- get_layer_from_attribute(db, dimension, attribute, from_attribute)
  }
  if (!(is.null(from_layer) & is.null(by))) {
    db <- define_geoattribute_from_layer(db, dimension, attribute, geoatt, from_layer, by)
  } else {
    stop("A geographic layer or geoattribute must be indicated.")
  }
  db
}

#' Get unrelated instances of a `geoattribute`
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
#' @return A `tibble`.
#'
#' @family star database geographic attributes
#'
#' @examples
#'
#' geo_mrs_db <- mrs_db |>
#'   get_unrelated_instances()
#'
#' @export
get_unrelated_instances <-
  function(db,
           dimension,
           attribute,
           geometry)
    UseMethod("get_unrelated_instances")

#' @rdname get_unrelated_instances
#'
#' @export
get_unrelated_instances.star_database <- function(db,
                                              dimension = NULL,
                                              attribute = NULL,
                                              geometry = NULL) {
  stopifnot("One dimension must be indicated (only one)." = length(dimension) == 1)
  validate_dimension_names(db, dimension)
  validate_dimension_attributes(db, dimension, attribute)
  stopifnot("gometry must be 'point' or 'polygon'." = geometry %in% c("polygon", "point"))
  geoatt <- get_geoattribute_name(attribute)
  stopifnot("That geometry is not defined for the attribute." = !is.null(db$geo[[dimension]][[geoatt]][[geometry]]))
  data_lay <- sf::st_drop_geometry(db$geo[[dimension]][[geoatt]][[geometry]])
  data_dim <- unique(db$dimensions[[dimension]]$table[, attribute])
  out <- dplyr::setdiff(data_dim, data_lay) |>
    dplyr::arrange(tidyselect::all_of(attribute))
  out
}


#' Define geoattribute from a layer
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

  # add new instances to old ones
  if (!is.null(db$geo[[dimension]][[geoatt]][[geometry]])) {
    no_new <- dplyr::setdiff(db$geo[[dimension]][[geoatt]][[geometry]], from_layer)
    if (nrow(no_new > 0)) {
      from_layer <- rbind(from_layer, no_new)
    }
  }
  db$geo[[dimension]][[geoatt]][[geometry]] <- from_layer
  if (is.null(db$geo[[dimension]][[geoatt]][["point"]])) {
    crs <- sf::st_crs(from_layer)
    tryCatch(
      from_layer_point <-
        sf::st_transform(from_layer, 3857) |>
        sf::st_centroid() |>
        sf::st_transform(crs),
      warning = function(w)
        1
    )
    db$geo[[dimension]][[geoatt]][["point"]] <- from_layer_point
  }
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
#' @param geoatt A string, geoattribute name.
#' @param from_attribute A string, attribute name.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
define_geoattribute_from_attribute <- function(db,
                                               dimension = NULL,
                                               attribute = NULL,
                                               geoatt = NULL,
                                               from_attribute = NULL) {
  stopifnot("from_attribute must be indicated." = !is.null(from_attribute))
  validate_dimension_attributes(db, dimension, from_attribute)
  from_geoatt <- get_geoattribute_name(from_attribute)
  stopifnot("No geometry is defined for the attribute." = !is.null(db$geo[[dimension]][[from_geoatt]]))
  geometries <- names(db$geo[[dimension]][[from_geoatt]])

  all_attributes <- union(from_attribute, attribute)
  data_dim <- unique(db$dimensions[[dimension]]$table[, all_attributes])
  if ("polygon" %in% geometries) {
    from_layer <- db$geo[[dimension]][[from_geoatt]][["polygon"]]
    from_layer <- dplyr::inner_join(data_dim, from_layer, by = from_attribute)
    from_layer <- sf::st_as_sf(from_layer)
    from_layer <- from_layer[, attribute]
    from_layer <- from_layer |>
      dplyr::group_by_at(attribute) |>
      dplyr::summarize(.groups = "drop")
    db$geo[[dimension]][[geoatt]][["polygon"]] <- from_layer

    crs <- sf::st_crs(from_layer)
    tryCatch(
      from_layer_point <-
        sf::st_transform(from_layer, 3857) |>
        sf::st_centroid() |>
        sf::st_transform(crs),
      warning = function(w)
        1
    )
    db$geo[[dimension]][[geoatt]][["point"]] <- from_layer_point
  } else {
    from_layer <- db$geo[[dimension]][[from_geoatt]][["point"]]
    from_layer <- dplyr::inner_join(data_dim, from_layer, by = from_attribute)
    from_layer <- sf::st_as_sf(from_layer)
    from_layer <- from_layer[, attribute]
    from_layer <- from_layer |>
      dplyr::group_by_at(attribute) |>
      dplyr::summarize(geometry = sf::st_union(geometry)) |>
      sf::st_centroid()
    db$geo[[dimension]][[geoatt]][["point"]] <- from_layer
  }
  data_lay <- sf::st_drop_geometry(from_layer)
  out <- dplyr::setdiff(data_dim, data_lay)
  if (nrow(out) > 0) {
    warning("Instances of the dimension remain unrelated to the layer. Check them using `get_unrelated_instances()`.")
  }
  db
}


get_layer_from_attribute <- function(db,
                                     dimension = NULL,
                                     attribute = NULL,
                                     from_attribute = NULL) {
  from_geoatt <- get_geoattribute_name(from_attribute)
  stopifnot("No geometry is defined for the attribute." = !is.null(db$geo[[dimension]][[from_geoatt]]))
  geometries <- names(db$geo[[dimension]][[from_geoatt]])
  if ("polygon" %in% geometries) {
    tg <- "polygon"
  } else {
    tg <- "point"
  }
  all_attributes <- union(from_attribute, attribute)
  data_dim <- unique(db$dimensions[[dimension]]$table[, all_attributes])
  from_layer <- db$geo[[dimension]][[from_geoatt]][[tg]]
  from_layer <- dplyr::inner_join(data_dim, from_layer, by = from_attribute)
  from_layer <- sf::st_as_sf(from_layer)
  from_layer <- from_layer[, attribute] |>
    dplyr::group_by_at(attribute)
  if (tg == "polygon") {
    from_layer <- from_layer |>
      dplyr::summarize(.groups = "drop")
  } else {
    from_layer <- from_layer |>
      dplyr::summarize(geometry = sf::st_union(geometry)) |>
      sf::st_centroid()
  }
  from_layer
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
