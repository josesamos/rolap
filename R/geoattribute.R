#' Transform coordinates to point geometry
#'
#' From the coordinates defined in fields such as latitude and longitude, it
#' returns a layer of points.
#'
#' If we start from a geographic layer, it initially transforms it into a table.
#'
#' The CRS of the new layer is indicated. If a CRS is not indicated, it
#' considers the layer's CRS by default and, if it is not a layer, it considers
#' 4326 CRS (WGS84).
#'
#' @param table A `tibble` object.
#' @param lon_lat A vector, name of longitude and latitude attributes.
#' @param crs A coordinate reference system: integer with the EPSG code, or
#'   character with proj4string.
#'
#' @return A `sf` object.
#'
#' @family level definition functions
#'
#' @examples
#'
#' us_state_point <-
#'   coordinates_to_point(us_layer_state,
#'                        lon_lat = c("INTPTLON", "INTPTLAT"))
#'
#' @export
coordinates_to_point <- function(table, lon_lat = c("intptlon", "intptlat"), crs = NULL) {
  if ("sf" %in% class(table)) {
    if (is.null(crs)) {
      crs <- sf::st_crs(table)
    }
    table <- tibble::tibble((sf::st_drop_geometry(table)))
  }
  lon_lat <- unique(lon_lat)
  stopifnot("Two attributes must be indicated: longitude and latitude." = length(lon_lat) == 2)
  names <- names(table)
  lon <- grep(lon_lat[1], names, ignore.case = TRUE)
  lat <- grep(lon_lat[2], names, ignore.case = TRUE)
  stopifnot("Two attributes of the table must be indicated." = length(lon) > 0 & length(lat) > 0)
  if (is.null(crs)) {
    crs <- 4326 # WGS84
  }

  table |>
    sf::st_as_sf(
      coords = names[c(lon, lat)],
      crs = crs,
      agr = "constant",
      remove = TRUE
    )
}



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
#' geometry <- get_geometry(us_layer_state)
#'
#' @export
get_geometry <- function(layer) {
  layer <- sf::st_as_sf(layer)
  res <- unique(as.character(sf::st_geometry_type(layer, by_geometry = TRUE)))
  if (length(intersect(res, c("CIRCULARSTRING", "CURVEPOLYGON", "MULTIPOLYGON", "TRIANGLE", "POLYGON"))) > 0) {
    return("polygon")
  } else if (length(intersect(res, c("LINESTRING", "MULTILINESTRING", "CURVE", "MULTICURVE", "COMPOUNDCURVE"))) > 0) {
    return("line")
  } else if (length(intersect(res, c("POINT", "MULTIPOINT"))) > 0) {
    return("point")
  }
  res
}


#' Summarize geometry of a layer
#'
#' Groups the geometric elements of a layer according to the values of the indicated
#' attribute.
#'
#' @param layer A `sf` object.
#' @param attribute A string, attribute name.
#'
#' @return A `sf` object.
#'
#' @family star database geographic attributes
#'
#' @examples
#'
#' layer <-
#'   summarize_layer(us_layer_state, "REGION")
#'
#' @export
summarize_layer <- function(layer, attribute) {
  geometry <- get_geometry(layer)
  if (!(geometry %in% c("polygon", "point"))) {
    stop(sprintf('layer has unsupported geometry: %s.', geometry[1]))
  }
  layer <- layer|>
    dplyr::group_by_at(attribute)
  if (geometry == "polygon") {
    layer <- layer |>
      dplyr::summarize(.groups = "drop")
  } else {
    geocol <- attr(layer, "sf_column")
    layer <- layer |>
      dplyr::summarize(geom = sf::st_union(eval(parse(text = geocol)))) |>
      sf::st_centroid()
  }
  layer
}


#' Get point geometry
#'
#' Obtain point geometry from polygon geometry using the centroid.
#'
#' @param layer A `sf` object.
#'
#' @return A `sf` object.
#'
#' @family star database geographic attributes
#'
#' @examples
#'
#' layer <-
#'   get_point_geometry(us_layer_state)
#'
#' @export
get_point_geometry <- function(layer) {
  geometry <- get_geometry(layer)
  if (geometry == "polygon") {
    # suppress warning message
    sf::st_agr(layer) = "constant"
    crs <- sf::st_crs(layer)
    layer <-
      sf::st_transform(layer, 3857) |>
      sf::st_centroid() |>
      sf::st_transform(crs)
  } else {
    stop("The geometry of the layer must be polygon.")
  }
  layer
}


#' Get unrelated instances of a `geoattribute`
#'
#' We obtain the values of the dimension attribute that do not have an associated
#' geographic element of the indicated geometry.
#'
#' @param db A `star_database` object.
#' @param dimension A string, dimension name.
#' @param attribute A vector, attribute names.
#' @param geometry A string, geometry name ('point' or 'polygon').
#'
#' @return A `tibble`.
#'
#' @family star database geographic attributes
#'
#' @examples
#'
#' db <- mrs_db |>
#'   define_geoattribute(
#'     dimension = "where",
#'     attribute = "state",
#'     from_layer = us_layer_state,
#'     by = "STUSPS"
#'   )
#'
#' instances <- get_unrelated_instances (db,
#'                                       dimension = "where",
#'                                       attribute = "state")
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
                                                  geometry = "polygon") {
  stopifnot("One dimension must be indicated (only one)." = length(dimension) == 1)
  validate_dimension_names(db, dimension)
  validate_dimension_attributes(db, dimension, attribute)
  stopifnot("gometry must be 'point' or 'polygon'." = geometry %in% c("polygon", "point"))
  geoatt <- get_geoattribute_name(attribute)
  stopifnot("That geometry is not defined for the attribute." = !is.null(db$geo[[dimension]][[geoatt]][[geometry]]))
  data_lay <- sf::st_drop_geometry(db$geo[[dimension]][[geoatt]][[geometry]])
  data_dim <- unique(db$dimensions[[dimension]]$table[, attribute])
  out <- dplyr::setdiff(data_dim, data_lay) |>
    dplyr::arrange()
  out
}


#' Define `geoattribute` of a dimension
#'
#' Define a set of attributes as a dimension's `geoattribute`. The set of attribute
#' values must uniquely designate the instances of the given geographic layer.
#'
#' The definition can be done in two ways: Associates the instances of the attributes
#' with the instances of a geographic layer or defines it from the geometry of
#' previously defined geographic attributes.
#'
#' Multiple attributes can be specified in the `attribute` parameter, the geographical
#' attribute is the combination of all of them.
#'
#' If defined from a layer (`from_layer` parameter), additionally the attributes
#' used for the join between the tables (dimension and layer tables) must be
#' indicated (`by` parameter).
#'
#' If defined from another attribute, it should have a finer granularity, to obtain
#' the result by grouping its instances.
#'
#' If other geographic information has previously been associated with that attribute,
#' the new information is considered and previous instances for which no new information
#' is provided are also added.
#'
#' If the geometry provided is polygons, a point layer is also generated.
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
#' db <- mrs_db |>
#'   define_geoattribute(
#'     dimension = "where",
#'     attribute = "state",
#'     from_layer = us_layer_state,
#'     by = "STUSPS"
#'   ) |>
#'   define_geoattribute(
#'     dimension = "where",
#'     attribute = "region",
#'     from_attribute = "state"
#'   )
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
  if (!(is.null(from_layer) | is.null(by))) {
    db <- define_geoattribute_from_layer(db, dimension, attribute, geoatt, from_layer, by)
  } else {
    stop("A geographic layer or geoattribute must be indicated.")
  }
  db
}


#' Get layer from attribute
#'
#' Gets the geographic layer associated with the from_attribute at the level of
#' the indicated attributes.
#'
#' @param db A `star_database` object.
#' @param dimension A string, dimension name.
#' @param attribute A string, attribute name.
#' @param from_attribute A string, attribute name.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
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
  summarize_layer(from_layer[, attribute], attribute)
}


#' Define geoattribute from a layer
#'
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
  stopifnot(
    "We must select the same number of attributes in the dimension as in the layer." = length(attribute) == length(by)
  )
  validate_attributes(colnames(from_layer), by)
  geometry <- get_geometry(from_layer)
  if (!(geometry %in% c("polygon", "point"))) {
    stop(sprintf('from_layer has unsupported geometry: %s.', geometry[1]))
  }
  from_layer <- summarize_layer(from_layer[, by], by)
  names <- names(from_layer)
  for (i in seq_along(by)) {
    j <- which(names == by[i])
    names[j] <- attribute[i]
  }
  names(from_layer) <- names

  # add old instances to new ones
  if (!is.null(db$geo[[dimension]][[geoatt]][[geometry]])) {
    old_instances <-
      sf::st_drop_geometry(db$geo[[dimension]][[geoatt]][[geometry]])
    layer_instances <- sf::st_drop_geometry(from_layer)
    old_instances <- dplyr::setdiff(old_instances, layer_instances)
    if (nrow(old_instances > 0)) {
      old_instances <-
        dplyr::inner_join(old_instances, db$geo[[dimension]][[geoatt]][[geometry]], by = attribute)
      old_instances <- sf::st_as_sf(old_instances)
      from_layer <- rbind(from_layer, old_instances)
    }
  }
  db$geo[[dimension]][[geoatt]][[geometry]] <- from_layer
  if (geometry == "polygon") {
    from_layer_point <- get_point_geometry(from_layer)
    if (is.null(db$geo[[dimension]][[geoatt]][["point"]])) {
      db$geo[[dimension]][[geoatt]][["point"]] <- from_layer_point
    } else {
      old_instances <-
        sf::st_drop_geometry(db$geo[[dimension]][[geoatt]][["point"]])
      layer_instances <- sf::st_drop_geometry(from_layer_point)
      layer_new_instances <-
        dplyr::setdiff(layer_instances, old_instances)
      if (nrow(layer_new_instances > 0)) {
        layer_new_instances <-
          dplyr::inner_join(layer_new_instances, from_layer_point, by = attribute)
        layer_new_instances <- sf::st_as_sf(layer_new_instances)
        db$geo[[dimension]][[geoatt]][["point"]] <-
          rbind(db$geo[[dimension]][[geoatt]][["point"]], layer_new_instances)
      }
    }
  }
  data_lay <- sf::st_drop_geometry(from_layer)
  data_dim <- unique(db$dimensions[[dimension]]$table[, attribute])
  out <- dplyr::setdiff(data_dim, data_lay)
  if (nrow(out) > 0) {
    warning(
      "Instances of the dimension remain unrelated to the layer. Check them using `get_unrelated_instances()`."
    )
  }
  db
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

#' From geodimensions, leave only contained in vector of names
#'
#' @param db A `star_database` object.
#' @param names A vector of strings, dimension names.
#'
#' @return A list of geodimensions.
#'
#' @keywords internal
filter_geo_dimensions <- function(db, dim) {
  geo <- list()
  for (d in dim) {
    if (!is.null(db$geo[[d]])) {
      geo[[d]] <- db$geo[[d]]
    }
  }
  geo
}



#' Integrate two geodimensions
#'
#' @param gd1 A geodimension.
#' @param gd2 A geodimension.
#'
#' @return A geodimension.
#'
#' @keywords internal
integrate_geo_dimensions <- function(gd1, gd2) {
  for (geoatt in names(gd2)) {
    if (!(geoatt %in% names(gd1))) {
      gd1[[geoatt]] <- gd2[[geoatt]]
    } else {
      for(geo in names(gd2[[geoatt]])) {
        if (!(geo %in% names(gd1[[geoatt]]))) {
          gd1[[geoatt]][[geo]] <- gd2[[geoatt]][[geo]]
        } else {
          t1 <- sf::st_drop_geometry(gd1[[geoatt]][[geo]])
          t2 <- sf::st_drop_geometry(gd2[[geoatt]][[geo]])
          td <- dplyr::setdiff(t2, t1)
          if (nrow(td > 0)) {
            td <- dplyr::inner_join(td, gd2[[geoatt]][[geo]], by = names(td))
            td <- sf::st_as_sf(td)
            gd1[[geoatt]][[geo]] <- rbind(gd1[[geoatt]][[geo]], td)
          }
        }
      }
    }
  }
  gd1
}
