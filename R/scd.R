


s <- star_schema() |>
  define_scd(name = "Where",
             natural_key = c("COD_PRO",
                             "COD_MUN")) |>
  define_type_0(name = "Where",
                attributes = c("NAME",
                               "COD_MUN")) |>
  define_type_1(name = "Where",
                attributes = c("NAME",
                               "COD_MUN")) |>
  define_type_3(name = "Where",
                current = "NAME",
                previous = "COD_MUN") |>
  define_type_6(name = "Where",
                current = "NAME",
                previous = "COD_MUN") |>
  define_time_stamp(
    name = "Where",
    from = "DATE",
    to = "DATE_END",
    value_from = NULL
  ) |>
  define_version(
    name = "Where",
    version = "version",
    current = "current",
    expired = "expired"
  )

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
#' If defined from another attribute, it should have the same or finer granularity,
#' to obtain the result by grouping its instances. The considered attribute can be
#' the pair that defines longitude and latitude.
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
#' @param from_attribute A vector, attribute names.
#'
#' @return A `star_database` object.
#'
#' @family star database geographic attributes
#'
#' @examples
#'
#' db <- mrs_db |>
#'   define_scd(
#'     dimension = "where",
#'     attribute = "state",
#'     from_layer = us_layer_state,
#'     by = "STUSPS"
#'   ) |>
#'   define_scd(
#'     dimension = "where",
#'     attribute = "region",
#'     from_attribute = "state"
#'   )  |>
#'   define_scd(
#'     dimension = "where",
#'     attribute = "city",
#'     from_attribute = c("long", "lat")
#'   )
#'
#' @export
define_scd <-
  function(db,
           dimension,
           attribute,
           from_layer,
           by,
           from_attribute)
    UseMethod("define_scd")

#' @rdname define_scd
#'
#' @export
define_scd.star_database <- function(db,
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
    db <- define_scd_from_layer(db, dimension, attribute, geoatt, from_layer, by)
  } else {
    stop("A geographic layer or geoattribute must be indicated.")
  }
  db
}

