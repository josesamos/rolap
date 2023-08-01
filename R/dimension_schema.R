#' `dimension_schema` S3 class
#'
#' A `dimension_schema` object is created, we have to define its name and the
#' set of attributes that make it up.
#'
#' A `dimension_schema` object is part of a `star_schema` object, defines
#' a dimension of the star schema.
#'
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `dimension_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' d <- dimension_schema(
#'   name = "when",
#'   attributes = c(
#'     "Week Ending Date",
#'     "WEEK",
#'     "Year"
#'   )
#' )
#'
#' @export
dimension_schema <- function(name = NULL, attributes = NULL) {
  stopifnot(!is.null(name))
  stopifnot(length(attributes) > 0)
  stopifnot(length(attributes) == length(unique(attributes)))
  structure(list(name = name, attributes = attributes), class = "dimension_schema")
}

# generic
get_dimension_name <- function(schema) UseMethod("get_dimension_name")


#' Get dimension name
#'
#' Get the dimension name.
#'
#' @param schema A `dimension_schema` object.
#'
#' @return A string.
#'
#' @keywords internal
get_dimension_name.dimension_schema <- function(schema) {
  schema$name
}


#' Get attribute names
#'
#' Get the names of the attributes defined in the dimension schema.
#'
#' @param schema A `dimension_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_attribute_names.dimension_schema <- function(schema) {
  schema$attributes
}
