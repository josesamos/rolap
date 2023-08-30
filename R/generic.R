
get_measure_names_schema <- function(schema) UseMethod("get_measure_names_schema")

get_attribute_names_schema <- function(schema) UseMethod("get_attribute_names_schema")

snake_case_table <- function(table) UseMethod("snake_case_table")


#' Transform names according to the snake case style
#'
#' For flat tables, transform attribute and measure names according to the snake
#' case style. For star databases, transform fact, dimension, measures, and
#' attribute names according to the snake case style.
#'
#' This style is suitable if we are going to work with databases.
#'
#' @param db A `flat_table` or `star_database` object.
#'
#' @return A `flat_table` or `star_database` object.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   snake_case()
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#'
#' @export
snake_case <- function(db) UseMethod("snake_case")


#' Get the names of the attributes
#'
#' Obtain the names of the attributes in a flat table or a dimension in a star
#' database.
#'
#' If indicated, names can be obtained in alphabetical order or as a vector
#' definition in R
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param ordered A boolean, sort names alphabetically.
#' @param as_definition A boolean, get the names as a vector definition in R.
#'
#' @return A vector of strings or a string, attribute names.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' names <- flat_table('iris', iris) |>
#'   get_attribute_names()
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_attribute_names(name = "where")
#'
#' @export
get_attribute_names <- function(db, name, ordered, as_definition) UseMethod("get_attribute_names")


#' Get the names of the measures
#'
#' Obtain the names of the measures in a flat table or in a star database.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param ordered A boolean, sort names alphabetically.
#' @param as_definition A boolean, get the names as a vector definition in R.
#'
#' @return A vector of strings or a string, measure names.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' names <- flat_table('iris', iris) |>
#'   get_measure_names()
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_measure_names()
#'
#' @export
get_measure_names <- function(db, name, ordered, as_definition) UseMethod("get_measure_names")


#' Rename attributes
#'
#' Rename attributes in a flat table or a dimension in a star database.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, dimension name.
#' @param old A vector of names.
#' @param new A vector of names.
#'
#' @return A `flat_table` or `star_database` object.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_attribute_names(
#'     name = "where",
#'     new = c(
#'       "Region",
#'       "State",
#'       "City"
#'     )
#'   )
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_attribute_names(name = "where",
#'                       old = c("REGION"),
#'                       new = c("Region"))
#'
#' ft <- flat_table('iris', iris) |>
#'   set_attribute_names(
#'     old = c('Species'),
#'     new = c('species'))
#'
#' @export
set_attribute_names <- function(db, name, old, new) UseMethod("set_attribute_names")


#' Rename measures
#'
#' Rename measures in a flat table or in facts in a star database.
#'
#' @param db A `flat_table` or `star_database` object.
#' @param name A string, fact name.
#' @param old A vector of names.
#' @param new A vector of names.
#'
#' @return A `flat_table` or `star_database` object.
#'
#' @family star database and flat table functions
#' @seealso \code{\link{star_database}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_measure_names(
#'     new = c(
#'       "Pneumonia and Influenza",
#'       "All",
#'       "Rows Aggregated"
#'     )
#'   )
#'
#' ft <- flat_table('iris', iris) |>
#'   set_measure_names(
#'     old = c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'),
#'     new = c('pl', 'pw', 'ls', 'sw'))
#'
#' @export
set_measure_names <- function(db, name, old, new) UseMethod("set_measure_names")



