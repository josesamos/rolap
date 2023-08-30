
get_measure_names <- function(schema) UseMethod("get_measure_names")

get_attribute_names <- function(schema) UseMethod("get_attribute_names")

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
#' @family star database and flat table definition functions
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

