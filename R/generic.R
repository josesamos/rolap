
get_measure_names <- function(schema) UseMethod("get_measure_names")

get_attribute_names <- function(schema) UseMethod("get_attribute_names")

snake_case_table <- function(table) UseMethod("snake_case_table")

#' Generate a list of tibbles with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a list of tibbles, as this function does.
#'
#'
#' @param db A `star_database` or `constellation` object.
#'
#' @return A list of `tibble`
#'
#' @family star schema and constellation definition functions
#'
#' @examples
#'
#' s <- star_schema() |>
#'   define_facts(fact_schema(
#'     name = "mrs_cause",
#'     measures = c(
#'       "Pneumonia and Influenza Deaths",
#'       "All Deaths"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "when",
#'     attributes = c(
#'       "Year"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "where",
#'     attributes = c(
#'       "REGION",
#'       "State",
#'       "City"
#'     )
#'   ))
#'
#' # ft_num contains instances
#' db <- star_database(s, ft_num) |>
#'   snake_case()
#'
#' tl <- db |>
#'   as_tibble_list()
#'
#' @export
as_tibble_list <- function(db) UseMethod("as_tibble_list")
