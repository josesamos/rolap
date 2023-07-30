
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
#' @family star database definition functions
#' @seealso \code{\link{mrs_cause_schema}}, \code{\link{mrs_age_schema}}, \code{\link{ft_num}}, \code{\link{ft_age}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' tl1 <- db1 |>
#'   as_tibble_list()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", list(db1, db2))
#' tl <- ct |>
#'   as_tibble_list()
#'
#' @export
as_tibble_list <- function(db) UseMethod("as_tibble_list")


#' Generate a `dm` class with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a `dm` class, as this function does, in this way it can be
#' saved directly in a DBMS.
#'
#'
#' @param db A `star_database` or `constellation` object.
#'
#' @return A `dm` object.
#'
#' @family star database definition functions
#' @seealso \code{\link{mrs_cause_schema}}, \code{\link{mrs_age_schema}}, \code{\link{ft_num}}, \code{\link{ft_age}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' dm1 <- db1 |>
#'   as_dm_class()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", list(db1, db2))
#' dm <- ct |>
#'   as_dm_class()
#'
#' @export
as_dm_class <- function(db) UseMethod("as_dm_class")
