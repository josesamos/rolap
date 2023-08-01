
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
#' @family star database and constellation exportation functions
#' @seealso \code{\link{star_database}}, \code{\link{constellation}}
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
#' @param pk_facts A boolean, include primary key in fact tables.
#'
#' @return A `dm` object.
#'
#' @family star database and constellation exportation functions
#' @seealso \code{\link{star_database}}, \code{\link{constellation}}
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
as_dm_class <- function(db, pk_facts) UseMethod("as_dm_class")
