#' `fact_table` S3 class
#'
#' A `fact_table` object is created, we have to get its
#' surrogate keys.
#'
#' @param name A string, fact name.
#' @param surrogate_keys A vector of strings, surrogate key names.
#' @param dim_int_names A vector of strings, internal names of dimensions.
#' @param instances A flat table with the fact instances.
#'
#' @return A `fact_table` object.
#' @keywords internal
fact_table <- function(name = NULL, surrogate_keys = NULL, dim_int_names = NULL, instances = NULL) {
  # Check the type of the base object
  stopifnot(tibble::is_tibble(instances))
  stopifnot(!is.null(name))
  stopifnot(!is.null(surrogate_keys))
  stopifnot(!is.null(dim_int_names))

  structure(
    list(
      name = name,
      surrogate_keys = surrogate_keys,
      dim_int_names = dim_int_names,
      table = instances
    ),
    class = "fact_table"
  )
}

#' Transform names according to the snake case style
#'
#' @param table A `fact_table` object.
#'
#' @return A `fact_table` object.
#'
#' @keywords internal
snake_case_table.fact_table <- function(table) {
  table$name <- snakecase::to_snake_case(table$name)
  names(table$table) <- snakecase::to_snake_case(names(table$table))
  table
}

