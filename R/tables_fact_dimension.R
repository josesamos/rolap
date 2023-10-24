#' `dimension_table` S3 class
#'
#' A `dimension_table` object is created, we have to define its
#' surrogate key.
#'
#' @param name A string, dimension name.
#' @param attributes A vector of strings, attributes names.
#' @param instances A flat table with the dimension instances.
#'
#' @return A `dimension_table` object.
#' @keywords internal
dimension_table <- function(name = NULL, attributes = NULL, instances = NULL) {
  # Check the type of the base object
  stopifnot("A tibble with the instances was expected." = tibble::is_tibble(instances))
  stopifnot("Missing table name." = !is.null(name))
  stopifnot("Missing table attributes." = !is.null(attributes))

  ft <- instances[, attributes]
  # remove duplicates and sort
  ft <- dplyr::arrange_all(unique(ft))
  # add surrogate primary key
  # := variables for parameter names
  # !! expands the expression into a string
  surrogate_key = sprintf("%s_key", snakecase::to_snake_case(name))
  ft <- tibble::add_column(ft,!!surrogate_key := 1:nrow(ft), .before = 1)

  structure(
    list(
      name = name,
      surrogate_key = surrogate_key,
      table = ft
    ),
    class = "dimension_table"
  )
}

# generic
snake_case_table <- function(table) UseMethod("snake_case_table")
add_surrogate_key <- function(dimension_table, instances) UseMethod("add_surrogate_key")
get_surrogate_key <- function(dimension_table) UseMethod("get_surrogate_key")



#' Add the surrogate key from a dimension table to the instances table.
#'
#' @param dimension_table A `dimension_table` object.
#' @param instances A `tibble`, the instances table.
#'
#' @return A `tibble`.
#' @keywords internal
add_surrogate_key.dimension_table <- function(dimension_table, instances) {
  attributes <- colnames(dimension_table$table)
  attributes <- attributes[attributes != dimension_table$surrogate_key]
  dplyr::inner_join(instances, dimension_table$table, by = attributes)
}


#' Get surrogate key names
#'
#' Get the names of the surrogate keys defined in the dimension table.
#'
#' @param dimension_table A `dimension_table` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_surrogate_key.dimension_table <- function(dimension_table) {
  dimension_table$surrogate_key
}


#' Transform names according to the snake case style
#'
#' @param table A `dimension_table` object.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
snake_case_table.dimension_table <- function(table) {
  table$name <- snakecase::to_snake_case(table$name)
  names(table$table) <- snakecase::to_snake_case(names(table$table))
  table
}


#' Conform dimensions
#'
#' Generate a dimension from a list of dimensions with the same schema.
#'
#' @param to_conform A `dimension_table` object list.
#'
#' @return A `dimension_table` object.
#'
#' @keywords internal
conform_dimensions <- function(to_conform) {
  dim <- to_conform[[1]]
  # to check if dimensions have the same schema
  dim_attr <- names(dim$table)
  dim_attr_length <- length(dim_attr)

  dim$table <- dplyr::select(dim$table,-tidyselect::all_of(dim$surrogate_key))
  attributes <- names(dim$table)
  if (length(to_conform) > 1) {
    for (d in 2:length(to_conform)) {
      # check if dimensions have the same schema
      dim_attr <- unique(c(dim_attr, names(to_conform[[d]]$table)))
      stopifnot("Dimensions to conform do not have the same attributes." = dim_attr_length == length(dim_attr))

      dim$table <-
        dplyr::bind_rows(dim$table,
                         dplyr::select(to_conform[[d]]$table, tidyselect::all_of(attributes)))
    }
  }
  dim$table <- dplyr::arrange_all(unique(dim$table))
  dim$table <-
    tibble::add_column(dim$table,
                       !!dim$surrogate_key := 1:nrow(dim$table),
                       .before = 1)
  dim
}


## ----------------------------------------------------------------------------------------------------------


#' `fact_table` S3 class
#'
#' A `fact_table` object is created, we have to get its
#' surrogate keys.
#'
#' @param name A string, fact name.
#' @param surrogate_keys A vector of strings, surrogate key names.
#' @param agg A vector of strings, aggregation functions.
#' @param dim_int_names A vector of strings, internal names of dimensions.
#' @param instances A flat table with the fact instances.
#'
#' @return A `fact_table` object.
#' @keywords internal
fact_table <- function(name = NULL, surrogate_keys = NULL, agg = NULL, dim_int_names = NULL, instances = NULL) {
  # Check the type of the base object
  stopifnot("A tibble with the instances was expected." = tibble::is_tibble(instances))
  stopifnot("Missing table name." = !is.null(name))
  stopifnot("Missing surrogate keys." = !is.null(surrogate_keys))
  stopifnot("Missing aggregation functions." = !is.null(agg))
  stopifnot("Missing dimension names." = !is.null(dim_int_names))

  structure(
    list(
      name = name,
      surrogate_keys = surrogate_keys,
      agg = agg,
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
  names(table$agg) <- snakecase::to_snake_case(names(table$agg))
  table
}

