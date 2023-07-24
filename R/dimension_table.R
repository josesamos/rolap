#' `dimension_table` S3 class
#'
#' A `dimension_table` object is created, we have to define its
#' surrogate key.
#'
#' @param dimension_schema A `dimension_schema` object.
#' @param flat_table A flat table with the dimension instances.
#'
#' @return A `dimension_table` object.
#' @keywords internal
dimension_table <- function(dimension_schema = NULL, flat_table = NULL) {
  # Check the type of the base object
  stopifnot(tibble::is_tibble(flat_table))
  stopifnot(!is.null(dimension_schema$name))

  ft <- flat_table[, dimension_schema$attributes]
  # remove duplicates and sort
  ft <- dplyr::arrange_all(unique(ft))
  # add surrogate primary key
  # := variables for parameter names
  # !! expands the expression into a string
  name <- dimension_schema$name
  surrogate_key = sprintf("%s_key", name)
  ft <- tibble::add_column(ft,!!surrogate_key := 1:nrow(ft), .before = 1)

  structure(list(name = name, surrogate_key = surrogate_key, dimension = ft), class = "dimension_table")
}
