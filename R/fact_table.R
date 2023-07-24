#' `fact_table` S3 class
#'
#' A `fact_table` object is created, we have to get its
#' surrogate keys.
#'
#' @param fact_schema A `dimension_schema` object.
#' @param instances A flat table with the fact instances.
#' @param surrogate_keys A vector of strings, surrogate key names.
#'
#' @return A `fact_table` object.
#' @keywords internal
fact_table <- function(fact_schema = NULL, instances = NULL, surrogate_keys) {
  # Check the type of the base object
  stopifnot(tibble::is_tibble(instances))
  stopifnot(!is.null(fact_schema$name))

  structure(
    list(
      name = fact_schema$name,
      surrogate_keys = surrogate_keys,
      facts = instances
    ),
    class = "fact_table"
  )
}
