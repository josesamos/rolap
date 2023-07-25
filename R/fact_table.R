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
      facts = instances
    ),
    class = "fact_table"
  )
}
