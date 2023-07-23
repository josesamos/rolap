#' `star_database` S3 class
#'
#' An `star_database` object is created from a `star_schema` object and a flat
#' table that contains the data from which database instances are derived.
#'
#' Measures and attributes of the `star_schema` must correspond to the names of
#' the columns of the flat table.
#'
#' @return A `star_database` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{fact_schema}}
#'
#' @examples
#'
#' s <- star_schema()
#'
#' @export
star_database <- function(schema, instances) {
  structure(list(schema = schema, facts = NULL, dimensions = NULL), class = "star_database")
}

