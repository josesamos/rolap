#' `star_schema` S3 class
#'
#' An empty `star_schema` object is created in which definition of facts
#' and dimensions can be added.
#'
#' To get a star database (a `star_database` object) we need a flat table
#' (implemented through a `tibble`) and a `star_schema` object. The
#' definition of facts and dimensions in the `star_schema` object is made
#' from the flat table columns. Each attribute can only appear once in the
#' definition.
#'
#' @return A `star_schema` object.
#'
#' @family star definition functions
#' @seealso \code{\link{star_schema}}
#'
#' @examples
#'
#' sc <- star_schema()
#'
#' @export
star_schema <- function() {
  structure(list(fact = NULL, dimensions = NULL), class = "star_schema")
}
