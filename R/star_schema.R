#' `star_schema` S3 class
#'
#' An empty `star_schema` object is created in which definition of facts
#' and dimensions can be added.
#'
#' To get a star database (a `star_database` object) we need a flat table
#' and a `star_schema` object. The definition of facts and dimensions in
#' the `star_schema` object is made from the flat table columns.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{fact_schema}}
#'
#' @examples
#'
#' s <- star_schema()
#'
#' @export
star_schema <- function() {
  structure(list(facts = NULL, dimensions = NULL), class = "star_schema")
}



#' Define facts in a `star_schema` object.
#'
#' @param star A `star_schema` object.
#' @param facts A `fact_schema` object.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_schema}}
#'
#' @examples
#'
#' s <- star_schema()
#' f <- fact_schema(
#'   name = "mrs_cause",
#'   measures = c(
#'     "Pneumonia and Influenza Deaths",
#'     "Other Deaths"
#'   )
#' )
#' s <- s |>
#'   define_facts(f)
#' @export
define_facts <- function(star, facts) {
  structure(list(facts = facts, dimensions = star$dimensions), class = "star_schema")
}
