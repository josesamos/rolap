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
  stopifnot(("fact_schema" %in% class(facts)))
  structure(list(facts = facts, dimensions = star$dimensions), class = "star_schema")
}


#' Define dimension in a `star_schema` object.
#'
#' @param star A `star_schema` object.
#' @param dimension A `dimension_schema` object.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_schema}}
#'
#' @examples
#'
#' s <- star_schema()
#' d <- dimension_schema(
#'   name = "when",
#'   attributes = c(
#'     "Week Ending Date",
#'     "WEEK",
#'     "Year"
#'   )
#' )
#' s <- s |>
#'   define_dimension(d)
#' @export
define_dimension <- function(star, dimension) {
  stopifnot(("dimension_schema" %in% class(dimension)))
  if (is.null(star$dimensions)) {
    d <- list(dimension)
    names(d) <- snakecase::to_snake_case(trimws(dimension$name))
  } else {
    stopifnot(!(snakecase::to_snake_case(trimws(dimension$name)) %in% names(star$dimensions)))
    d <- star$dimensions
    n <- names(d)
    d[[length(d) + 1]] <- dimension
    names(d) <- c(n, snakecase::to_snake_case(trimws(dimension$name)))
  }
  structure(list(facts = star$facts, dimensions = d), class = "star_schema")
}
