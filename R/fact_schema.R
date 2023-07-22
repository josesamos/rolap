#' `fact_schema` S3 class
#'
#' A `fact_schema` object is created, the essential data is a name and
#' a set of measurements that can be empty (does not have explicit
#' measurements).
#'
#'  A `fact_schema` object is part of a `star_schema` object, defines
#'  the facts of the star schema.
#'
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#'
#' @return A `fact_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{star_schema}}
#'
#' @examples
#'
#' f <- fact_schema(
#'   name = "mrs_cause",
#'   measures = c(
#'     "Pneumonia and Influenza Deaths",
#'     "Other Deaths"
#'   )
#' )
#'
#' @export
fact_schema <- function(name = NULL, measures = NULL) {
  stopifnot(!is.null(name))
  structure(list(name = name, measures = measures), class = "fact_schema")
}
