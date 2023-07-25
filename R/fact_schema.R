#' `fact_schema` S3 class
#'
#' A `fact_schema` object is created, the essential data is a name and
#' a set of measurements that can be empty (does not have explicit
#' measures). It is part of a `star_schema` object, defines the facts
#' of the star schema.
#'
#' Associated with each measure there is an aggregation function that can be
#' SUM, MAX or MIN. AVG is not considered among the possible aggregation
#' functions: The reason is that calculating AVG by considering subsets of
#' data does not necessarily yield the AVG of the total data.
#'
#' An additional measure corresponding to the COUNT of aggregated rows can
#' be added which, together with SUM, allows us to obtain the AVG if needed.
#'
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names, each one for its
#'   corresponding measure. If none is indicated, the default is SUM. Additionally
#'   they can be MAX or MIN.
#' @param nrow_agg A string, name of a new measure that represents the COUNT
#'   of rows aggregated for each resulting row.
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
fact_schema <- function(name = NULL,
                        measures = NULL,
                        agg_functions = NULL,
                        nrow_agg = NULL) {
  stopifnot(!is.null(name))
  if (!is.null(agg_functions)) {
    stopifnot(length(measures) == length(agg_functions))
    for (agg_function in agg_functions) {
      stopifnot(agg_function %in% c("SUM", "MAX", "MIN"))
    }
  }
  stopifnot(length(c(measures, nrow_agg)) == length(unique(c(measures, nrow_agg))))
  structure(
    list(
      name = name,
      measures = measures,
      agg_functions = agg_functions,
      nrow_agg = nrow_agg
    ),
    class = "fact_schema"
  )
}

#' Get fact name
#'
#' @param schema A `fact_schema` object.
#'
#' @return A string.
#'
#' @keywords internal
get_fact_name <- function(schema) {
  schema$name
}


#' Get measure names
#'
#' Get the names of the measures defined in the fact schema.
#'
#' @param schema A `fact_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_measure_names.fact_schema <- function(schema) {
  schema$measures
}

#' Get aggregate functions
#'
#' @param schema A `fact_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_agg_functions <- function(schema) {
  schema$agg_functions
}


#' Get number of rows aggregate column
#'
#' @param schema A `fact_schema` object.
#'
#' @return A string.
#'
#' @keywords internal
get_nrow_agg <- function(schema) {
  schema$nrow_agg
}
