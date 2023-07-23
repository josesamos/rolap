#' `star_database` S3 class
#'
#' An `star_database` object is created from a `star_schema` object and a flat
#' table that contains the data from which database instances are derived.
#'
#' Measures and attributes of the `star_schema` must correspond to the names of
#' the columns of the flat table.
#'
#' @param schema A `star_schema` object.
#' @param instances A flat table to define the database instances according to the schema.
#'
#' @return A `star_database` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{fact_schema}}
#'
#' @examples
#'
#' dput(colnames(ft)) # ft is defined in the package.
#'
#' s <- star_schema() |>
#'   define_facts(fact_schema(
#'     name = "mrs_cause",
#'     measures = c(
#'       "Pneumonia and Influenza Deaths",
#'       "Other Deaths"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "when",
#'     attributes = c(
#'       "Week Ending Date",
#'       "WEEK",
#'       "Year"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "where",
#'     attributes = c(
#'       "REGION",
#'       "State",
#'       "City"
#'     )
#'   ))
#'
#' db <- star_database(s, ft)
#'
#' @export
star_database <- function(schema, instances) {
  measures <- get_measure_names(schema)
  print(measures)
  attributes <- get_attribute_names(schema)
  print(attributes)
  structure(list(schema = schema, facts = NULL, dimensions = NULL), class = "star_database")
}

