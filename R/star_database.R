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
#' dput(colnames(ft_num)) # ft_num is defined in the package.
#' # Measures must be numerical
#'
#' s <- star_schema() |>
#'   define_facts(fact_schema(
#'     name = "mrs_cause",
#'     measures = c(
#'       "Pneumonia and Influenza Deaths",
#'       "All Deaths"
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
#' db <- star_database(s, ft_num)
#'
#' @export
star_database <- function(schema, instances) {
  stopifnot(tibble::is_tibble(instances))
  instance_attributes <- names(instances)
  attributes <- get_attribute_names(schema)
  for (attribute in attributes) {
    stopifnot(attribute %in% instance_attributes)
  }
  measures <- get_measure_names(schema)
  for (measure in measures) {
    stopifnot(measure %in% instance_attributes)
  }
  measure_types <- dplyr::summarise_all(instances[, measures], class)
  for (measure_type in seq_along(measure_types)) {
    measure_type <- measure_types[[measure_type]][1]
    stopifnot(measure_type %in% c("integer", "double", "integer64", "numeric"))
  }

  db <-
    list(facts = vector("list", length = 1),
         dimensions =  vector("list", length = length(schema$dimensions)))
  names(db$facts) <- sd$fact$name
  names(db$dimensions) <- names(sd$dimension)


  structure(list(schema = schema, facts = NULL, dimensions = NULL), class = "star_database")
}

