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
#' @seealso \code{\link{star_database}}
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
#' Facts are part of a `star_schema` object. They can be defined directly
#' as a `fact_schema` object or giving the name and a set of measures
#' that can be empty (does not have explicit measures).
#'
#' Associated with each measurement there is an aggregation function that can be
#' SUM, MAX or MIN. AVG is not considered among the possible aggregation
#' functions: The reason is that calculating AVG by considering subsets of
#' data does not necessarily yield the AVG of the total data.
#'
#' An additional measurement corresponding to the COUNT of aggregated rows is
#' added which, together with SUM, allows us to obtain the mean if needed.
#'
#' @param schema A `star_schema` object.
#' @param facts A `fact_schema` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names, each one for its
#'   corresponding measure. If none is indicated, the default is SUM. Additionally
#'   they can be MAX or MIN.
#' @param nrow_agg A string, name of a new measure that represents the COUNT
#'   of rows aggregated for each resulting row.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{fact_schema}}
#'
#' @examples
#'
#' s <- star_schema() |>
#'   define_facts(
#'     name = "mrs_cause",
#'     measures = c(
#'       "Pneumonia and Influenza Deaths",
#'       "Other Deaths"
#'     )
#'   )
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
define_facts <- function(schema, facts, name, measures, agg_functions, nrow_agg) UseMethod("define_facts")

#' @rdname define_facts
#'
#' @export
define_facts.star_schema <-
  function(schema,
           facts = NULL,
           name = NULL,
           measures = NULL,
           agg_functions = NULL,
           nrow_agg = NULL) {
    if (!is.null(facts)) {
      stopifnot(("fact_schema" %in% class(facts)))
      stopifnot(is.null(name) &
                  is.null(measures) &
                  is.null(agg_functions) & is.null(nrow_agg))
    } else {
      facts <- fact_schema(
        name = name,
        measures = measures,
        agg_functions = agg_functions,
        nrow_agg = nrow_agg
      )
    }
    f <- list(facts)
    names(f) <- snakecase::to_snake_case(facts$name)
    structure(list(facts = f, dimensions = schema$dimensions), class = "star_schema")
  }

#' Define dimension in a `star_schema` object.
#'
#' Dimensions are part of a `star_schema` object. They can be defined directly
#' as a `dimension_schema` object or giving the name and a set of attributes.
#'
#' @param schema A `star_schema` object.
#' @param dimension A `dimension_schema` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `star_schema` object.
#'
#' @family star schema definition functions
#' @seealso \code{\link{dimension_schema}}
#'
#' @examples
#'
#' s <- star_schema() |>
#'   define_dimension(
#'     name = "when",
#'     attributes = c(
#'       "Week Ending Date",
#'       "WEEK",
#'       "Year"
#'     )
#'   )
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
define_dimension <- function(schema, dimension, name, attributes) UseMethod("define_dimension")

#' @rdname define_dimension
#'
#' @export
define_dimension.star_schema <- function(schema, dimension = NULL, name = NULL, attributes = NULL) {
  if (!is.null(dimension)) {
    stopifnot(("dimension_schema" %in% class(dimension)))
    stopifnot(is.null(name) & is.null(attributes))
  } else {
    dimension <- dimension_schema(name = name, attributes = attributes)
  }
  if (is.null(schema$dimensions)) {
    d <- list(dimension)
    names(d) <- snakecase::to_snake_case(dimension$name)
  } else {
    stopifnot(!(snakecase::to_snake_case(dimension$name) %in% names(schema$dimensions)))
    d <- schema$dimensions
    n <- names(d)
    d[[length(d) + 1]] <- dimension
    names(d) <- c(n, snakecase::to_snake_case(dimension$name))
  }
  structure(list(facts = schema$facts, dimensions = d), class = "star_schema")
}


#' Get measure names
#'
#' Get the names of the measures defined in the fact schema.
#'
#' @param schema A `star_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_measure_names.star_schema <- function(schema) {
  names <- NULL
  for (fact in schema$facts) {
    names <- c(names, get_measure_names(fact))
  }
  unique(names)
}


#' Get attribute names
#'
#' Get the names of the attribute defined in the dimension schemas.
#'
#' @param schema A `star_schema` object.
#'
#' @return A vector of strings.
#'
#' @keywords internal
get_attribute_names.star_schema <- function(schema) {
  names <- NULL
  for (dimension in schema$dimensions) {
    names <- c(names, get_attribute_names(dimension))
  }
  unique(names)
}

