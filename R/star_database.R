#' `star_database` S3 class
#'
#' A `star_database` object is created from a `star_schema` object and a flat
#' table that contains the data from which database instances are derived.
#'
#' Measures and attributes of the `star_schema` must correspond to the names of
#' the columns of the flat table.
#'
#' Since NA values cause problems when doing Join operations between tables,
#' you can indicate the value that will be used to replace them before doing
#' these operations. If none is indicated, a default value is taken.
#'
#' @param schema A `star_schema` object.
#' @param instances A flat table to define the database instances according to the schema.
#' @param unknown_value A string, value used to replace NA values in dimensions.
#'
#' @return A `star_database` object.
#'
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}, \code{\link{star_schema}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num)
#'
#' @export
star_database <- function(schema, instances, unknown_value = NULL) {
  stopifnot("fact_schema" %in% class(schema$facts[[1]]))
  for (d in seq_along(schema$dimensions)) {
    stopifnot("dimension_schema" %in% class(schema$dimensions[[d]]))
  }
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

  # create the structure for instances
  db <-
    list(
      facts = vector("list", length = length(schema$facts)),
      dimensions =  vector("list", length = length(schema$dimensions))
    )
  names(db$facts) <- names(schema$facts)
  names(db$dimensions) <- names(schema$dimensions)

  # get a flat table ready to generate facts and dimensions
  # (NA values are replaced by unknown_value)
  instances[, attributes] <- prepare_to_join(instances[, attributes], unknown_value)

  # generate dimension tables
  keys <- c()
  for (d in names(schema$dimensions)) {
    # generate dimension table
    db$dimensions[d] <-
      list(dimension_table(
        get_dimension_name(schema$dimensions[[d]]),
        get_attribute_names(schema$dimensions[[d]]),
        instances
      ))
    # include surrogate key in instances
    instances <- add_surrogate_key(db$dimensions[[d]], instances)
    keys <- c(keys, get_surrogate_key(db$dimensions[[d]]))
  }

  # select only keys and measures in instances
  instances <- instances[, c(keys, measures)]

  # group instances in facts
  instances <- group_by_keys(
    instances,
    keys,
    measures,
    get_agg_functions(schema$facts[[1]]),
    get_nrow_agg(schema$facts[[1]])
  )
  db$facts[1] <-
    list(fact_table(
      get_fact_name(schema$fact[[1]]),
      keys,
      names(schema$dimensions),
      instances
    ))

  structure(list(schema = schema, instance = db), class = "star_database")
}


#' Transform names according to the snake case style
#'
#' Transform fact, dimension, measures, and attribute names according to the
#' snake case style.
#'
#' This style is suitable if we are going to work with databases.
#'
#' @param db A `star_database` object.
#'
#' @return A `star_database` object.
#'
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
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
#' # ft_num contains instances
#' db <- star_database(s, ft_num) |>
#'   snake_case()
#'
#' @export
snake_case <- function(db) UseMethod("snake_case")

#' @rdname snake_case
#'
#' @export
snake_case.star_database <- function(db) {
  for (f in names(db$instance$facts)) {
    db$instance$facts[[f]] <- snake_case_table(db$instance$facts[[f]])
  }
  for (d in names(db$instance$dimensions)) {
    db$instance$dimensions[[d]] <- snake_case_table(db$instance$dimensions[[d]])
  }
  db
}

#' @rdname as_tibble_list
#'
#' @export
as_tibble_list.star_database <- function(db) {
  as_tibble_list_common(db$instance$dimensions, db$instance$facts)
}

#' @rdname as_dm_class
#'
#' @export
as_dm_class.star_database <- function(db, pk_facts = TRUE) {
  as_dm_class_common(db$instance$dimensions, db$instance$facts, pk_facts)
}



