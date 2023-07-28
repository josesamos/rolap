#' `star_database` S3 class
#'
#' A `star_database` object is created from a `star_schema` object and a flat
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
#' @family star database definition functions
#' @seealso \code{\link{star_schema}}, \code{\link{mrs_cause_schema}}, \code{\link{ft_num}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num)
#'
#' @export
star_database <- function(schema, instances) {
  stopifnot(check_schema_validity(schema) == TRUE)
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
  # (NA values are replaced by UNKNOWN)
  instances[, attributes] <- prepare_to_join(instances[, attributes])

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
#' @family star database definition functions
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
snake_case <- function(db) {
  for (f in names(db$instance$facts)) {
    db$instance$facts[[f]] <- snake_case_table(db$instance$facts[[f]])
  }
  for (d in names(db$instance$dimensions)) {
    db$instance$dimensions[[d]] <- snake_case_table(db$instance$dimensions[[d]])
  }
  db
}

# as_tibble_list.star_database -------------------------------------------

#' Generate a list of tibbles with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a list of tibbles, as this function does.
#'
#' @param db A `star_database` object.
#'
#' @return A list of `tibble`
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#'
#' tl <- db |>
#'   as_tibble_list()
#'
#' @export
as_tibble_list.star_database <- function(db) {
  l <- NULL
  lnames <- NULL
  for (f in names(db$instance$facts)) {
    l <- c(l, list(db$instance$facts[[f]]$table))
    lnames <- c(lnames, f)
  }
  for (d in names(db$instance$dimensions)) {
    l <- c(l, list(db$instance$dimensions[[d]]$table))
    lnames <- c(lnames, d)
  }
  names(l) <- lnames
  l
}

