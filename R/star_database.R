#' `star_database` S3 class
#'
#' A `star_database` object is created from a `star_schema` object and a flat
#' table that contains the data from which database instances are derived.
#'
#' Measures and measures of the `star_schema` must correspond to the names of
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
  stopifnot("Schema does not include fact_schema object." = methods::is(schema$facts[[1]], "fact_schema"))
  for (d in seq_along(schema$dimensions)) {
    stopifnot("Schema does not include dimension_schema object." = methods::is(schema$dimensions[[d]], "dimension_schema"))
  }
  stopifnot("A tibble with the instances was expected." = tibble::is_tibble(instances))
  instance_attributes <- names(instances)
  attributes <- get_attribute_names(schema)
  for (attribute in attributes) {
    if (!(attribute %in% instance_attributes)) {
      stop(sprintf("The schema attribute '%s' is not defined on instances.", attribute))
    }
  }
  measures <- get_measure_names(schema)
  for (measure in measures) {
    if (!(measure %in% instance_attributes)) {
      stop(sprintf("The fact measure '%s' is not defined on instances.", measure))
    }
  }
  stopifnot("The intersection between measures and attributes is not empty." = length(intersect(attributes, measures)) == 0)
  measure_types <- dplyr::summarise_all(instances[, measures], class)
  for (measure_type in seq_along(measure_types)) {
    measure_type <- measure_types[[measure_type]][1]
    if (!(measure_type %in% c("integer", "double", "integer64", "numeric"))) {
      stop(sprintf("'%s' is not one of the numeric types that measures can have.", measure_type))
    }
  }

  # default agg function
  agg_functions <- get_agg_functions(schema$facts[[1]])
  if (is.null(agg_functions)) {
    agg_functions <-  rep("SUM", length(measures))
  }
  # add the new measure to count the number of rows aggregated
  nrow_agg <- get_nrow_agg(schema$facts[[1]])
  if (is.null(nrow_agg)) {
    nrow_agg <- 'nrow_agg'
  }

  # create the structure for instances
  db <-
    structure(list(
      name = names(schema$facts)[1],
      operations = vector("list", length = length(schema$facts)),
      facts = vector("list", length = length(schema$facts)),
      dimensions =  vector("list", length = length(schema$dimensions)),
      rpd = list()
    ),
    class = "star_database")

  names(db$operations) <- names(schema$facts)
  names(db$facts) <- names(schema$facts)
  names(db$dimensions) <- names(schema$dimensions)

  # get a flat table ready to generate facts and dimensions
  # (NA values are replaced by unknown_value)
  instances[, attributes] <- prepare_to_join(instances[, attributes], unknown_value)

  # generate dimension tables
  keys <- c()
  op <- NULL
  for (d in names(schema$dimensions)) {
    # generate dimension table
    dim_name <- get_dimension_name(schema$dimensions[[d]])
    dim_attributes <- get_attribute_names(schema$dimensions[[d]])
    db$dimensions[[d]] <- dimension_table(dim_name, dim_attributes, instances)
    # include surrogate key in instances
    instances <- add_surrogate_key(db$dimensions[[d]], instances)
    keys <- c(keys, get_surrogate_key(db$dimensions[[d]]))
    op <- add_operation(op, "define_dimension", dim_name, dim_attributes)
  }

  # select only keys and measures in instances
  instances <- instances[, c(keys, measures)]

  # group instances in facts
  instances <- group_by_keys(
    instances,
    keys,
    measures,
    agg_functions,
    nrow_agg
  )

  agg <- c(agg_functions, "SUM")
  names(agg) <- c(measures, nrow_agg)

  fact_name <- get_fact_name(schema$fact[[1]])
  db$facts[[1]] <- fact_table(fact_name, keys, agg, names(schema$dimensions), instances)
  db$operations[[1]] <- add_operation(op, "define_facts", fact_name, names(agg), agg)

  db
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
  for (f in names(db$facts)) {
    db$facts[[f]] <- snake_case_table(db$facts[[f]])
  }
  for (d in names(db$dimensions)) {
    db$dimensions[[d]] <- snake_case_table(db$dimensions[[d]])
  }
  db$operations[[1]] <- add_operation(db$operations[[1]], "snake_case")

  db
}

#' @rdname as_tibble_list
#'
#' @export
as_tibble_list.star_database <- function(db) {
  as_tibble_list_common(db$dimensions, db$facts)
}

#' @rdname as_dm_class
#'
#' @export
as_dm_class.star_database <- function(db, pk_facts = TRUE) {
  as_dm_class_common(db$dimensions, db$facts, pk_facts)
}


#' Define a role playing dimension and its associated dimensions
#'
#' The same dimension can play several roles in relation to the facts. We can
#' define the main dimension and the dimensions that play different roles.
#'
#' As a result, all the dimensions will have the same instances and, if we deem
#' it necessary, also the same name of their attributes (except the surrogate key).
#'
#' @param db A `star_database` object.
#' @param rpd A string, dimension name (role playing dimension).
#' @param roles A vector of strings, dimension names (dimension roles).
#' @param rpd_att_names A boolean, common attribute names taken from rpd dimension.
#' @param att_names A vector of strings, common attribute names.
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
#'     name = "When",
#'     attributes = c(
#'       "Year",
#'       "WEEK",
#'       "Week Ending Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Available",
#'     attributes = c(
#'       "Data Availability Year",
#'       "Data Availability Week",
#'       "Data Availability Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Received",
#'     attributes = c(
#'       "Reception Year",
#'       "Reception Week",
#'       "Reception Date"
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
#' db <- star_database(s, ft_cause_rpd) |>
#'   role_playing_dimension(
#'     rpd = "When",
#'     roles = c("When Available", "When Received"),
#'     rpd_att_names = TRUE
#'   )
#'
#' db <- star_database(s, ft_cause_rpd) |>
#'   role_playing_dimension("When",
#'                          c("When Available", "When Received"),
#'                          att_names = c("Year", "Week", "Date"))
#'
#' @export
role_playing_dimension <- function(db, rpd, roles, rpd_att_names, att_names) UseMethod("role_playing_dimension")

#' @rdname role_playing_dimension
#'
#' @export
role_playing_dimension.star_database <- function(db, rpd, roles, rpd_att_names = FALSE, att_names = NULL) {
  rpd <- unique(snakecase::to_snake_case(rpd))
  stopifnot("Only one value can be indicated in rpd." = length(rpd) == 1)
  roles <- unique(snakecase::to_snake_case(roles))
  stopifnot("At least one role must be indicated." = length(roles) >= 1)
  stopifnot("rpd should not be included in roles." = !(rpd %in% roles))
  att_names <- unique(att_names)
  dims <- c(rpd, roles)
  # have to be dimensions
  dim_names <- names(db$dimensions)
  # they should not be previously defined rpd
  prev_rpd <- NULL
  for (n in names(db$rpd)) {
    prev_rpd <-c(prev_rpd, db$rpd[[n]])
  }
  # they must have the same structure (number of attributes)
  n_att <- 0
  for (d in dims) {
    if (!(d %in% dim_names)) {
      stop(sprintf("'%s' is not a dimension name.", d))
    }
    if (d %in% prev_rpd) {
      stop(sprintf("'%s' is included in a previous rpd definition.", d))
    }
    if (n_att == 0) {
      n_att <- ncol(db$dimensions[[d]]$table)
    } else {
      stopifnot("rpd and roles must have the same number of attributes." = n_att == ncol(db$dimensions[[d]]$table))
    }
  }
  if (!is.null(att_names)) {
    stopifnot("The number of attributes does not match those of att_names." = n_att == length(att_names) + 1)
  }
  # they meet all the requirements

  # annotate rpd
  db$rpd[[rpd]] <- dims

  # rename attributes
  if (rpd_att_names == TRUE) {
    att_names <- names(db$dimensions[[rpd]]$table)[-1]
  }
  if (!is.null(att_names)) {
    for (d in dims) {
      names(db$dimensions[[d]]$table) <-
        c(names(db$dimensions[[d]]$table)[1], att_names)
    }
  } else {
    att_names <- ""
  }

  db <- share_dimensions(db, dims)
  db$operations[[1]] <- add_operation(db$operations[[1]], "role_playing_dimension", rpd, roles, att_names)
  db
}


#' Share the given dimensions in the database
#'
#' @param db `star_database` or `constellation` object.
#' @param dims Vector of dimension names.
#'
#' @return A `star_database` or `constellation` object.
#' @keywords internal
share_dimensions <- function(db, dims) {
  # merge dimensions
  to_conform <- vector("list", length = length(dims))
  for (i in seq_along(dims)) {
    to_conform[i] <- db$dimensions[dims[i]]
    if (i > 1) {
      # to be able to conform they must have the same columns.
      names(to_conform[[i]]$table) <- names(to_conform[[1]]$table)
    }
  }
  cd <- conform_dimensions(to_conform)

  for (i in seq_along(dims)) {
    surrogate_key <- db$dimensions[[dims[i]]]$surrogate_key
    all_att <- names(db$dimensions[[dims[i]]]$table)
    attributes <- all_att[all_att != surrogate_key]

    # join facts to original dimension
    for (f in seq_along(db$facts)) {
      if (dims[i] %in% db$facts[[f]]$dim_int_names) {
        db$facts[[f]]$table <-
          dplyr::select(
            dplyr::inner_join(db$facts[[f]]$table,
                              db$dimensions[[dims[i]]]$table,
                              by = surrogate_key),-tidyselect::all_of(surrogate_key)
          )
      }
    }

    # change dimension table keeping attribute names
    db$dimensions[[dims[i]]]$table <- cd$table
    names(db$dimensions[[dims[i]]]$table) <- all_att

    # join new dimension to facts
    for (f in seq_along(db$facts)) {
      if (dims[i] %in% db$facts[[f]]$dim_int_names) {
        db$facts[[f]]$table <-
          dplyr::select(
            dplyr::inner_join(db$facts[[f]]$table,
                              db$dimensions[[dims[i]]]$table,
                              by = attributes),-tidyselect::all_of(attributes)
          )
      }
    }
  }
  # reorder attributes in facts
  for (f in seq_along(db$facts)) {
    measures <-
      setdiff(names(db$facts[[f]]$table),
              db$facts[[f]]$surrogate_keys)
    db$facts[[f]]$table <-
      dplyr::select(db$facts[[f]]$table, tidyselect::all_of(c(db$facts[[f]]$surrogate_keys, measures)))
  }
  db
}


#' Rename the attributes of a dimension
#'
#' The dimension attribute names match those of the flat table from which they
#' are defined. This function allows you to change their names.
#'
#' @param db A `star_database` object.
#' @param name A string, dimension name.
#' @param attributes A vector of strings, attribute names.
#'
#' @return A `star_database` object.
#'
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_dimension_attribute_names(
#'     name = "where",
#'     attributes = c(
#'       "Region",
#'       "State",
#'       "City"
#'     )
#'   )
#'
#' @export
set_dimension_attribute_names <- function(db, name, attributes) UseMethod("set_dimension_attribute_names")

#' @rdname set_dimension_attribute_names
#'
#' @export
set_dimension_attribute_names.star_database <- function(db, name, attributes) {
  attributes <- unique(attributes)
  stopifnot("Missing dimension name." = !is.null(name))
  stopifnot("It is not a dimension name." = name %in% names(db$dimensions))
  att_names <- names(db$dimensions[[name]]$table)
  stopifnot("The dimension has a different number of attributes." = length(attributes) == length(att_names) - 1)
  names(db$dimensions[[name]]$table) <- c(att_names[1], attributes)
  db$operations[[1]] <- add_operation(db$operations[[1]], "set_dimension_attribute_names", name, attributes)
  db
}

#' Get the names of the attributes of a dimension
#'
#' Obtain the names of the attributes of a dimension.
#'
#' @param db A `star_database` object.
#' @param name A string, dimension name.
#'
#' @return A vector of strings, attribute names.
#'
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_dimension_attribute_names(name = "where")
#'
#' @export
get_dimension_attribute_names <- function(db, name) UseMethod("get_dimension_attribute_names")

#' @rdname get_dimension_attribute_names
#'
#' @export
get_dimension_attribute_names.star_database <- function(db, name) {
  stopifnot("Missing dimension name." = !is.null(name))
  stopifnot("It is not a dimension name." = name %in% names(db$dimensions))
  att_names <- names(db$dimensions[[name]]$table)
  att_names[-1]
}


#' Rename the measures of a star database
#'
#' The measure names match those of the flat table from which they
#' are defined. This function allows you to change their names.
#'
#' @param db A `star_database` object.
#' @param measures A vector of strings, measure names.
#'
#' @return A `star_database` object.
#'
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   set_fact_measure_names(
#'     measures = c(
#'       "Pneumonia and Influenza",
#'       "All",
#'       "Rows Aggregated"
#'     )
#'   )
#'
#' @export
set_fact_measure_names <- function(db, measures) UseMethod("set_fact_measure_names")

#' @rdname set_fact_measure_names
#'
#' @export
set_fact_measure_names.star_database <- function(db, measures) {
  measures <- unique(measures)
  measure_names <- setdiff(names(db$facts[[1]]$table), db$facts[[1]]$surrogate_keys)
  stopifnot("Facts have a different number of measures." = length(measures) == length(measure_names))
  names(db$facts[[1]]$table) <- c(db$facts[[1]]$surrogate_keys, measures)
  db$operations[[1]] <- add_operation(db$operations[[1]], "set_fact_measure_names", names(db$facts), measures)
  db
}

#' Get the names of the measures of a star database
#'
#' Obtain the names of the measures of a star database.
#'
#' @param db A `star_database` object.
#'
#' @return A vector of strings, measure names.
#'
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_fact_measure_names()
#'
#' @export
get_fact_measure_names <- function(db) UseMethod("get_fact_measure_names")

#' @rdname get_fact_measure_names
#'
#' @export
get_fact_measure_names.star_database <- function(db) {
  setdiff(names(db$facts[[1]]$table), db$facts[[1]]$surrogate_keys)
}

#' Get similar instances of a dimension
#'
#' Obtain the names of the attributes of a dimension.
#'
#' @param db A `star_database` object.
#' @param name A string, dimension name.
#'
#' @return A vector of 'tibble' objects with similar instances.
#'
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' instances <- star_database(mrs_cause_schema, ft_num) |>
#'   get_similar_instances(name = "where")
#'
#' @export
get_similar_instances <- function(db, name) UseMethod("get_similar_instances")

#' @rdname get_similar_instances
#'
#' @export
get_similar_instances.star_database <- function(db, name) {
  stopifnot("Missing dimension name." = !is.null(name))
  stopifnot("It is not a dimension name." = name %in% names(db$dimensions))
  table <- db$dimensions[[name]]$table
  table <- data.frame(table[, colnames(table)[-1]], stringsAsFactors = FALSE)
  # in one column
  df_args <- c(table, sep="")
  table <- do.call(paste, df_args)
  # clean values
  table <- iconv(table, to="ASCII//TRANSLIT")
  table <- tolower(table)
  table <- snakecase::to_snake_case(table)
  table <- gsub("_", "", table)
  # id and value
  t_id <- data.frame(id = as.vector(db$dimensions[[name]]$table[1]), value = table)
  names(t_id) <- c("id", "value")
  # value frequency
  t_freq <- table(table)
  t_freq <- t_freq[t_freq > 1]
  # repeated values
  n_freq <- names(t_freq)
  res <- list()
  for (i in seq_along(n_freq)) {
    id <- t_id$id[t_id$value == n_freq[i]]
    v <- db$dimensions[[name]]$table[db$dimensions[[name]]$table[, 1] == id, ]
    res <- c(res, list(v))
  }
  res
}
