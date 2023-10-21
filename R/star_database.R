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
#' @family star database definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}, \code{\link{star_schema}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num)
#'
#' @export
star_database <- function(schema, instances, unknown_value = NULL) {
  star_database_with_previous_operations(schema, instances, unknown_value)
}

#' Creates a `star_database` adding previous operations
#'
#' @param schema A `star_schema` object.
#' @param instances A flat table to define the database instances according to the schema.
#' @param unknown_value A string, value used to replace NA values in dimensions.
#' @param operations A list of operations.
#' @param lookup_tables A list of lookup tables.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
star_database_with_previous_operations <-
  function(schema,
           instances,
           unknown_value = NULL,
           operations = NULL,
           lookup_tables = NULL) {
    stopifnot("Schema does not include fact_schema object." = methods::is(schema$facts[[1]], "fact_schema"))
    for (d in seq_along(schema$dimensions)) {
      stopifnot(
        "Schema does not include dimension_schema object." = methods::is(schema$dimensions[[d]], "dimension_schema")
      )
    }
    stopifnot("A tibble with the instances was expected." = tibble::is_tibble(instances))
    instance_attributes <- names(instances)
    attributes <- get_attribute_names_schema(schema)
    for (attribute in attributes) {
      if (!(attribute %in% instance_attributes)) {
        stop(sprintf(
          "The schema attribute '%s' is not defined on instances.",
          attribute
        ))
      }
    }
    measures <- get_measure_names_schema(schema)
    for (measure in measures) {
      if (!(measure %in% instance_attributes)) {
        stop(sprintf("The fact measure '%s' is not defined on instances.", measure))
      }
    }
    stopifnot("The intersection between measures and attributes is not empty." = length(intersect(attributes, measures)) == 0)
    measure_types <-
      dplyr::summarise_all(instances[, measures], class)
    for (measure_type in seq_along(measure_types)) {
      measure_type <- measure_types[[measure_type]][1]
      if (!(measure_type %in% c("integer", "double", "integer64", "numeric"))) {
        stop(
          sprintf(
            "'%s' is not one of the numeric types that measures can have.",
            measure_type
          )
        )
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
      structure(
        list(
          name = names(schema$facts)[1],
          operations = vector("list", length = length(schema$facts)),
          lookup_tables = vector("list", length = length(schema$facts)),
          schemas = vector("list", length = length(schema$facts)),
          refresh = list(),
          rdb_con = list(),
          facts = vector("list", length = length(schema$facts)),
          dimensions =  vector("list", length = length(schema$dimensions)),
          rpd = list()
        ),
        class = "star_database"
      )

    names(db$operations) <- names(schema$facts)
    names(db$lookup_tables) <- names(schema$facts)
    names(db$schemas) <- names(schema$facts)
    names(db$facts) <- names(schema$facts)
    names(db$dimensions) <- names(schema$dimensions)

    # get a flat table ready to generate facts and dimensions
    # (NA values are replaced by unknown_value)
    if (is.null(unknown_value)) {
      unknown_value <- get_default_unknown_value()
    }
    instances[, attributes] <-
      prepare_to_join(instances[, attributes], unknown_value)

    if (is.null(operations)) {
      op <- star_operation()
    } else {
      op <- operations
    }
    # generate dimension tables
    keys <- c()
    for (d in names(schema$dimensions)) {
      # generate dimension table
      dim_name <- get_dimension_name(schema$dimensions[[d]])
      dim_attributes <-
        get_attribute_names_schema(schema$dimensions[[d]])
      db$dimensions[[d]] <-
        dimension_table(dim_name, dim_attributes, instances)
      # include surrogate key in instances
      instances <- add_surrogate_key(db$dimensions[[d]], instances)
      keys <- c(keys, get_surrogate_key(db$dimensions[[d]]))
      # op <-
      #   add_operation(op, "define_dimension", dim_name, dim_attributes)
    }

    # select only keys and measures in instances
    instances <- instances[, c(keys, measures)]

    # group instances in facts
    instances <- group_by_keys(instances,
                               keys,
                               measures,
                               agg_functions,
                               nrow_agg)

    agg <- c(agg_functions, "SUM")
    names(agg) <- c(measures, nrow_agg)

    fact_name <- get_fact_name(schema$fact[[1]])
    db$facts[[1]] <-
      fact_table(fact_name, keys, agg, names(schema$dimensions), instances)
    # db$operations[[1]] <-
    #   add_operation(op, "define_facts", fact_name, names(agg), agg)
    if (!is.null(lookup_tables)) {
      db$lookup_tables[[1]] <- lookup_tables
    }
    db$schemas[[1]] <- schema
    db$operations[[1]] <-
      add_operation(op, "star_database", names(db$schemas), unknown_value)

    db
  }


#' @rdname get_star_database
#'
#' @export
get_star_database.star_database <- function(db, name) {
  if (!is.null(name)) {
    star <- validate_facts(names(db$facts), name)
    if (length(star) == 1) {
      db$name <- star
    }
    db$operations <- db$operations[star]
    db$lookup_tables <- db$lookup_tables[star]
    db$schemas <- db$schemas[star]
    db$refresh <- list()
    db$rdb_con <- list()
    db$facts <- db$facts[star]
    dim <- NULL
    for (f in names(db$facts)) {
      dim <- c(dim, db$facts[[f]]$dim_int_names)
    }
    dim <- unique(dim)
    db$dimensions <- db$dimensions[dim]
    db$rpd <- filter_rpd_dimensions(db, dim)
    db <- purge_dimension_instances_star_database(db)
  }
  db
}



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
  for (f in names(db$facts)) {
    db$operations[[f]] <-
      add_operation(db$operations[[f]], "snake_case")
  }
  db
}


#' @rdname get_attribute_names
#'
#' @export
get_attribute_names.star_database <-
  function(db,
           name,
           ordered = FALSE,
           as_definition = FALSE) {
    stopifnot("Missing dimension name." = !is.null(name))
    name <- snakecase::to_snake_case(name)
    stopifnot("It is not a dimension name." = name %in% names(db$dimensions))
    att_names <- names(db$dimensions[[name]]$table)
    transform_names(names = att_names[-1], ordered, as_definition)
  }


#' @rdname get_measure_names
#'
#' @export
get_measure_names.star_database <-
  function(db,
           name = NULL,
           ordered = FALSE,
           as_definition = FALSE) {
    if (is.null(name)) {
      name <- names(db$facts[1])
    }
    name <- snakecase::to_snake_case(name)
    stopifnot("It is not a fact name." = name %in% names(db$facts))
    names <-
      setdiff(names(db$facts[[name]]$table), db$facts[[name]]$surrogate_keys)
    transform_names(names, ordered, as_definition)
  }


#' @rdname set_attribute_names
#'
#' @export
set_attribute_names.star_database <-
  function(db, name, old = NULL, new) {
    stopifnot("Missing dimension name." = !is.null(name))
    name <- snakecase::to_snake_case(name)
    stopifnot("It is not a dimension name." = name %in% names(db$dimensions))
    att_names <- names(db$dimensions[[name]]$table)
    old <- validate_attributes(att_names[-1], old)
    stopifnot("There are repeated attributes." = length(new) == length(unique(new)))
    stopifnot(
      "The number of new names must be equal to the number of names to replace." = length(old) == length(new)
    )
    names <- replace_names(att_names, old, new)
    if (length(names) != length(unique(snakecase::to_snake_case(names)))) {
      stop("There are repeated attributes.")
    }
    names(db$dimensions[[name]]$table) <- names
    db$operations[[1]] <-
      add_operation(db$operations[[1]], "set_attribute_names", name, old, new)
    db
  }


#' @rdname set_measure_names
#'
#' @export
set_measure_names.star_database <-
  function(db, name = NULL, old = NULL, new) {
    if (is.null(name)) {
      name <- names(db$facts[1])
    }
    name <- snakecase::to_snake_case(name)
    stopifnot("It is not a fact name." = name %in% names(db$facts))
    measure_names <-
      setdiff(names(db$facts[[name]]$table), db$facts[[name]]$surrogate_keys)
    old <- validate_measures(measure_names, old)
    stopifnot("There are repeated measures." = length(new) == length(unique(new)))
    stopifnot(
      "The number of new names must be equal to the number of names to replace." = length(old) == length(new)
    )
    names <- replace_names(measure_names, old, new)
    names <- c(db$facts[[name]]$surrogate_keys, names)
    if (length(names) != length(unique(snakecase::to_snake_case(names)))) {
      stop("There are repeated measures.")
    }
    names(db$facts[[name]]$table) <- names
    db$operations[[name]] <-
      add_operation(db$operations[[name]],
                    "set_measure_names",
                    names(db$facts),
                    old,
                    new)
    db
  }


#' @rdname get_similar_attribute_values
#'
#' @export
get_similar_attribute_values.star_database <-
  function(db,
           name = NULL,
           attributes = NULL,
           exclude_numbers = FALSE,
           col_as_vector = NULL) {
    name <- validate_dimension_names(db, name)
    rv =  vector("list", length = length(name))
    names(rv) <- name
    original_att <- attributes
    for (dn in name) {
      dt <- db$dimensions[[dn]]$table
      attributes <-
        validate_attributes(colnames(dt)[-1], original_att)
      rv[[dn]] <-
        get_similar_values_table(dt[, attributes], attributes, exclude_numbers, col_as_vector)
    }
    if (length(rv) == 1) {
      rv[[1]]
    } else {
      rv
    }
  }


#' @rdname get_similar_attribute_values_individually
#'
#' @export
get_similar_attribute_values_individually.star_database <-
  function(db,
           name = NULL,
           attributes = NULL,
           exclude_numbers = FALSE,
           col_as_vector = NULL) {
    name <- validate_dimension_names(db, name)
    rv =  vector("list", length = length(name))
    names(rv) <- name
    original_att <- attributes
    for (dn in name) {
      attributes <-
        validate_attributes(colnames(db$dimensions[[dn]]$table)[-1], original_att)
      l <- list()
      for (at in attributes) {
        la <-
          get_similar_attribute_values(db, dn, at, exclude_numbers, col_as_vector)
        if (length(la) > 0) {
          l <- c(l, la)
        }
      }
      rv[[dn]] <- l
    }
    if (length(rv) == 1) {
      rv[[1]]
    } else {
      rv
    }
  }


#' @rdname get_unique_attribute_values
#'
#' @export
get_unique_attribute_values.star_database <-
  function(db,
           name = NULL,
           attributes = NULL,
           col_as_vector = NULL) {
    name <- validate_dimension_names(db, name)
    rv =  vector("list", length = length(name))
    names(rv) <- name
    original_att <- attributes
    for (dn in name) {
      dt <- db$dimensions[[dn]]$table
      attributes <-
        validate_attributes(colnames(dt)[-1], original_att)
      rv[[dn]] <-
        get_unique_values_table(dt[, attributes], col_as_vector)
    }
    if (length(rv) == 1) {
      rv[[1]]
    } else {
      rv
    }
  }


#' @rdname replace_attribute_values
#'
#' @export
replace_attribute_values.star_database <-
  function(db, name, attributes = NULL, old, new) {
    stopifnot("One dimension must be indicated (only one)." = length(name) == 1)
    name <- validate_dimension_names(db, name)
    table <- db$dimensions[[name]]$table
    att <- colnames(table)[-1]
    attributes <- validate_attributes(att, attributes)
    pos_att <- c()
    for (attribute in attributes) {
      pos <- which(att %in% attribute)
      pos_att <- c(pos_att, pos + 1)
    }
    n_att <- length(pos_att)
    stopifnot(
      "The number of new values must be equal to the number of dimension attributes." = n_att == length(new)
    )
    if (n_att > 1) {
      stopifnot("The number of old and new values must be equal." = length(old) == length(new))
    }
    # update various old values
    if (n_att == 1 & length(new) == 1 & length(old) > 1) {
      various_old <- TRUE
    } else {
      various_old <- FALSE
    }
    dims <- get_rpd_dimensions(db, name)
    for (name in dims) {
      table <- db$dimensions[[name]]$table
      if (!various_old) {
        for (j in 1:n_att) {
          table <- table[table[, pos_att[j]] == old[j], ]
        }
      } else {
        # 1 attribute and n old values
        or_res <- rep(FALSE, nrow(table))
        for (j in 1:length(old)) {
          or_res <- or_res | (table[, pos_att[1]] == old[j])
        }
        table <- table[or_res, ]
      }
      r <- as.vector(table[, 1])[[1]]
      if (length(r) > 0) {
        for (i in 1:length(r)) {
          for (j in 1:n_att) {
            db$dimensions[[name]]$table[db$dimensions[[name]]$table[, 1] == r[i], pos_att[j]] <-
              new[j]
          }
        }
      }
    }
    for (f in seq_along(db$facts)) {
      for (name in dims) {
        if (name %in% db$facts[[f]]$dim_int_names) {
          n <- names(db$facts[f])
          db$operations[[n]] <-
            add_operation(
              db$operations[[n]],
              "replace_attribute_values",
              c(name, "|", attributes),
              old,
              new
            )
          break
        }
      }
    }
    db
  }

#-------------------------------------------------------------------------------

#' Get the names of the dimensions of a star database
#'
#' Obtain the names of the dimensions of a star database.
#'
#' @param db A `star_database` object.
#' @param star A string or integer, star database name or index in constellation.
#'
#' @return A vector of strings, dimension names.
#'
#' @family star database definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_dimension_names()
#'
#' @export
get_dimension_names <- function(db, star)
  UseMethod("get_dimension_names")

#' @rdname get_dimension_names
#'
#' @export
get_dimension_names.star_database <- function(db, star = NULL) {
  sort(names(db$dimensions))
}


#' Get the names of the facts of a star database
#'
#' Obtain the names of the facts of a star database.
#'
#' @param db A `star_database` object.
#'
#' @return A vector of strings, fact names.
#'
#' @family star database definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_fact_names()
#'
#' @export
get_fact_names <- function(db)
  UseMethod("get_fact_names")

#' @rdname get_fact_names
#'
#' @export
get_fact_names.star_database <- function(db) {
  sort(names(db$facts))
}


#' Get the names of the tables of a star database
#'
#' Obtain the names of the tables of a star database.
#'
#' @param db A `star_database` object.
#'
#' @return A vector of strings, table names.
#'
#' @family star database definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' names <- star_database(mrs_cause_schema, ft_num) |>
#'   get_table_names()
#'
#' @export
get_table_names <- function(db)
  UseMethod("get_table_names")

#' @rdname get_table_names
#'
#' @export
get_table_names.star_database <- function(db) {
  sort(c(names(db$dimensions), names(db$facts)))
}


#' Group instances of a dimension
#'
#' After changes in values in the instances of a dimension, groups the instances
#' and, if necessary, also the related facts.
#'
#' @param db A `star_database` object.
#' @param name A string, dimension name.
#'
#' @return A `star_database` object.
#'
#' @family star database definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   group_dimension_instances(name = "where")
#'
#' @export
group_dimension_instances <-
  function(db, name)
    UseMethod("group_dimension_instances")

#' @rdname group_dimension_instances
#'
#' @export
group_dimension_instances.star_database <- function(db, name) {
  stopifnot("One dimension must be indicated (only one)." = length(name) == 1)
  name <- validate_dimension_names(db, name)
  dims <- get_rpd_dimensions(db, name)

  db <- share_dimensions(db, dims)

  for (f in seq_along(db$facts)) {
    for (name in dims) {
      if (name %in% db$facts[[f]]$dim_int_names) {
        # group instances in facts
        agg_functions <- db$facts[[f]]$agg
        measures <- names(agg_functions)
        len <- length(measures)
        db$facts[[f]]$table <-
          group_by_keys(
            table = db$facts[[f]]$table,
            keys = db$facts[[f]]$surrogate_keys,
            measures = names(db$facts[[f]]$agg),
            agg_functions = db$facts[[f]]$agg,
            nrow_agg = NULL
          )

        # annotate operation
        n <- names(db$facts[f])
        db$operations[[n]] <-
          add_operation(db$operations[[n]], "group_dimension_instances", name)

        # only once is necessary
        break
      }
    }
  }
  db
}


# Internal ---------------------------------------------------------------------

#' Validate dimension names
#'
#' @param db A `star_database` object.
#' @param name A vector of strings, dimension names.
#'
#' @return A vector of strings, dimension names.
#'
#' @keywords internal
validate_dimension_names <- function(db, name) {
  if (!is.null(name)) {
    name <- unique(snakecase::to_snake_case(name))
    for (dn in name) {
      if (!(dn %in% names(db$dimensions))) {
        stop(sprintf("'%s' is not a dimension name.", dn))
      }
    }
  } else {
    name <- names(db$dimensions)
  }
  # eliminate repeated rpd from name
  for (r in seq_along(db$rpd)) {
    if (length(intersect(name, db$rpd[[r]])) > 0) {
      name <- setdiff(name, db$rpd[[r]])
      name <- c(name, db$rpd[[r]][1])
    }
  }
  name
}


#' Add dimension instances
#'
#' @param db A `star_database` object.
#' @param name A string, dimension name.
#' @param table A table of new instances.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
add_dimension_instances <- function(db, name, table) {
  dim <- db$dimensions[[name]]
  last <- max(dim$table[, dim$surrogate_key])
  table <-
    tibble::add_column(table,!!dim$surrogate_key := (last + 1:nrow(table)), .before = 1)
  rpd <- get_rpd_dimensions(db, name)
  res <- list()
  names_res <- NULL
  for (d in rpd) {
    dim <- db$dimensions[[d]]
    names(table) <- names(dim$table)
    db$dimensions[[d]]$table <- rbind(dim$table, table)
    res <- c(res, list(table))
    names_res <- c(names_res, d)
  }
  names(res) <- names_res
  db$refresh[[length(db$refresh)]][['insert']] <-
    c(db$refresh[[length(db$refresh)]][['insert']], res)
  db
}

#' Purge instances of a dimension
#'
#' Delete instances of a dimension that are not referenced in the facts.
#'
#' @param db A `star_database` object.
#' @param dim A string, dimension name.
#'
#' @return A `tibble`, dimension table.
#'
#' @keywords internal
purge_dimension <- function(db, dim) {
  surrogate_key <- db$dimensions[[dim]]$surrogate_key
  rpd <- get_rpd_dimensions(db, dim)
  used <- NULL
  for (d in rpd) {
    for (f in seq_along(db$facts)) {
      if (d %in% db$facts[[f]]$dim_int_names) {
        col_f <- dplyr::select(db$facts[[f]]$table,
                               tidyselect::all_of(db$dimensions[[d]]$surrogate_key))
        names(col_f) <- surrogate_key
        used <- dplyr::bind_rows(used, col_f)
      }
    }
  }
  used <- dplyr::summarise(dplyr::group_by_at(used, surrogate_key))

  dplyr::inner_join(db$dimensions[[dim]]$table, used, by = surrogate_key)
}


#' Purge instances of dimensions
#'
#' Delete instances of dimensions that are not referenced in the facts.
#'
#' @param db A `star_database` object.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
purge_dimension_instances_star_database <- function(db) {
  for (dim in names(db$dimensions)) {
    db$dimensions[[dim]]$table <- purge_dimension(db, dim)
  }
  db
}


#' Purge instances of dimensions
#'
#' Delete instances of dimensions that are not referenced in the facts.
#'
#' @param db A `star_database` object.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
purge_dimension_instances <- function(db) {
  res <- list()
  res_names <- NULL
  for (dim in names(db$dimensions)) {
    original <- db$dimensions[[dim]]$table
    db$dimensions[[dim]]$table <- purge_dimension(db, dim)
    deleted <- dplyr::setdiff(original, db$dimensions[[dim]]$table)
    if (nrow(deleted) > 0) {
      res <- c(res, list(deleted |>
                           dplyr::select(tidyselect::all_of(db$dimensions[[dim]]$surrogate_key))))
      res_names <- c(res_names, dim)
    }
  }
  names(res) <- res_names
  db$refresh[[length(db$refresh)]][['delete']] <-
    c(db$refresh[[length(db$refresh)]][['delete']], res)
  db
}
