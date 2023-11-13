#' Refresh a star database in a constellation
#'
#' Incremental update of a star database from the star database generated with
#' the new data.
#'
#' There may be data in the update that already exists in the facts: it is
#' indicated what to do with it, replace it, group it, delete it or ignore it in
#' the update.
#'
#' If to obtain the update data we have had to perform new transformations (which
#' were not necessary to obtain the star database), we can indicate that these are
#' the new transformation operations for the star database. These operations are
#' not applied to the star database, they will only be applied to new periodic
#' updates.
#'
#' @param db A `star_database` object.
#' @param sdbu A `star_database_update` object.
#' @param existing_instances A string, operation to be carried out on the instances
#' of already existing facts. The possible values are: "ignore", "replace",
#' "group" and "delete".
#' @param replace_transformations A boolean, replace the `star_database`
#' transformation code with the `star_database_update` one.
#' @param ... internal test parameters.
#'
#' @return A `star_database` object.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db <-
#'   flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
#'                                       ft_cause_rpd$WEEK != '4',]) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   role_playing_dimension(rpd = "When",
#'                          roles = c("When Available", "When Received"))
#' f2 <- flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
#'                                            ft_cause_rpd$WEEK != '2',])
#' f2 <- f2 |>
#'   update_according_to(db)
#'
#' db <- db |>
#'   incremental_refresh(f2)
#'
#' @export
incremental_refresh <- function(db, sdbu, existing_instances, replace_transformations, ...)
  UseMethod("incremental_refresh")

#' @rdname incremental_refresh
#'
#' @export
incremental_refresh.star_database <-
  function(db,
           sdbu,
           existing_instances = "ignore",
           replace_transformations = FALSE,
           ...) {
    internal <- list(...)
    if (!(existing_instances %in% c("ignore", "replace", "group", "delete"))) {
      stop(
        sprintf(
          "Operation %s not available. It has to be: 'ignore', 'replace', 'group' or 'delete'.",
          existing_instances
        )
      )
    }
    star <- names(sdbu$star_database$facts)
    if (replace_transformations) {
      db$operations[[star]] <- sdbu$star_database$operations[[star]]
      db$lookup_tables[[star]] <- sdbu$star_database$lookup_tables[[star]]
      db$schemas[[star]] <- sdbu$star_database$schemas[[star]]
    }
    # in case two star databases of the constellation are being modified
    sdbu$combination <- check_refesh(db, sdbu$star_database)

    # refresh data
    refresh_name <- paste0('r', snakecase::to_snake_case(paste0(Sys.time())))
    names_refresh <- names(db$refresh)
    names_refresh <- c(names_refresh, refresh_name)
    l <- length(db$refresh) + 1
    db$refresh[[l]] <- vector("list", length = 3)
    names(db$refresh) <- names_refresh
    names(db$refresh[[l]]) <- c('insert', 'replace', 'delete')

    new_dim <- get_new_dimension_instances(sdbu)
    for (d in names(new_dim)) {
      table <- new_dim[[d]]
      db <- add_dimension_instances(db, d, table)
    }
    combination <- check_refesh(db, sdbu$star_database)
    facts <- combination[[star]]
    facts$table <- facts$table |>
      dplyr::select(-tidyselect::all_of(facts$surrogate_keys))
    # rename fact surrogate key
    sk <- paste0('original_', facts$surrogate_keys)
    names <- names(facts$table)
    i <- which(names %in% sk)
    names[i] <- facts$surrogate_keys
    names(facts$table) <- names
    rest <- setdiff(names, facts$surrogate_keys)
    facts$table <- facts$table[, c(facts$surrogate_keys, rest)]
    facts_new <- facts$table[facts$table$existing_fact == FALSE, ] |>
      dplyr::select(-tidyselect::all_of('existing_fact'))
    facts_exist <- facts$table[facts$table$existing_fact == TRUE, ] |>
      dplyr::select(-tidyselect::all_of('existing_fact'))

    # new facts
    if (nrow(facts_new) > 0) {
      db$facts[[star]]$table <- rbind(db$facts[[star]]$table, facts_new)
      facts_new <- list(facts_new)
      names(facts_new) <- star
      db$refresh[[length(db$refresh)]][['insert']] <-
        c(db$refresh[[length(db$refresh)]][['insert']], facts_new)
    }

    # existing facts: 'replace', 'group' or 'delete'
    if (nrow(facts_exist) > 0) {
      if (existing_instances == 'group') {
        db$facts[[star]]$table <- rbind(db$facts[[star]]$table, facts_exist)
        db$facts[[star]]$table <-
          group_by_keys(
            table = db$facts[[star]]$table,
            keys = db$facts[[star]]$surrogate_keys,
            measures = names(db$facts[[star]]$agg),
            agg_functions = db$facts[[star]]$agg,
            nrow_agg = NULL
          )
        facts_exist <- facts_exist |>
          dplyr::select(tidyselect::all_of(db$facts[[star]]$surrogate_keys)) |>
          dplyr::left_join(db$facts[[star]]$table, by = db$facts[[star]]$surrogate_keys)
        facts_exist <- list(db$facts[[star]]$surrogate_keys, facts_exist)
        names(facts_exist) <- c('surrogate_keys', 'table')
        facts_exist <- list(facts_exist)
        names(facts_exist) <- star
        db$refresh[[length(db$refresh)]][['replace']] <-
          c(db$refresh[[length(db$refresh)]][['replace']], facts_exist)
      } else {
        only_key <- facts_exist |>
          dplyr::select(tidyselect::all_of(db$facts[[star]]$surrogate_keys))
        only_key$existing_fact <- TRUE
        t <- db$facts[[star]]$table |>
          dplyr::left_join(only_key, by = db$facts[[star]]$surrogate_keys)
        t$existing_fact[is.na(t$existing_fact)] <-  FALSE

        if (existing_instances == 'replace') {
          db$facts[[star]]$table[t$existing_fact, ] <- facts_exist
          facts_exist <- list(db$facts[[star]]$surrogate_keys, facts_exist)
          names(facts_exist) <- c('surrogate_keys', 'table')
          facts_exist <- list(facts_exist)
          names(facts_exist) <- star
          db$refresh[[length(db$refresh)]][['replace']] <-
            c(db$refresh[[length(db$refresh)]][['replace']], facts_exist)
        } else if (existing_instances == 'delete') {
          db$facts[[star]]$table <- db$facts[[star]]$table[!(t$existing_fact), ]
          facts_exist <- list(facts_exist |>
                                dplyr::select(tidyselect::all_of(db$facts[[star]]$surrogate_keys)))
          names(facts_exist) <- star
          db$refresh[[length(db$refresh)]][['delete']] <-
            c(db$refresh[[length(db$refresh)]][['delete']], facts_exist)
          db <- purge_dimension_instances(db)
        }
      }
    }
    if (length(internal) == 0) {
      del <- FALSE
    } else {
      del <- internal[[1]]
    }
    db <- refresh_deployments(db, del)
    db
  }

#' Generate refresh sql
#'
#' Generate sql code for the first refresh operation.
#'
#' @param refresh A list of operations over tables.
#'
#' @return A vector of strings.
#'
#' @keywords internal
generate_refresh_sql <- function(refresh) {
  operations <- names(refresh)
  res <- NULL
  for (op in operations) {
    if (length(refresh[[op]]) > 0) {
      if (op == "insert") {
        for (table in names(refresh[[op]])) {
          instances <- refresh[[op]][[table]]
          if (nrow(instances) > 0) {
            sql <- generate_table_sql_insert(table, instances)
            res <- c(res, sql)
          }
        }
      } else if (op == "replace") {
        for (table in names(refresh[[op]])) {
          surrogate_keys <- refresh[[op]][[table]]$surrogate_keys
          instances <- refresh[[op]][[table]]$table
          if (nrow(instances) > 0) {
            sql <- generate_table_sql_update(table, surrogate_keys, instances)
            res <- c(res, sql)
          }
        }
      } else if (op == "delete") {
        for (table in names(refresh[[op]])) {
          instances <- refresh[[op]][[table]]
          if (nrow(instances) > 0) {
            sql <- generate_table_sql_delete(table, instances)
            res <- c(res, sql)
          }
        }
      }
    }
  }
  res
}


#' Generate table sql delete
#'
#' Generate sql code for deleting instances in a table.
#'
#' @param table A string, table name.
#' @param instances A `tibble`.
#'
#' @return A vector of strings.
#'
#' @keywords internal
generate_table_sql_delete <- function(table, instances) {
  surrogate_keys <- names(instances)
  res <- NULL
  n_ins <- nrow(instances)
  for (i in 1:n_ins) {
    sql <- paste0("DELETE FROM `", table, "` WHERE ")
    for (s in surrogate_keys) {
      if (s == surrogate_keys[1]) {
        sep = ""
      } else {
        sep = " AND "
      }
      sql <- paste(sql, sprintf("`%s` = %s", s, instances[i, s]), sep = sep)
    }
    # sql <- paste0(sql, ";")
    res <- c(res, sql)
  }
  res
}


#' Generate table sql update
#'
#' Generate sql code for updating a table.
#'
#' @param table A string, table name.
#' @param surrogate_keys A string.
#' @param instances A `tibble`.
#'
#' @return A vector of strings.
#'
#' @keywords internal
generate_table_sql_update <- function(table, surrogate_keys, instances) {
  measures <- setdiff(names(instances), surrogate_keys)
  res <- NULL
  n_ins <- nrow(instances)
  for (i in 1:n_ins) {
    sql <- paste0("UPDATE `", table, "` SET ")
    for (m in measures) {
      if (m == measures[1]) {
        sep = ""
      } else {
        sep = ", "
      }
      sql <- paste(sql, sprintf("`%s` = %s", m, instances[i, m]), sep = sep)
    }
    sql <- paste(sql, " WHERE ", sep = "")
    for (s in surrogate_keys) {
      if (s == surrogate_keys[1]) {
        sep = ""
      } else {
        sep = " AND "
      }
      sql <- paste(sql, sprintf("`%s` = %s", s, instances[i, s]), sep = sep)
    }
    # sql <- paste0(sql, ";")
    res <- c(res, sql)
  }
  res
}


#' Generate table sql insert
#'
#' Generate sql code for inserting a table.
#'
#' @param table A string, table name.
#' @param instances A `tibble`.
#'
#' @return A string.
#'
#' @keywords internal
generate_table_sql_insert <- function(table, instances) {
  res <-
    paste0("INSERT INTO `",
           table,
           "`(`",
           paste(names(instances), collapse = "`, `"),
           "`) VALUES ")
  n_att <- ncol(instances)
  n_ins <- nrow(instances)
  for (i in 1:n_ins) {
    dt <- "("
    for (j in 1:n_att) {
      if (j == 1) {
        sep = ""
      } else {
        sep = ", "
      }
      dt <- paste(dt, sprintf("'%s'", instances[i, j]), sep = sep)
    }
    dt <- paste(dt, ")", sep = "")
    if (i == n_ins) {
      # dt <- paste0(dt, ";")
    } else {
      dt <- paste0(dt, ", ")
    }
    res <- paste0(res, dt)
  }
  res
}



#' Checks the refresh of the selected star database from the given database
#'
#' Checks the refresh operation of the selected star database from the given
#' database. Once this operation is carried out, the results can be consulted on
#' the new instances in dimensions or existing instances in the facts.
#'
#' @param db A `star_database` object.
#' @param refresh_db A `star_database` object with the same structure with
#' new data.
#'
#' @return A list of facts and dimensions, first facts, then dimensions.
#'
#' @keywords internal
check_refesh <- function(db, refresh_db) {
  star <- names(refresh_db$facts)
  of <- db$facts[[star]]
  od <- db$dimensions[of$dim_int_names]
  rf <- refresh_db$facts[[star]]
  rd <- refresh_db$dimensions
  for (d in names(od)) {
    # rename dimension surrogate key
    names <- names(od[[d]]$table)
    i <- which(names == rd[[d]]$surrogate_key)
    names[i] <- paste0('original_', names[i])
    names(od[[d]]$table) <- names
    attributes <- names[-i]
    # rename fact surrogate key
    names <- names(of$table)
    i <- which(names == rd[[d]]$surrogate_key)
    names[i] <- paste0('original_', names[i])
    names(of$table) <- names

    rd[[d]]$table <- rd[[d]]$table |>
      dplyr::left_join(od[[d]]$table, by = attributes)
    rf$table <- rf$table |>
      dplyr::left_join(rd[[d]]$table,
                        by = rd[[d]]$surrogate_key) |>
      dplyr::select(-tidyselect::all_of(attributes))
  }
  measures <-
    setdiff(names(of$table), c(of$surrogate_keys, paste0('original_', rf$surrogate_keys)))
  of$table$existing_fact <- TRUE
  rf$table <- rf$table |>
    dplyr::left_join(dplyr::select(of$table, -tidyselect::all_of(measures)),
                      by = paste0('original_', rf$surrogate_keys))
  rf$table$existing_fact[is.na(rf$table$existing_fact)] <-  FALSE
  rf <- list(rf)
  names(rf) <- star
  c(rf, rd)
}



#' Get existing fact instances
#'
#' From the planned update, it obtains the instances of the update facts that
#' are already included in the star database facts to be updated.
#'
#' The most common thing is that refresh operations only include new instances
#' in fact tables, but it may be the case that repeated instances appear: They
#' may have different values in the measures, but the same values in the dimension
#' foreign keys. When the update occurs, we need to determine what happens to
#' these instances.
#'
#' @param sdbu A `star_database_update` object.
#'
#' @return A `tibble` object.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f1 <-
#'   flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
#'                                       ft_cause_rpd$WEEK != '4',]) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   role_playing_dimension(rpd = "When",
#'                          roles = c("When Available", "When Received"))
#' f2 <- flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
#'                                            ft_cause_rpd$WEEK != '2',])
#' f2 <- f2 |>
#'   update_according_to(f1)
#' fact_instances <- f2 |>
#'   get_existing_fact_instances()
#'
#' @export
get_existing_fact_instances <- function(sdbu) UseMethod("get_existing_fact_instances")

#' @rdname get_existing_fact_instances
#'
#' @export
get_existing_fact_instances.star_database_update <- function(sdbu) {
  ft <- sdbu$combination[[1]]$table[sdbu$combination[[1]]$table$existing_fact, ]
  sk <- sdbu$combination[[1]]$surrogate_keys
  measures <- setdiff(names(ft), c(sk, paste0('original_', sk), 'existing_fact'))
  attributes <- NULL
  dimensions <- sdbu$combination[-1]
  for (d in names(dimensions)) {
    dim <- dimensions[[d]]$table
    key <- dimensions[[d]]$surrogate_key
    attributes <-
      c(attributes, setdiff(names(dim), c(key, paste0('original_', key))))
    ft <- ft |>
      dplyr::inner_join(dim, by = key)
  }
  ft <- ft |>
    dplyr::select(tidyselect::all_of(c(attributes, measures)))
  ft
}


#' Get new dimension instances
#'
#' From the planned update, it obtains the instances of the update dimensions
#' that are not included in the star database dimensions to be updated.
#'
#' @param sdbu A `star_database_update` object.
#'
#' @return A list of `tibble` objects.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f1 <-
#'   flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
#'                                       ft_cause_rpd$WEEK != '4',]) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   role_playing_dimension(rpd = "When",
#'                          roles = c("When Available", "When Received"))
#' f2 <- flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
#'                                            ft_cause_rpd$WEEK != '2',])
#' f2 <- f2 |>
#'   update_according_to(f1)
#' dim_instances <- f2 |>
#'   get_new_dimension_instances()
#'
#' @export
get_new_dimension_instances <- function(sdbu) UseMethod("get_new_dimension_instances")

#' @rdname get_new_dimension_instances
#'
#' @export
get_new_dimension_instances.star_database_update <- function(sdbu) {
  dimensions <- sdbu$combination[-1]
  res <- list()
  dim_names <- names(dimensions)
  dim_names <- simplify_rpd_dimensions(sdbu$star_database, dim_names)
  for (d in dim_names) {
    dim <- dimensions[[d]]$table
    pk <- dimensions[[d]]$surrogate_key
    pko <-  paste0('original_', pk)
    attributes <- setdiff(names(dim), c(pk, pko))
    dim <- dim[is.na(dim[pko]), ]
    dim <- dim |>
      dplyr::select(tidyselect::all_of(attributes))
    if (nrow(dim) > 0) {
      dim <- list(dim)
      names(dim) <- d
      res <- c(res, dim)
    }
  }
  res
}


#' Get transformation function code
#'
#' From the planned update, it obtains the function with the source code of the
#' transformations performed on the original data in string vector format.
#'
#' @param sdbu A `star_database_update` object.
#'
#' @return A vector of strings.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f1 <- flat_table('ft_num', ft_cause_rpd) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   replace_attribute_values(
#'     name = "When Available",
#'     old = c('1962', '11', '1962-03-14'),
#'     new = c('1962', '3', '1962-01-15')
#'   ) |>
#'   group_dimension_instances(name = "When")
#' f2 <- flat_table('ft_num2', ft_cause_rpd) |>
#'   update_according_to(f1)
#' code <- f2 |>
#'   get_transformation_code()
#'
#' @export
get_transformation_code <- function(sdbu) UseMethod("get_transformation_code")

#' @rdname get_transformation_code
#'
#' @export
get_transformation_code.star_database_update <- function(sdbu) {
  sdbu$code
}


#' Get transformation function file
#'
#' From the planned update, it obtains the function with the source code of the
#' transformations performed on the original data in file format.
#'
#' @param sdbu A `star_database_update` object.
#' @param file A string, file name.
#'
#' @return A string, file name.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f1 <- flat_table('ft_num', ft_cause_rpd) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   replace_attribute_values(
#'     name = "When Available",
#'     old = c('1962', '11', '1962-03-14'),
#'     new = c('1962', '3', '1962-01-15')
#'   ) |>
#'   group_dimension_instances(name = "When")
#' f2 <- flat_table('ft_num2', ft_cause_rpd) |>
#'   update_according_to(f1)
#' file <- f2 |>
#'   get_transformation_file()
#'
#' @export
get_transformation_file <- function(sdbu, file) UseMethod("get_transformation_file")

#' @rdname get_transformation_file
#'
#' @export
get_transformation_file.star_database_update <- function(sdbu, file = NULL) {
  if (is.null(file)) {
    res <- sdbu$file
  } else {
    file.copy(from = sdbu$file, to = file, overwrite = TRUE)
    res <- file
  }
  res
}


#' @rdname get_star_database
#'
#' @export
get_star_database.star_database_update <- function(db, name = NULL) {
  db$star_database
}


#' Get lookup tables
#'
#' From the planned update, it obtains the lookup tables used to define the data.
#'
#' @param sdbu A `star_database_update` object.
#'
#' @return A list of `flat_table` objects.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f1 <- flat_table('ft_num', ft_cause_rpd) |>
#'   as_star_database(mrs_cause_schema_rpd)
#' f2 <- flat_table('ft_num2', ft_cause_rpd) |>
#'   update_according_to(f1)
#' ft <- f2 |>
#'   get_lookup_tables()
#'
#' @export
get_lookup_tables <- function(sdbu) UseMethod("get_lookup_tables")

#' @rdname get_lookup_tables
#'
#' @export
get_lookup_tables.star_database_update <- function(sdbu) {
  sdbu$star_database$lookup_tables[[1]]
}



#' Get star schema
#'
#' From the planned update, it obtains the star schema used to define the data.
#'
#' @param sdbu A `star_database_update` object.
#'
#' @return A `star_schema` object.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' f1 <- flat_table('ft_num', ft_cause_rpd) |>
#'   as_star_database(mrs_cause_schema_rpd)
#' f2 <- flat_table('ft_num2', ft_cause_rpd) |>
#'   update_according_to(f1)
#' st <- f2 |>
#'   get_star_schema()
#'
#' @export
get_star_schema <- function(sdbu) UseMethod("get_star_schema")

#' @rdname get_star_schema
#'
#' @export
get_star_schema.star_database_update <- function(sdbu) {
  sdbu$star_database$schemas[[1]]
}
