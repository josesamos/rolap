
#' `star_query` S3 class
#'
#' An empty `star_query` object is created where we can select facts and
#' measures, dimensions, dimension attributes and filter dimension rows.
#'
#' @param db A `star_database` object.
#'
#' @return A `star_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' sq <- mrs_db |>
#'   star_query()
#'
#' @export
star_query <- function(db)
  UseMethod("star_query")

#' @rdname star_query
#'
#' @export
star_query.star_database <- function(db) {
  schema <- get_star_query_schema(db)

  structure(list(
    schema = schema,
    query = list(fact = list(),
                 dimension = list())
  ),
  class = "star_query")
}


#' Get star query schema
#'
#' Obtain the star database schema to perform queries.
#'
#' @param db A `star_database` object.
#'
#' @return A star database schema, list of fact and dimension schemes.
#'
#' @keywords internal
get_star_query_schema <- function(db) {
  fact <- vector("list", length = length(db$facts))
  dimension = vector("list", length = length(db$dimensions))
  names(fact) <- names(db$facts)
  names(dimension) <- names(db$dimensions)

  for (f in names(fact)) {
    fact[[f]]$fk <- db$facts[[f]]$surrogate_keys
    l <- length(db$facts[[f]]$agg)
    fact[[f]]$nrow_agg <- db$facts[[f]]$agg[l]
    fact[[f]]$measure <- db$facts[[f]]$agg[-l]
  }

  for (d in names(dimension)) {
    dimension[[d]]$pk <- db$dimensions[[d]]$surrogate_key
    attribute <-
      setdiff(names(db$dimensions[[d]]$table),
              db$dimensions[[d]]$surrogate_key)
    dimension[[d]]$attribute <- attribute
    dimension[[d]]$table <- db$dimensions[[d]]$table[0, ]
  }

  list(fact = fact,
       dimension = dimension)
}


#' Select fact
#'
#' To define the fact to be consulted, its name is indicated, optionally, a
#' vector of names of selected measures, another of aggregation functions and
#' another of new names for measures are also indicated.
#'
#' If there is only one fact table, it is the one that is considered if no name
#' is indicated.
#'
#' If no aggregation function is given, those defined for the measures are considered.
#'
#' If no new names are given, the original names will be considered. If the
#' aggregation function is different from the one defined by default, it will be
#' included as a prefix to the name.
#'
#' @param sq A `star_query` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names, each one for its
#'   corresponding measure. They can be SUM, MAX or MIN.
#' @param new A vector of measure new names.
#' @param nrow_agg A string, name of a new measure that represents the COUNT
#'   of rows aggregated for each resulting row.
#'
#' @return A `star_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' sq <- mrs_db |>
#'   star_query()
#'
#' sq_1 <- sq |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = "all_deaths",
#'     agg_functions = "MAX"
#'   )
#'
#' sq_2 <- sq |>
#'   select_fact(name = "mrs_age",
#'               measures = "all_deaths")
#'
#' sq_3 <- sq |>
#'   select_fact(name = "mrs_age")
#'
#' @export
select_fact <- function(sq, name, measures, agg_functions, new, nrow_agg) {
  UseMethod("select_fact")
}

#' @rdname select_fact
#' @export
select_fact.star_query <- function(sq,
                                   name = NULL,
                                   measures = NULL,
                                   agg_functions = NULL,
                                   new = NULL,
                                   nrow_agg = NULL) {
  if (is.null(nrow_agg)) {
    nrow_agg <- 'nrow_agg_sq'
  }
  if (is.null(name)) {
    if (length(sq$schema$fact) == 1) {
      name <- names(sq$schema$fact)
    } else {
      stopifnot("The name of the fact must be indicated." = !is.null(name))
    }
  } else {
    validate_names(names(sq$schema$fact), name, concept = 'fact name')
  }
  stopifnot("The fact had already been selected." = !(name %in% names(sq$query$fact)))
  measure_names <- c(sq$schema$fact[[name]]$measure, sq$schema$fact[[name]]$nrow_agg)
  if (!is.null(measures)) {
    validate_names(names(measure_names), measures, concept = 'measure', repeated = TRUE)
  }
  if (!is.null(agg_functions)) {
    validate_names(c("SUM", "MAX", "MIN"),
                   agg_functions,
                   concept = 'aggregation function',
                   repeated = TRUE)
    stopifnot(
      "Measures and aggregation functions do not correspond." = length(measures) == length(agg_functions)
    )
    names(agg_functions) <- measures
  } else {
    agg_functions <- measure_names[measures]
  }
  if (!is.null(new)) {
    stopifnot("There are repeated names among the new measure names." = length(new) == length(unique(new)))
    stopifnot(
      "Measures and new measure names do not correspond." = length(measures) == length(new)
    )
  }
  if (!(nrow_agg %in% names(agg_functions))) {
    agg_functions[nrow_agg] <- 'SUM'
  } else {
    stop("The name of the new measure that represents the COUNT corresponds to another measure already defined.")
  }

  mnames <- paste0(tolower(agg_functions), '_', names(agg_functions))
  stopifnot("There are repeated measures with the same aggregation function." = length(mnames) == length(unique(mnames)))

  fact_names <- names(sq$query$fact)
  sq$query$fact <-
    c(sq$query$fact, list(list(measure = agg_functions, new = new)))
  names(sq$query$fact) <- c(fact_names, name)

  sq
}


#' Select dimension
#'
#' To add a dimension in a `star_query` object, we have to define its name and a
#' subset of the dimension attributes. If only the name of the dimension is
#' indicated, it is considered that all its attributes should be added.
#'
#' @param sq A `star_query` object.
#' @param name A string, name of the dimension.
#' @param attributes A vector of attribute names.
#'
#' @return A `star_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' sq <- mrs_db |>
#'   star_query() |>
#'   select_dimension(name = "where",
#'                   attributes = c("city", "state")) |>
#'   select_dimension(name = "when")
#'
#' @export
select_dimension <- function(sq, name, attributes) {
  UseMethod("select_dimension")
}

#' @rdname select_dimension
#' @export
select_dimension.star_query <- function(sq,
                                        name = NULL,
                                        attributes = NULL) {
  stopifnot("The name of the dimension must be indicated." = !is.null(name))
  validate_names(names(sq$schema$dimension), name, concept = 'dimension name')
  stopifnot("The dimension had already been selected." = is.null(sq$query$dimension[[name]]$attribute))
  attributes <- validate_names(sq$schema$dimension[[name]]$attribute, attributes, concept = 'attribute')
  sq$query$dimension[[name]]$attribute <- attributes
  sq
}


#' Filter dimension
#'
#' Allows you to define selection conditions for dimension rows.
#'
#' Conditions can be defined on any attribute of the dimension (not only on
#' attributes selected in the query for the dimension). The selection is made
#' based on the function `dplyr::filter`. Conditions are defined in exactly the
#' same way as in that function.
#'
#' @param sq A `star_query` object.
#' @param name A string, name of the dimension.
#' @param ... Conditions, defined in exactly the same way as in `dplyr::filter`.
#'
#' @return A `star_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' sq <- mrs_db |>
#'   star_query() |>
#'   filter_dimension(name = "when", week <= " 3") |>
#'   filter_dimension(name = "where", city == "Cambridge")
#'
#' @export
filter_dimension <- function(sq, name, ...) {
  UseMethod("filter_dimension")
}

#' @rdname filter_dimension
#' @export
filter_dimension.star_query <- function(sq, name = NULL, ...) {
  stopifnot("The name of the dimension must be indicated." = !is.null(name))
  validate_names(names(sq$schema$dimension), name, concept = 'dimension name')
  stopifnot("The dimension had already been filtered." = is.null(sq$query$dimension[[name]]$filter))
  dplyr::filter(sq$schema$dimension[[name]]$table, ...)
  filter <- as.character(substitute(alist(...)))
  # checking that it is correct (inverse operation)
  # dplyr::filter(sq$schema$dimension[[name]]$table, eval(parse(text = filter)))
  sq$query$dimension[[name]]$filter <- filter
  sq
}

# ------------------------------------------------------------------------------


#' Run query
#'
#' Once we have selected the facts, dimensions and defined the conditions on the
#' instances, we can execute the query to obtain the result.
#'
#' As an option, we can indicate if we do not want to unify the facts in the
#' case of having the same grain.
#'
#' @param db A `star_database` object.
#' @param sq A `star_query` object.
#'
#' @return A `star_database` object.
#'
#' @family query functions
#'
#' @examples
#'
#' sq <- mrs_db |>
#'   star_query() |>
#'   select_dimension(name = "where",
#'                    attributes = c("city", "state")) |>
#'   select_dimension(name = "when",
#'                    attributes = "year") |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = "all_deaths",
#'     agg_functions = "MAX"
#'   ) |>
#'   select_fact(
#'     name = "mrs_cause",
#'     measures = c("pneumonia_and_influenza_deaths", "all_deaths")
#'   ) |>
#'   filter_dimension(name = "when", week <= " 3") |>
#'   filter_dimension(name = "where", city == "Bridgeport")
#'
#' mrs_db_2 <- mrs_db |>
#'   run_query(sq)
#'
#' @export
run_query <- function(db, sq)
  UseMethod("run_query")

#' @rdname run_query
#'
#' @export
run_query.star_database <- function(db, sq) {
  stopifnot("At least one fact table must be selected." = length(sq$query$fact) > 0)
  db <- apply_select_fact(db, sq)
  db <- apply_filter_dimension(db, sq)
  db <- apply_select_dimension(db, sq)
  db <- remove_duplicate_dimension_rows(db)
  db <- group_facts(db)

  db$operations <- star_operation()
  db$lookup_tables <- list()
  db$schemas <- list()
  db$refresh <- list()
  db$deploy <- list()
  db$rpd <- list()
  db$geo <- filter_geo_attributes(db)

  db
}


#' Apply select fact
#'
#' Select the facts, measures and define the aggregation functions.
#'
#' @param db A `star_database` object.
#' @param sq A `star_query` object.
#'
#' @param db A `star_database` object.
#'
#' @keywords internal
apply_select_fact <- function(db, sq) {
  names <- names(sq$query$fact)
  db$facts <- db$facts[names]
  for (f in names) {
    agg <- sq$query$fact[[f]]$measure
    pk <- db$facts[[f]]$surrogate_keys
    countvar <- names(agg)[length(agg)]
    db$facts[[f]]$table[countvar] <- 1L
    db$facts[[f]]$table <- db$facts[[f]]$table[c(pk, names(agg))]
    new <- sq$query$fact[[f]]$new
    if (!is.null(new)) {
      measure_names <- c(new, countvar)
    } else {
      measure_names <- names(agg)
      for (i in seq_along(measure_names[-length(measure_names)])) {
        if (agg[i] != db$facts[[f]]$agg[measure_names[i]]) {
          measure_names[i] <- paste0(tolower(agg[i]), '_',  measure_names[i])
        }
      }
    }
    names(db$facts[[f]]$table) <- c(pk, measure_names)
    names(agg) <- measure_names
    db$facts[[f]]$agg <- agg
  }
  db
}


#' Apply filter dimension
#'
#' Select the instances of the dimensions that meet the defined conditions.
#'
#' @param db A `star_database` object.
#' @param sq A `star_query` object.
#'
#' @param db A `star_database` object.
#'
#' @keywords internal
apply_filter_dimension <- function(db, sq) {
  names <- names(sq$query$dimension)
  for (d in names) {
    filter <- sq$query$dimension[[d]]$filter
    if (!is.null(filter)) {
      db$dimensions[[d]]$table <-
        dplyr::filter(db$dimensions[[d]]$table, eval(parse(text = filter)))
      # filter facts
      for (f in names(db$facts)) {
        if (d %in% db$facts[[f]]$dim_int_names) {
          pk <- db$dimensions[[d]]$surrogate_key
          db$facts[[f]]$table <-
            dplyr::inner_join(db$facts[[f]]$table, db$dimensions[[d]]$table[, pk], by = pk)
        }
      }
    }
  }
  db
}


#' Apply select dimension
#'
#' Select dimensions and attributes.
#'
#' @param db A `star_database` object.
#' @param sq A `star_query` object.
#'
#' @param db A `star_database` object.
#'
#' @keywords internal
apply_select_dimension <- function(db, sq) {
  names <- names(sq$query$dimension)
  sel_names <- NULL
  for (d in names) {
    attribute <- sq$query$dimension[[d]]$attribute
    if (!is.null(attribute)) {
      sel_names <- c(sel_names, d)
      pk <- db$dimensions[[d]]$surrogate_key
      db$dimensions[[d]]$table <- db$dimensions[[d]]$table[, c(pk, attribute)]
    }
  }
  # delete from facts
  all_dimensions <- names(db$dimensions)
  rest_names <- setdiff(all_dimensions, sel_names)
  for (d in rest_names) {
    for (f in names(db$facts)) {
      if (d %in% db$facts[[f]]$dim_int_names) {
        db$facts[[f]]$dim_int_names <- setdiff(db$facts[[f]]$dim_int_names, d)
        stopifnot("At least one dimension must be selected for each fact table." = length(db$facts[[f]]$dim_int_names) > 0)
        pk <- db$dimensions[[d]]$surrogate_key
        db$facts[[f]]$surrogate_keys <- setdiff(db$facts[[f]]$surrogate_keys, pk)
        table_fields <- names(db$facts[[f]]$table)
        table_fields <- setdiff(table_fields, pk)
        db$facts[[f]]$table <- db$facts[[f]]$table[, table_fields]
      }
    }
  }
  db$dimensions <- db$dimensions[sel_names]
  db
}


#' Remove duplicate dimension rows
#'
#' After selecting only a few columns of the dimensions, there may be rows with
#' duplicate values. We eliminate duplicates and adapt facts to the new
#' dimensions.
#'
#' @param db A `star_database` object.
#'
#' @param db A `star_database` object.
#'
#' @keywords internal
remove_duplicate_dimension_rows <- function(db) {
  # remove duplicate dimension rows
  for (d in names(db$dimensions)) {
    # remove duplicates and sort
    pk <- db$dimensions[[d]]$surrogate_key
    attributes <- names(db$dimensions[[d]]$table)
    i <- which(attributes == pk)
    attributes <- attributes[-i]
    tpk <- NULL
    for (f in names(db$facts)) {
      if (pk %in% db$facts[[f]]$surrogate_keys) {
        tpk <- rbind(tpk, db$facts[[f]]$table[, pk])
      }
    }
    tpk <- unique(tpk)
    db$dimensions[[d]]$table <-
      dplyr::inner_join(db$dimensions[[d]]$table, tpk, by = pk)
    ft <-
      dplyr::arrange_all(unique(db$dimensions[[d]]$table[, -i]))
    if (nrow(db$dimensions[[d]]$table) > nrow(ft)) {
      # add surrogate primary key
      # := variables for parameter names
      # !! expands the expression into a string
      ft <-
        tibble::add_column(ft, !!pk := 1:nrow(ft), .before = 1)
      for (f in names(db$facts)) {
        if (pk %in% db$facts[[f]]$surrogate_keys) {
          # join facts to original dimension
          db$facts[[f]]$table <-
            dplyr::select(
              dplyr::inner_join(db$facts[[f]]$table,
                                db$dimensions[[d]]$table,
                                by = pk),
              -tidyselect::all_of(pk)
            )
          # join new dimension to facts
          db$facts[[f]]$table <-
            dplyr::select(
              dplyr::inner_join(db$facts[[f]]$table,
                                ft,
                                by = attributes),
              -tidyselect::all_of(attributes)
            )
        }
      }
      db$dimensions[[d]]$table <- ft
    }
  }
  db
}


#' Group facts
#'
#' Once the external keys have been possibly replaced, group the rows of facts.
#'
#' @param db A `star_database` object.
#'
#' @param db A `star_database` object.
#'
#' @keywords internal
group_facts <- function(db) {
  for (f in names(db$facts)) {
    fk <- db$facts[[f]]$surrogate_keys
    measures <- setdiff(names(db$facts[[f]]$table), fk)
    db$facts[[f]]$table <-
      group_by_keys(
        table = db$facts[[f]]$table[, c(fk, measures)],
        keys = fk,
        measures = measures,
        agg_functions = db$facts[[f]]$agg[measures],
        nrow_agg = NULL
      )
  }
  db
}
