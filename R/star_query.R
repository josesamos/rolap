

#' Get star query schema
#'
#' Obtain the star database schema to perform queries.
#'
#' @param db A `star_database` object.
#'
#' @return A list of objects (facts and dimensions schemes).
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
#' sq <- star_query(mrs_db)
#'
#' @export
star_query <- function(db = NULL) {
  schema <- get_star_query_schema(db)

  structure(list(schema = schema,
                 query = list()),
            class = "star_query")
}


#' Select fact
#'
#' To define the fact to be consulted, its name is indicated, optionally, a
#' vector of names of selected measures and another of aggregation functions are
#' also indicated.
#'
#' If there is only one fact table, it is the one that is considered if no name
#' is indicated.
#'
#' If no measure is given, only the one corresponding to the number of aggregated
#' rows will be included (it is always included).
#'
#' If no aggregation function is given, those defined for the measures are considered.
#'
#' @param sq A `star_query` object.
#' @param name A string, name of the fact.
#' @param measures A vector of measure names.
#' @param agg_functions A vector of aggregation function names, each one for its
#'   corresponding measure. They can be SUM, MAX or MIN.
#'
#' @return A `star_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' sq <- star_query(mrs_db) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = "all_deaths",
#'     agg_functions = "MAX"
#'   )
#'
#' sq <- star_query(mrs_db) |>
#'   select_fact(name = "mrs_age",
#'               measures = "all_deaths")
#'
#' sq <- star_query(mrs_db) |>
#'   select_fact(name = "mrs_age")
#'
#' @export
select_fact <- function(sq, name, measures, agg_functions) {
  UseMethod("select_fact")
}

#' @rdname select_fact
#' @export
select_fact.star_query <- function(sq,
                                   name = NULL,
                                   measures = NULL,
                                   agg_functions = NULL) {
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
  stopifnot("There are repeated measures" = length(measures) == length(unique(measures)))
  if (!is.null(measures)) {
    validate_names(names(sq$schema$fact[[name]]$measure), measures, concept = 'measure')
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
    agg_functions <- sq$schema$fact[[name]]$measure[measures]
  }

  agg_functions <- c(agg_functions, sq$schema$fact[[name]]$nrow_agg)

  if (is.null(sq$query$fact)) {
    sq$query$fact <- list(measure = agg_functions)
    names(sq$query$fact) <- name
  } else {
    fact_names <- names(sq$query$fact)
    sq$query$fact <- c(sq$query$fact, list(measure = agg_functions))
    names(sq$query$fact) <- c(fact_names, name)
  }
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
#' sq <- star_query(mrs_db) |>
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
  if (is.null(sq$query$dimension)) {
    sq$query$dimension <- list(list(attribute = attributes))
    names(sq$query$dimension) <- name
  } else {
    dimension_names <- names(sq$query$dimension)
    sq$query$dimension <- c(sq$query$dimension, list(list(attribute = attributes)))
    names(sq$query$dimension) <- c(dimension_names, name)
  }
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
#' sq <- star_query(mrs_db) |>
#'   filter_dimension(name = "when", week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston")
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
  filter <- substitute(alist(...))
  if (is.null(sq$query$dimension)) {
    sq$query$dimension <- list(list(filter = filter))
    names(sq$query$dimension) <- name
  } else {
    dimension_names <- names(sq$query$dimension)
    sq$query$dimension <- c(sq$query$dimension, list(list(filter = filter)))
    names(sq$query$dimension) <- c(dimension_names, name)
  }
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
#' @param sq A `star_query` object.
#' @param unify_by_grain A boolean, unify facts with the same grain.
#'
#' @return A `star_query` object.
#'
#' @family query functions
#'
#' @examples
#'
#' ms <- star_query(mrs_db) |>
#'   select_dimension(name = "where",
#'                    attributes = c("city", "state")) |>
#'   select_dimension(name = "when",
#'                    attributes = c("when_happened_year")) |>
#'   select_fact(
#'     name = "mrs_age",
#'     measures = c("all_deaths"),
#'     agg_functions = c("MAX")
#'   ) |>
#'   select_fact(
#'     name = "mrs_cause",
#'     measures = c("pneumonia_and_influenza_deaths", "other_deaths")
#'   ) |>
#'   filter_dimension(name = "when", when_happened_week <= "03") |>
#'   filter_dimension(name = "where", city == "Boston") |>
#'   run_query()
#'
#' @export
run_query <- function(sq, unify_by_grain = TRUE) {
  UseMethod("run_query")
}



#' @rdname run_query
#' @export
run_query.star_query <- function(sq, unify_by_grain = TRUE) {
  sq <- define_selected_facts(sq)
  sq <- define_selected_dimensions(sq)
  sq <- filter_selected_instances(sq)
  sq <- delete_unused_foreign_keys(sq)
  sq <- remove_duplicate_dimension_rows(sq)
  sq <- group_facts(sq)
  if (unify_by_grain) {
    sq <- unify_facts_by_grain (sq)
  }
  class(sq$output) <- class(sq$input)[1]
  sq$output
}


#' Define selected facts
#'
#' Measure names are stored as the names of the columns with the aggregation
#' functions.
#'
#' @param sq A `star_query` object.
#'
#' @return A `star_query` object.
#'
#' @keywords internal
define_selected_facts <- function(sq) {
  for (name in names(sq$fact)) {
    # measure names are the names of the columns with the aggregation functions
    sq$output$fact[[name]] <-
      sq$input$fact[[name]][, c(attr(sq$input$fact[[name]], "foreign_keys"), names(sq$fact[[name]]))]
    attr(sq$output$fact[[name]], "measures") <- names(sq$fact[[name]])
    attr(sq$output$fact[[name]], "agg_functions") <- sq$fact[[name]]
  }
  sq
}

#' Define selected dimensions
#'
#' Include the selected dimensions and only the selected attributes in them.
#'
#' @param sq A `star_query` object.
#'
#' @return A `star_query` object.
#'
#' @keywords internal
define_selected_dimensions <- function(sq) {
  for (name in names(sq$dimension)) {
    sq$output$dimension[[name]] <- sq$input$dimension[[name]][, sq$dimension[[name]]]
  }
  sq
}

#' Filter selected instances
#'
#' For some dimensions the instances to include have been defined, we have the
#' value of the primary key. They are filtered for both facts and dimensions.
#'
#' @param sq A `star_query` object.
#'
#' @return A `star_query` object.
#'
#' @keywords internal
filter_selected_instances <- function(sq) {
  for (name in names(sq$key)) {
    # filter facts
    for (f in names(sq$output$fact)) {
      key <- sprintf("%s_key", name)
      if (key %in% names(sq$output$fact[[f]])) {
        sq$output$fact[[f]] <-
          sq$output$fact[[f]][sq$output$fact[[f]][[key]] %in% sq$key[[name]], ]
      }
    }
    # filter dimensions
    if (name %in% names(sq$output$dimension)) {
      sq$output$dimension[[name]] <-
        sq$output$dimension[[name]][sq$output$dimension[[name]][[1]] %in% sq$key[[name]], ]
    }
  }
  sq
}

#' Delete unused foreign keys
#'
#' In facts, remove foreign keys from dimensions not included in the result.
#'
#' @param sq A `star_query` object.
#'
#' @return A `star_query` object.
#'
#' @keywords internal
delete_unused_foreign_keys <- function(sq) {
  for (name in names(sq$output$fact)) {
    fk <- attr(sq$output$fact[[name]], "foreign_keys")
    key_dimensions <- sprintf("%s_key", names(sq$dimension))
    col <-
      which(names(sq$output$fact[[name]]) %in% generics::setdiff(fk, key_dimensions))
    if (length(col) > 0) {
      sq$output$fact[[name]] <- sq$output$fact[[name]][,-c(col)]
    }
    attr(sq$output$fact[[name]], "foreign_keys") <-
      generics::intersect(fk, key_dimensions)
  }
  sq
}

#' Remove duplicate dimension rows
#'
#' After selecting only a few columns of the dimensions, there may be rows with
#' duplicate values. We eliminate duplicates and adapt facts to the new
#' dimensions.
#'
#' @param sq A `star_query` object.
#'
#' @return A `star_query` object.
#'
#' @keywords internal
remove_duplicate_dimension_rows <- function(sq) {
  # remove duplicate dimension rows
  for (name in names(sq$dimension)) {
    # remove duplicates and sort
    ft <-
      dplyr::arrange_all(tibble::as_tibble(unique(sq$output$dimension[[name]][, -1])))
    if (nrow(ft) < nrow(sq$output$dimension[[name]])) {
      # add surrogate primary key
      # := variables for parameter names
      # !! expands the expression into a string
      ft <-
        tibble::add_column(ft,!!sprintf("%s_key", name) := 1:nrow(ft), .before = 1)
      for (f in names(sq$output$fact)) {
        key <- sprintf("%s_key", name)
        if (key %in% names(sq$output$fact[[f]])) {
          sq$output$fact[[f]] <-
            dereference_dimension(sq$output$fact[[f]], sq$output$dimension[[name]])
          sq$output$fact[[f]] <-
            reference_dimension(sq$output$fact[[f]], ft, names(ft)[-1])
        }
      }
      class <- class(sq$output$dimension[[name]])
      sq$output$dimension[[name]] <- ft
      class(sq$output$dimension[[name]]) <- class
    }
  }
  sq
}

#' Group facts
#'
#' Once the external keys have been possibly replaced, group the rows of facts.
#'
#' @param sq A `star_query` object.
#'
#' @return A `star_query` object.
#'
#' @keywords internal
group_facts <- function(sq) {
  for (name in names(sq$output$fact)) {
    sq$output$fact[[name]] <- group_table(sq$output$fact[[name]])
  }
  sq
}

#' Unify facts by grain
#'
#' @param sq A `star_query` object.
#'
#' @return A `star_query` object.
#'
#' @keywords internal
unify_facts_by_grain <- function(sq) {
  fact <- NULL
  unified_fact <- NULL
  names_fact <- names(sq$output$fact)
  for (i in seq_along(names_fact)) {
    if (!(names_fact[i] %in% unified_fact)) {
      fact[[names_fact[i]]] <- sq$output$fact[[names_fact[i]]]
      fk_i <- attr(sq$output$fact[[names_fact[i]]], "foreign_keys")
      agg <- list(fact[[names_fact[i]]])
      for (j in seq_along(names_fact)[seq_along(names_fact) > i]) {
        fk_j <- attr(sq$output$fact[[names_fact[j]]], "foreign_keys")
        if (generics::setequal(fk_i, fk_j)) {
          unified_fact <- c(unified_fact, names_fact[j])
          fact2 <- sq$output$fact[[names_fact[j]]][, c(fk_i, attr(sq$output$fact[[names_fact[j]]], "measures"))]

          for (m in attr(fact2, "measures")) {
            m_new <- sprintf("%s_%s", names_fact[j], m)
            names(fact2)[which(names(fact2) == m)] <- m_new
            attr(fact2, "measures")[which(attr(fact2, "measures") == m)] <- m_new
            names(attr(fact2, "agg_functions"))[which(names(attr(fact2, "agg_functions")) == m)] <- m_new
          }

          attr(fact[[names_fact[i]]], "measures") <-
            c(attr(fact[[names_fact[i]]], "measures"), attr(fact2, "measures"))
          attr(fact[[names_fact[i]]], "agg_functions") <-
            c(attr(fact[[names_fact[i]]], "agg_functions"), attr(fact2, "agg_functions"))

          agg <- c(agg, list(fact2))
        }
      }
      if (length(agg) > 1) {
        if (is.null(fk_i)) {
          par_by = character()
        } else {
          par_by = fk_i
        }
        at <- attributes(fact[[names_fact[i]]])
        fact[[names_fact[i]]] <- purrr::reduce(agg, dplyr::inner_join, by = par_by)
        class(fact[[names_fact[i]]]) <- at$class
        attr(fact[[names_fact[i]]], "name") <- at$name
        attr(fact[[names_fact[i]]], "measures") <- at$measures
        attr(fact[[names_fact[i]]], "agg_functions") <- at$agg_functions
        attr(fact[[names_fact[i]]], "nrow_agg") <- at$nrow_agg
      }
    }
  }
  sq$output$fact <- fact
  sq
}
