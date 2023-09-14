#' `flat_table` S3 class
#'
#' Creates a `flat_table` object.
#'
#' The objective is to allow the transformation of flat tables.
#'
#' We can also indicate the value that is used in the data with undefined values.
#'
#' @param name A string.
#' @param instances A `tibble`, table of instances.
#' @param unknown_value A string, value used to replace empty and NA values in
#' attributes.
#'
#' @return A `flat_table` object.
#'
#' @family flat table definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris)
#'
#' ft <- flat_table('ft_num', ft_num)
#'
#' @export
flat_table <- function(name = NULL, instances, unknown_value = NULL) {
  stopifnot("Missing flat table name." = !is.null(name))
  if (!tibble::is_tibble(instances)) {
    if (is.data.frame(instances)) {
      instances <- tibble::as_tibble(instances)
    } else {
      stop("A tibble with the instances was expected.")
    }
  }
  if (is_empty_string(unknown_value)) {
    unknown_value <- get_default_unknown_value()
  } else {
    unknown_value <- unknown_value[1]
  }
  attributes <- NULL
  measures <- NULL
  types <- dplyr::summarise_all(instances, class)
  names <- names(types)
  for (t in seq_along(types)) {
    if (types[t] %in% c("integer", "double", "integer64", "numeric")) {
      measures <- c(measures, names[t])
    } else {
      attributes <- c(attributes, names[t])
    }
  }
  n_row <- nrow(instances)
  # all attributes of type character
  instances[, attributes] <- data.frame(lapply(instances[, attributes], as.character), stringsAsFactors = FALSE)

  if (n_row == 1) {
    instances <- tibble::as_tibble_row(instances)
  } else {
    instances <- tibble::as_tibble(instances)
  }

  structure(
    list(
      name = name,
      table = instances[, c(attributes, measures)],
      unknown_value = unknown_value,
      operations = star_operation() |>
        add_operation("flat_table", c(name, unknown_value), attributes, measures),
      pk_attributes = NULL,
      lookup_tables = list(),
      attributes = attributes,
      measures = measures
    ),
    class = "flat_table"
  )
}


#' @rdname snake_case
#'
#' @export
snake_case.flat_table <- function(db) {
  db$name <- snakecase::to_snake_case(db$name)
  names(db$table) <- snakecase::to_snake_case(names(db$table))
  if (!is.null(db$pk_attributes)) {
    db$pk_attributes <- snakecase::to_snake_case(db$pk_attributes)
  }
  if (!is.null(db$attributes)) {
    db$attributes <- snakecase::to_snake_case(db$attributes)
  }
  if (!is.null(db$measures)) {
    db$measures <- snakecase::to_snake_case(db$measures)
  }
  db$operations <- add_operation(db$operations, "snake_case")
  db
}


#' @rdname get_attribute_names
#'
#' @export
get_attribute_names.flat_table <- function(db, name = NULL, ordered = FALSE, as_definition = FALSE) {
  transform_names(names = db$attributes, ordered, as_definition)
}


#' @rdname get_measure_names
#'
#' @export
get_measure_names.flat_table <- function(db, name = NULL, ordered = FALSE, as_definition = FALSE) {
  transform_names(names = db$measures, ordered, as_definition)
}


#' @rdname set_attribute_names
#'
#' @export
set_attribute_names.flat_table <- function(db, name = NULL, old = NULL, new) {
  old <- validate_attributes(db$attributes, old)
  stopifnot("There are repeated attributes." = length(new) == length(unique(new)))
  stopifnot("The number of new names must be equal to the number of names to replace." = length(old) == length(new))
  names <- replace_names(db$attributes, old, new)
  if (length(c(names, db$measures)) != length(unique(snakecase::to_snake_case(c(names, db$measures))))) {
    stop("There are repeated attributes or measures.")
  }
  db$table <- db$table[, c(db$attributes, db$measures)]
  names(db$table) <- c(names, db$measures)
  db$attributes <- names
  db$operations <- add_operation(db$operations, "set_attribute_names", name, old, new)
  db
}


#' @rdname set_measure_names
#'
#' @export
set_measure_names.flat_table <- function(db, name = NULL, old = NULL, new) {
  old <- validate_measures(db$measures, old)
  stopifnot("There are repeated measures." = length(new) == length(unique(new)))
  stopifnot("The number of new names must be equal to the number of names to replace." = length(old) == length(new))
  names <- replace_names(db$measures, old, new)
  if (length(c(names, db$attributes)) != length(unique(snakecase::to_snake_case(c(names, db$attributes))))) {
    stop("There are repeated attributes or measures.")
  }
  db$table <- db$table[, c(db$attributes, db$measures)]
  names(db$table) <- c(db$attributes, names)
  db$measures <- names
  db$operations <- add_operation(db$operations, "set_measure_names", name, old, new)
  db
}


#' @rdname get_similar_attribute_values
#'
#' @export
get_similar_attribute_values.flat_table <-
  function(db,
           name = NULL,
           attributes = NULL,
           exclude_numbers = FALSE,
           col_as_vector = NULL) {
    attributes <- validate_attributes(db$attributes, attributes)
    get_similar_values_table(db$table[, attributes], attributes, exclude_numbers, col_as_vector)
  }


#' @rdname get_similar_attribute_values_individually
#'
#' @export
get_similar_attribute_values_individually.flat_table <-
  function(db,
           name = NULL,
           attributes = NULL,
           exclude_numbers = FALSE,
           col_as_vector = NULL) {
    attributes <- validate_attributes(db$attributes, attributes)
    res <- list()
    for (at in attributes) {
      la <- get_similar_attribute_values(db, name, at, exclude_numbers, col_as_vector)
      if (length(la) > 0) {
        res <- c(res, la)
      }
    }
    res
  }


#' @rdname get_unique_attribute_values
#'
#' @export
get_unique_attribute_values.flat_table <- function(db,
                                                   name = NULL,
                                                   attributes = NULL,
                                                   col_as_vector = NULL) {
  attributes <- validate_attributes(db$attributes, attributes)
  get_unique_values_table(db$table[, attributes], col_as_vector)
}


#' @rdname replace_attribute_values
#'
#' @export
replace_attribute_values.flat_table <- function(db, name = NULL, attributes = NULL, old, new) {
  attributes <- validate_attributes(db$attributes, attributes)
  n_att <- length(attributes)
  stopifnot(
    "The number of new values must be equal to the number of dimension attributes." = n_att == length(new)
  )
  if (n_att > 1) {
    stopifnot("The number of old and new values must be equal." = length(old) == length(new))
  }
  # update various old values
  if (n_att == 1 &
      length(new) == 1 & length(old) > 1) {
    various_old <- TRUE
  } else {
    various_old <- FALSE
  }
  table <- db$table
  pos_id <- ncol(table) + 1
  id <- 1:nrow(table)
  table[pos_id] <- id
  if (!various_old) {
    for (j in 1:n_att) {
      table <- table[table[, attributes[j]] == old[j], ]
    }
    table <- table[stats::complete.cases(table[, attributes]), ]
    r <- as.vector(table[pos_id])[[1]]
    if (length(r) > 0) {
      for (i in 1:length(r)) {
        for (j in 1:n_att) {
          db$table[id == r[i], attributes[j]] <- new[j]
        }
      }
    }
  } else {
    # 1 attribute and n old values
    for (j in 1:length(old)) {
      db$table[, attributes[1]] <-
        apply(db$table[, attributes[1], drop = FALSE], 2, function(x)
          sub(old[j], new[1], x, fixed = TRUE))
    }
  }
  db$operations <-
    add_operation(db$operations, "replace_attribute_values", attributes, old, new)
  db
}

#-------------------------------------------------------------------------------


#' Get the table of the flat table
#'
#' Obtain the table of a flat table.
#'
#' @param ft A `flat_table` object.
#'
#' @return A `tibble`, the table.
#'
#' @family flat table definition functions
#' @seealso \code{\link{select_attributes}}, \code{\link{select_measures}}
#'
#' @examples
#'
#' table <- flat_table('iris', iris) |>
#'   get_table()
#'
#' @export
get_table <- function(ft) UseMethod("get_table")

#' @rdname get_table
#'
#' @export
get_table.flat_table <- function(ft) {
  ft$table
}

#' Get the unknown value defined
#'
#' Obtain the unknown value of a flat table.
#'
#' @param ft A `flat_table` object.
#'
#' @return A string.
#'
#' @family flat table definition functions
#' @seealso \code{\link{select_attributes}}, \code{\link{select_measures}}
#'
#' @examples
#'
#' table <- flat_table('iris', iris) |>
#'   get_unknown_value_defined()
#'
#' @export
get_unknown_value_defined <- function(ft) UseMethod("get_unknown_value_defined")

#' @rdname get_unknown_value_defined
#'
#' @export
get_unknown_value_defined.flat_table <- function(ft) {
  ft$unknown_value
}


#' Get unknown attribute values
#'
#' Obtain the instances that have an empty or unknown value in any given attribute.
#' If no attribute is given, all are considered.
#'
#' If a name is indicated in the `col_as_vector` parameter, it includes a column
#' with the data in vector form to be used in other functions.
#'
#' @param ft A `flat_table` object.
#' @param attributes A vector of strings, attribute names.
#' @param col_as_vector A string, name of the column to include a vector of values.
#'
#' @return A `tibble` with unknown values in instances.
#'
#' @family flat table definition functions
#' @seealso \code{\link{replace_empty_values}}
#'
#' @examples
#'
#' iris2 <- iris
#' iris2[10, 'Species'] <- NA
#' instances <- flat_table('iris', iris2) |>
#'   get_unknown_values()
#'
#' @export
get_unknown_values <- function(ft, attributes, col_as_vector) UseMethod("get_unknown_values")

#' @rdname get_unknown_values
#'
#' @export
get_unknown_values.flat_table <- function(ft, attributes = NULL, col_as_vector = NULL) {
  attributes <- validate_attributes(ft$attributes, attributes)
  table <- replace_empty_values_table(ft$table, attributes, unknown_value = ft$unknown_value)
  table <- table[, attributes]
  or_res <- rep(FALSE, nrow(table))
  for (j in 1:length(attributes)) {
    or_res <- or_res | (table[, attributes[j]] == ft$unknown_value)
  }
  table <- ft$table[or_res, attributes]
  table <- dplyr::arrange_all(unique(table))
  if (!is.null(col_as_vector)) {
    table <- add_dput_column(table, col_as_vector)
  }
  table
}


#' Get a star database from a flat table
#'
#' Obtain a star database from the flat table and a star schema.
#'
#' @param ft A `flat_table` object.
#' @param schema A `star_schema` object.
#'
#' @return A `star_database` object.
#'
#' @family flat table definition functions
#' @seealso \code{\link{star_schema}}, \code{\link{star_database}}
#'
#' @examples
#'
#' db <- flat_table('ft_num', ft_num) |>
#'   as_star_database(mrs_cause_schema)
#'
#' @export
as_star_database <- function(ft, schema) UseMethod("as_star_database")

#' @rdname as_star_database
#'
#' @export
as_star_database.flat_table <-
  function(ft, schema) {
    star_database_with_previous_operations(
      schema,
      instances = ft$table,
      unknown_value = ft$unknown_value,
      operations = ft$operations
    )
  }
