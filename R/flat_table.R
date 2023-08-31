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
  if (is.null(unknown_value)) {
    unknown_value <- "___UNKNOWN___"
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
      operations = star_operation(),
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


#-------------------------------------------------------------------------------

