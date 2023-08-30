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

#' Get the names of the attributes of a flat table
#'
#' Obtain the names of the attributes of a flat table.
#'
#' @param ft A `flat_table` object.
#' @param ordered A boolean, sort names alphabetically.
#' @param as_definition A boolean, as the definition of the vector in R.
#'
#' @return A vector of strings or a `tibble`, attribute names.
#'
#' @family query functions of a flat table
#' @seealso \code{\link{replace_values}}
#'
#' @examples
#'
#' names <- flat_table('iris', iris) |>
#'   get_attribute_names()
#'
#' @export
get_attribute_names <- function(ft, ordered, as_definition) UseMethod("get_attribute_names")

#' @rdname get_attribute_names
#'
#' @export
get_attribute_names.flat_table <- function(ft, ordered = FALSE, as_definition = FALSE) {
  transform_names(names = ft$attributes, ordered, as_definition)
}


# Internal #####################################################################

#' For each row, add a vector of values
#'
#' @param names A vector of strings, names of attributes or measures.
#' @param ordered A boolean, sort names alphabetically.
#' @param as_definition A boolean, as the definition of the vector in R.
#'
#' @return A vector of strings, attribute or measure names.
#'
#' @keywords internal
transform_names <- function(names, ordered, as_definition) {
  if (ordered) {
    names <- sort(names)
  }
  if (as_definition & length(names) > 0) {
    v <- tibble::as_tibble(data.frame(matrix(names, ncol = length(names), nrow = 1)))
    v <- add_dput_column(v, column = 'vector')
    names <- v$vector
  }
  names
}

