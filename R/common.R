
# Internal ---------------------------------------------------------------------

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

#' Validate attribute names
#'
#' @param defined_attributes A vector of strings, defined attribute names.
#' @param attributes A vector of strings, new attribute names.
#' @param repeated A boolean, repeated attributes allowed.
#'
#' @return A vector of strings, attribute names.
#'
#' @keywords internal
validate_attributes <- function(defined_attributes, attributes, repeated = FALSE) {
  if (is.null(attributes)) {
    attributes <- defined_attributes
  } else {
    if (!repeated) {
      stopifnot("There are repeated attributes." = length(attributes) == length(unique(attributes)))
    }
    for (attribute in attributes) {
      if (!(attribute %in% defined_attributes)) {
        stop(sprintf(
          "'%s' is not defined as attribute.",
          attribute
        ))
      }
    }
  }
  attributes
}

#' Validate measure names
#'
#' @param defined_measures A vector of strings, defined measure names.
#' @param measures A vector of strings, measure names.
#'
#' @return A vector of strings, measure names.
#'
#' @keywords internal
validate_measures <- function(defined_measures, measures) {
  if (is.null(measures)) {
    measures <- defined_measures
  } else {
    stopifnot("There are repeated measures." = length(measures) == length(unique(measures)))
    for (measure in measures) {
      if (!(measure %in% defined_measures)) {
        stop(sprintf(
          "'%s' is not defined as measure.",
          measure
        ))
      }
    }
  }
  measures
}


#' Replace names
#'
#' @param original A string, original names.
#' @param old A vector of names to replace.
#' @param new A vector of names, new names.
#'
#' @return A vector of strings, names replaced.
#'
#' @keywords internal
replace_names <- function(original, old, new) {
  names <- original
  for (i in seq_along(old)) {
    j <- which(original == old[i])
    names[j] <- new[i]
  }
  names
}


#' For each row, add a vector of values
#'
#' @param v A `tibble`, rows of a dimension table.
#' @param column A string, name of the column to include a vector of values.
#'
#' @return A `tibble`, rows of a dimension table.
#'
#' @keywords internal
add_dput_column <- function(v, column) {
  n_att <- ncol(v)
  v[column] <- ""
  for (i in 1:nrow(v)) {
    dt <- "c("
    for (j in 1:n_att) {
      if (j == 1) {
        sep = ""
      } else {
        sep = ", "
      }
      dt <- paste(dt, sprintf("'%s'", v[i, j]), sep = sep)
    }
    dt <- paste(dt, ")", sep = "")
    v[i, column] <- dt
  }
  v
}


#' Get similar values in a table
#'
#' @param table A `tibble` object.
#' @param attributes A vector of strings, attribute names.
#' @param exclude_numbers A boolean, exclude numbers from comparison.
#' @param col_as_vector A string, name of the column to include a vector of values.
#'
#' @return A vector of `tibble` objects with similar instances.
#'
#' @keywords internal
get_similar_values_table <- function(table, attributes, exclude_numbers, col_as_vector) {
  table <- data.frame(table, stringsAsFactors = FALSE)
  # in one column
  dt <- do.call(paste, c(table, sep = ""))
  # clean values
  dt <- iconv(dt, to = "ASCII//TRANSLIT")
  if (exclude_numbers) {
    dt <- gsub('[0-9]+', '', dt)
  }
  dt <- tolower(dt)
  dt <- snakecase::to_snake_case(dt)
  dt <- gsub("_", "", dt)
  # value frequency
  t_freq <- table(dt)
  t_freq <- t_freq[t_freq > 1]
  # repeated values
  n_freq <- names(t_freq)
  res <- list()
  for (i in seq_along(n_freq)) {
    v <- table[dt == n_freq[i], attributes]
    v <- dplyr::arrange_all(unique(tibble::as_tibble(v)))
    if (nrow(v) > 1) {
      names(v) <- attributes
      if (!is.null(col_as_vector)) {
        v <- add_dput_column(v, col_as_vector)
      }
      res <- c(res, list(v))
    }
  }
  res
}


#' Get unique values in a table
#'
#' @param table A `tibble` object.
#' @param col_as_vector A string, name of the column to include a vector of values.
#'
#' @return A vector of `tibble` objects with similar instances.
#'
#' @keywords internal
get_unique_values_table <- function(table, col_as_vector) {
  dt <- data.frame(table, stringsAsFactors = FALSE)
  dt <- dplyr::arrange_all(unique(tibble::as_tibble(dt)))
  if (!is.null(col_as_vector)) {
    dt <- add_dput_column(dt, col_as_vector)
  }
  dt
}


#' Replace empty values with the unknown value
#'
#' @param ft A `flat_table` object.
#' @param attributes A vector of names.
#' @param empty_values A vector of values that correspond to empty values.
#'
#' @return A `flat_table` object.
#'
#' @keywords internal
replace_empty_values_table <- function(ft, attributes = NULL, empty_values = NULL) {
  attributes <- validate_attributes(ft$attributes, attributes)
  # replace empty and NA with unknown_value (for join)
  ft$table[, attributes] <-
    apply(ft$table[, attributes, drop = FALSE], 2, function(x)
      gsub("\\s+", " ", trimws(x)))
  ft$table[, attributes] <-
    apply(ft$table[, attributes, drop = FALSE], 2, function(x)
      dplyr::na_if(x, ""))
  for (i in seq_along(empty_values)) {
    ft$table[, attributes] <-
      apply(ft$table[, attributes, drop = FALSE], 2, function(x)
        dplyr::na_if(x, empty_values[i]))
  }
  ft$table[, attributes] <-
    apply(ft$table[, attributes, drop = FALSE], 2, function(x)
      tidyr::replace_na(x, ft$unknown_value))
  ft
}
