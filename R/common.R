#' get default unknown value
#'
#' @return A string.
#'
#' @keywords internal
get_default_unknown_value <- function() {
  "___UNKNOWN___"
}

#' check if a string is empty
#'
#' @param string A string.
#'
#' @return A boolean.
#'
#' @keywords internal
is_empty_string <- function(string) {
  res <- (is.null(string) | identical(string, character(0)))
  res
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
  if (length(measures) == 0) {
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


#' Validate fact names
#'
#' @param defined_facts A vector of strings, defined fact names.
#' @param facts A vector of strings, fact names.
#'
#' @return A vector of strings, fact names.
#'
#' @keywords internal
validate_facts <- function(defined_facts, facts) {
  stopifnot("Some fact name must be indicated." = length(facts) > 0)
  facts <- snakecase::to_snake_case(facts)
  stopifnot("There are repeated fact names." = length(facts) == length(unique(facts)))
  for (f in facts) {
    if (!(f %in% defined_facts)) {
      stop(sprintf("'%s' is not defined as fact name.", f))
    }
  }
  facts
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
  if (length(names) == 0) {
    names <- NULL
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


#' Validate names
#'
#' @param defined_names A vector of strings, defined attribute names.
#' @param names A vector of strings, new attribute names.
#' @param concept A string, treated concept.
#' @param repeated A boolean, repeated names allowed.
#'
#' @return A vector of strings, names.
#'
#' @keywords internal
validate_names <- function(defined_names, names, concept = 'name', repeated = FALSE) {
  if (is.null(names)) {
    names <- defined_names
  } else {
    if (!repeated) {
      stopifnot("There are repeated values." = length(names) == length(unique(names)))
    }
    for (name in names) {
      if (!(name %in% defined_names)) {
        stop(sprintf(
          "'%s' is not defined as %s.",
          name, concept
        ))
      }
    }
  }
  names
}
