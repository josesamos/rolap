
# vector to string ----------------------------------------------------

#' Transforms a vector of strings into a string.
#'
#' @param vector A vector of strings.
#'
#' @return A string.
#' @keywords internal
vector_to_string <- function(vector) {
  paste(vector, collapse = "<|>")
}

# string to vector ----------------------------------------------------

#' Transforms string into a vector of strings.
#'
#' @param str A string.
#'
#' @return A vector of strings.
#' @keywords internal
string_to_vector <- function(str) {
  unlist(strsplit(str, "<|>", fixed = TRUE))
}

# add_operation ----------------------------------------------------

#' A data frame row is added with a new operation
#'
#'
#' @param op A data frame of operations.
#' @param op_name A string, operation name.
#' @param name A string, element name.
#' @param details A vector of strings, operation details.
#' @param details2 A vector of strings, operation additional details.
#'
#' @return A data frame.
#' @keywords internal
add_operation <- function(op = NULL, op_name, name = "", details = NULL, details2 = NULL) {
  if (is.null(op)) {
    op <- data.frame(
      operation = character(),
      name = character(),
      details = character(),
      details2 = character(),
      order = integer(),
      stringsAsFactors = FALSE
    )
  }
  if (length(op$order) == 0) {
    order <- 1
  } else {
    order <- max(op$order) + 1
  }
  if (is.null(details)) {
    details <- ""
  } else {
    details <- vector_to_string(details)
  }
  if (is.null(details2)) {
    details2 <- ""
  } else {
    details2 <- vector_to_string(details2)
  }

  op <-
    rbind(
      op,
      data.frame(
        operation = op_name,
        name = name,
        details = details,
        details2 = details2,
        order = order,
        stringsAsFactors = FALSE
      )
    )
}

# as_tibble_list_common ----------------------------------------------------

#' Generate a list of tibbles with fact and dimension tables
#'
#' @param dimensions A list of dimension tables.
#' @param facts A list of fact tables.
#'
#' @return A list of `tibble` objects.
#' @keywords internal
as_tibble_list_common <- function(dimensions, facts) {
  l <- NULL
  lnames <- NULL
  for (d in names(dimensions)) {
    l <- c(l, list(dimensions[[d]]$table))
    lnames <- c(lnames, d)
  }
  for (f in names(facts)) {
    l <- c(l, list(facts[[f]]$table))
    lnames <- c(lnames, f)
  }
  names(l) <- lnames
  l
}


# as_dm_class_common ----------------------------------------------------

#' Generate a list of tibbles with fact and dimension tables
#'
#' @param dimensions A list of dimension tables.
#' @param facts A list of fact tables.
#'
#' @return  A `dm` object.
#' @param pk_facts A boolean, include primary key in fact tables.
#'
#' @importFrom rlang :=
#'
#' @keywords internal
as_dm_class_common <- function(dimensions, facts, pk_facts) {
  c <- dm::dm()
  for (d in names(dimensions)) {
    c <- c |>
      dm::dm(!!d := dimensions[[d]]$table) |>
      dm::dm_add_pk(!!d, !!dimensions[[d]]$surrogate_key) |>
      dm::dm_set_colors(darkgreen = !!d)
  }
  for (f in names(facts)) {
    c <- c |>
      dm::dm(!!f := facts[[f]]$table) |>
      dm::dm_set_colors(darkblue = !!f)
    for (s in facts[[f]]$surrogate_keys) {
      t <- gsub("_key", "", s)
      c <- c |>
        dm::dm_add_fk(!!f, !!s, !!t)
    }
    if (pk_facts) {
      c <- c |>
        dm::dm_add_pk(!!f, !!facts[[f]]$surrogate_keys)
    }
  }
  c
}

