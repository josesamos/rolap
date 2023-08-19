#' `star_operation` S3 class
#'
#' A `star_operation` object is created.
#'
#' A `star_operation` object is part of a `star_schema` object, defines
#' operations of the star schema.
#'
#' @return A `star_operation` object.
#'
#' @keywords internal
star_operation <- function() {
  operations <- data.frame(
    operation = character(),
    name = character(),
    details = character(),
    details2 = character(),
    order = integer(),
    stringsAsFactors = FALSE
  )

  structure(list(operations = operations), class = "star_operation")
}

# generic
add_operation <- function(op, op_name, name, details, details2) UseMethod("add_operation")


# add_operation ----------------------------------------------------

#' A data frame row is added with a new operation
#'
#'
#' @param op A `star_operation` object.
#' @param op_name A string, operation name.
#' @param name A string, element name.
#' @param details A vector of strings, operation details.
#' @param details2 A vector of strings, operation additional details.
#'
#' @return A data frame.
#' @keywords internal
add_operation.star_operation <- function(op, op_name, name = "", details = NULL, details2 = NULL) {
  if (length(op$operations$order) == 0) {
    order <- 1
  } else {
    order <- max(op$operations$order) + 1
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

  op$operations <-
    rbind(
      op$operations,
      data.frame(
        operation = op_name,
        name = name,
        details = details,
        details2 = details2,
        order = order,
        stringsAsFactors = FALSE
      )
    )
  op
}


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
