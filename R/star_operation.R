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
#' @keywords internal
add_operation <- function(op, op_name, name, details, details2) UseMethod("add_operation")
#' @keywords internal
is_new_operation <- function(op, op_name, name, details, details2) UseMethod("is_new_operation")
#' @keywords internal
get_next_operation <- function(op, op_name, name, actual) UseMethod("get_next_operation")


#' A `star_operation` object row is added with a new operation
#'
#' @param op A `star_operation` object.
#' @param op_name A string, operation name.
#' @param name A string, element name.
#' @param details A vector of strings, operation details.
#' @param details2 A vector of strings, operation additional details.
#'
#' @return A `star_operation` object.
#' @keywords internal
add_operation.star_operation <- function(op, op_name, name = NULL, details = NULL, details2 = NULL) {
  if (is_new_operation(op, op_name, name, details, details2)) {
    if (length(op$operations$order) == 0) {
      order <- 1
    } else {
      order <- max(op$operations$order) + 1
    }
    details <- vector_to_string(details)
    details2 <- vector_to_string(details2)
    if (is.null(name)) {
      name <- ""
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
  }
  op
}

#' A `star_operation` is new?
#'
#' @param op A `star_operation` object.
#' @param op_name A string, operation name.
#' @param name A string, element name.
#' @param details A vector of strings, operation details.
#' @param details2 A vector of strings, operation additional details.
#'
#' @return A boolean.
#' @keywords internal
is_new_operation.star_operation <- function(op, op_name, name = NULL, details = NULL, details2 = NULL) {
  details <- vector_to_string(details)
  details2 <- vector_to_string(details2)
  res <- op$operations[op$operations$operation == op_name,]
  res <- res[res$name == name,]
  res <- res[res$details == details,]
  res <- res[res$details2 == details2,]
  # it is not already registered
  nrow(res) == 0
}


#' A `star_operation` object row is returned, the one following the actual given
#'
#' @param op A `star_operation` object.
#' @param op_name A string, operation name.
#' @param name A string, element name.
#' @param actual A `star_operation` object.
#'
#' @return A data frame.
#' @keywords internal
get_next_operation.star_operation <- function(op, op_name, name = NULL, actual = NULL) {
  res <- op$operations[op$operations$operation == op_name, ]
  res <- res[res$name == name, ]
  if (!is.null(actual)) {
    res <- res[res$order > actual$order, ]
  }
  if (nrow(res) > 0) {
    order <- min(res$order)
    res <- res[res$order == order, ]
  } else {
    res <- NULL
  }
  res
}


# vector to string ----------------------------------------------------

#' Transforms a vector of strings into a string.
#'
#' @param vector A vector of strings.
#'
#' @return A string.
#' @keywords internal
vector_to_string <- function(vector) {
  if (is.null(vector)) {
    res <- ""
  } else {
    res <- paste(vector, collapse = "<|>")
  }
  res
}

# string to vector ----------------------------------------------------

#' Transforms string into a vector of strings.
#'
#' @param str A string.
#'
#' @return A vector of strings.
#' @keywords internal
string_to_vector <- function(str) {
  if (str == "") {
    res <- str
  } else {
    res <- unlist(strsplit(str, "<|>", fixed = TRUE))
  }
  res
}
