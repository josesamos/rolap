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
add_operation <- function(op, op_name, name = NULL, details = NULL, details2 = NULL) {
  if (is_new_operation(op, op_name, name, details, details2)) {
    if (length(op$operations$order) == 0) {
      order <- 1
    } else {
      order <- max(op$operations$order) + 1
    }
    name <- vector_to_string(name)
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


#' Delete a set of operations
#'
#' @param op A `star_operation` object.
#' @param op2 A `star_operation` object.
#'
#' @return op A `star_operation` object.
#' @keywords internal
delete_operation_set <- function(op, op2) {
  for (i in seq_along(op2$operations$operation)) {
    op_name <- op2$operations[i, 'operation']
    name <- op2$operations[i, 'name']
    details <- op2$operations[i, 'details']
    details2 <- op2$operations[i, 'details2']
    op <- delete_operation(op, op_name, name, details, details2)
  }
  op
}


#' Delete an operation
#'
#' @param op A `star_operation` object.
#' @param op_name A string, operation name.
#' @param name A string, element name.
#' @param details A vector of strings, operation details.
#' @param details2 A vector of strings, operation additional details.
#'
#' @return op A `star_operation` object.
#' @keywords internal
delete_operation <- function(op, op_name, name = NULL, details = NULL, details2 = NULL) {
  name <- vector_to_string(name)
  details <- vector_to_string(details)
  details2 <- vector_to_string(details2)
  res <- op$operations[op$operations$operation == op_name,]
  res <- res[res$name == name,]
  res <- res[res$details == details,]
  res <- res[res$details2 == details2,]
  rn <- row.names(res)
  if (length(rn) > 0) {
    res <- setdiff(row.names(op$operations), rn)
    op$operations <- op$operations[res, ]
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
is_new_operation <- function(op, op_name, name = NULL, details = NULL, details2 = NULL) {
  name <- vector_to_string(name)
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
get_next_operation <- function(op, op_name, name = NULL, actual = NULL) {
  res <- op$operations[op$operations$operation == op_name, ]
  if (length(name) == 1 & op_name == 'replace_attribute_values') {
    name <- c(name, "|")
  }
  name <- vector_to_string(name)
  res <- res[startsWith(res$name, name), ]
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
    res <- NULL
  } else {
    res <- unlist(strsplit(str, "<|>", fixed = TRUE))
  }
  res
}
