#' Transform to attribute
#'
#' Transform measures into attributes. We can indicate if we want all the numbers
#' in the result to have the same length and the number of decimal places.
#'
#' If a length other than 0 is specified in the `equal_length` parameter, at
#' least that length will be obtained in the result, padded with zeros on the
#' left.
#'
#' @param ft A `flat_table` object.
#' @param measures A vector of strings, measure names.
#' @param equal_length An integer, string length.
#' @param decimal_places An integer, number of decimal places.
#' @param k_sep A character, indicates thousands separator.
#' @param decimal_sep A character, indicates decimal separator.
#'
#' @return ft A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table(iris) |>
#'   transform_to_attribute(
#'     measures = "Sepal.Length",
#'     equal_length = 3,
#'     decimal_places = 2
#'   )
#'
#' @export
transform_to_attribute <- function(ft, measures, equal_length, decimal_places, k_sep, decimal_sep) UseMethod("transform_to_attribute")

#' @rdname transform_to_attribute
#'
#' @export
transform_to_attribute.flat_table <-
  function(ft,
           measures,
           equal_length = 0,
           decimal_places = 0,
           k_sep = ',',
           decimal_sep = '.') {
    stopifnot("Missing measure name." = !is.null(measures))
    measures <- validate_measures(ft$measures, measures)
    if (decimal_places > 0) {
      format <- sprintf(".%df", decimal_places)
    } else {
      format <- "d"
    }
    for (measure in measures) {
      if (decimal_places > 0) {
        values <- suppressWarnings(as.double(ft$table[, measure][[1]]))
      } else {
        values <- suppressWarnings(as.integer(ft$table[, measure][[1]]))
      }
      if (equal_length > 0) {
        fo <- paste0("%", format)
        if (equal_length > 0) {
          s <- sprintf(fo, ft$table[, measure][[1]])
          l <- max(nchar(s))
          fo <- paste0("%0", max(l, equal_length), format)
        }
        values <- trimws(sprintf(fo, values))
      } else {
        if (decimal_places > 0) {
          values <- formatC(
            values,
            format = "f",
            big.mark = k_sep,
            decimal.mark = decimal_sep,
            digits = decimal_places
          )
        } else {
          values <- formatC(
            values,
            format = "d",
            big.mark = k_sep,
            decimal.mark = decimal_sep,
            digits = decimal_places
          )
        }
      }
      ft$table[, measure][[1]] <- gsub("NA", ft$unknown_value, values)
      ft$measures <- setdiff(ft$measures, measure)
      ft$attributes <- c(ft$attributes, measure)
    }
    ft$table <- ft$table[, c(ft$attributes, ft$measures)]
    ft
  }


#' Transform to measure
#'
#' Transform attributes into measures.
#'
#' We can indicate a thousands indicator to remove and a decimal separator to use.
#' The only decimal separators considered are "." and ",".
#'
#' @param ft A `flat_table` object.
#' @param attributes A vector of strings, attribute names.
#' @param k_sep A character, thousands separator to remove.
#' @param decimal_sep A character, new decimal separator to use, if necessary.
#'
#' @return ft A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table(iris) |>
#'   transform_to_attribute(measures = "Sepal.Length", decimal_places = 2) |>
#'   transform_to_measure(attributes = "Sepal.Length", decimal_sep = ".")
#'
#' @export
transform_to_measure <- function(ft, attributes, k_sep, decimal_sep) UseMethod("transform_to_measure")

#' @rdname transform_to_measure
#'
#' @export
transform_to_measure.flat_table <- function(ft, attributes, k_sep = NULL, decimal_sep = NULL) {
  attributes <- validate_attributes(ft$attributes, attributes)
  ft$table[, attributes] <-
    apply(ft$table[, attributes, drop = FALSE], 2, function(x)
      gsub(ft$unknown_value, "", x))
  if (!is.null(k_sep)) {
    if (k_sep == ".") {
      pattern <- "\\."
    } else {
      pattern <- k_sep
    }
    ft$table[, attributes] <-
      apply(ft$table[, attributes, drop = FALSE], 2, function(x)
        stringr::str_replace_all(x, pattern, ""))
  }
  if (!is.null(decimal_sep)) {
    if (decimal_sep == ".") {
      pattern <- ","
    } else {
      pattern <- "\\."
      decimal_sep <- ','
    }
    ft$table[, attributes] <-
      apply(ft$table[, attributes, drop = FALSE], 2, function(x)
        stringr::str_replace(x, pattern, decimal_sep))
    ft$table[, attributes] <-
      apply(ft$table[, attributes, drop = FALSE], 2, function(x)
        suppressWarnings(as.double(x)))
  } else {
    ft$table[, attributes] <-
      apply(ft$table[, attributes, drop = FALSE], 2, function(x)
        suppressWarnings(as.integer(x)))
  }
  ft$attributes <- setdiff(ft$attributes, attributes)
  ft$measures <- c(ft$measures, attributes)
  ft$table <- ft$table[, c(ft$attributes, ft$measures)]
  ft
}

#' Transform measure names into attribute values
#'
#' Transforms the measure names into values of a new attribute. The values of
#' the measures will become values of the new measure that is indicated.
#'
#' If we wanted to perform the reverse operation later using the `transform_from_values`
#' function, we would need to uniquely identify each original row. By indicating
#' a value in the `id_reverse` parameter, an identifier is added that will allow
#' us to always carry out the inverse operation.
#'
#' @param ft A `flat_table` object.
#' @param attribute A string, new attribute that will store the measures names.
#' @param measure A string, new measure that will store the measure names.
#' @param id_reverse A string, name of a new attribute that will store the row id.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table(iris) |>
#'   transform_to_values(attribute = 'Characteristic',
#'                       measure = 'Value')
#'
#' ft <- flat_table(iris) |>
#'   transform_to_values(attribute = 'Characteristic',
#'                       measure = 'Value',
#'                       id_reverse = 'id')
#'
#' @export
transform_to_values <- function(ft, attribute, measure, id_reverse) UseMethod("transform_to_values")

#' @rdname transform_to_values
#'
#' @importFrom rlang :=
#'
#' @export
transform_to_values.flat_table <- function(ft, attribute = NULL, measure = NULL, id_reverse = NULL) {
  stopifnot("Missing attribute name." = !is.null(attribute))
  stopifnot("Missing measure name." = !is.null(measure))
  stopifnot("Only one attribute name is needed." = length(attribute) == 1)
  stopifnot("Only one measure name is needed." = length(measure) == 1)
  att <- snakecase::to_snake_case(ft$attributes)
  if (snakecase::to_snake_case(attribute) %in% att) {
    stop(sprintf(
      "'%s' is already defined as an attribute in the flat table.",
      attribute
    ))
  }
  if (!is.null(id_reverse)) {
    stopifnot("Only one id name is needed." = length(id_reverse) == 1)
    if (snakecase::to_snake_case(id_reverse) %in% att) {
      stop(sprintf(
        "'%s' is already defined as an attribute in the flat table.",
        id_reverse
      ))
    }
    ft$table <- tibble::add_column(ft$table,!!id_reverse := 1:nrow(ft$table), .before = 1)
    l <- nchar(sprintf("%d", nrow(ft$table)))
    fo <- paste0("r%0", l, "d")
    ft$table[, id_reverse] <- sprintf(fo, as.integer(ft$table[, id_reverse][[1]]))
    ft$attributes <- c(id_reverse, ft$attributes)
  }
  ft$table <- ft$table[, c(ft$attributes, ft$measures)]
  interval <- (length(ft$attributes) + 1):length(colnames(ft$table))
  ft$table <- tidyr::gather(ft$table, attribute, measure, !!interval, na.rm = TRUE)
  names(ft$table) <- c(ft$attributes, attribute, measure)
  ft$measures <- measure
  ft$attributes <- c(ft$attributes, attribute)
  ft$table <- ft$table[, c(ft$attributes, ft$measures)]
  ft
}


#' Transform attribute values into measure names
#'
#' The values of an attribute will become measure names. There can only be one
#' measure that will be from where the new defined measures take the values.
#'
#' @param ft A `flat_table` object.
#' @param attribute A string, attribute that stores the measures names.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table(iris) |>
#'   transform_to_values(attribute = 'Characteristic',
#'                       measure = 'Value',
#'                       id_reverse = 'id')
#' ft <- ft |>
#'   transform_from_values(attribute = 'Characteristic')
#'
#' @export
transform_from_values <- function(ft, attribute) UseMethod("transform_from_values")

#' @rdname transform_from_values
#'
#' @export
transform_from_values.flat_table <- function(ft, attribute = NULL) {
  stopifnot("There can only be one measure defined in the flat table." = length(ft$measures) == 1)
  stopifnot("Missing attribute name." = !is.null(attribute))
  stopifnot("Only one attribute name is needed." = length(attribute) == 1)
  if (!(attribute %in% ft$attributes)) {
    stop(sprintf("'%s' is not defined as an attribute in the flat table.", attribute))
  }
  ft$table <- tidyr::spread(ft$table, key = !!attribute, value = !!(ft$measures))
  ft$attributes <- setdiff(ft$attributes, attribute)
  ft$measures <- setdiff(names(ft$table), ft$attributes)
  ft$table <- ft$table[, c(ft$attributes, ft$measures)]
  ft
}
