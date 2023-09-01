
#' Select attributes of a flat table
#'
#' Select only the indicated attributes from the flat table.
#'
#' @param ft A `flat_table` object.
#' @param attributes A vector of names.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   select_attributes(attributes = c('Species'))
#'
#' ft <- flat_table('ft_num', ft_num) |>
#'   select_attributes(attributes = c('Year', 'WEEK', 'Week Ending Date'))
#'
#' @export
select_attributes <- function(ft, attributes) UseMethod("select_attributes")

#' @rdname select_attributes
#'
#' @export
select_attributes.flat_table <- function(ft, attributes) {
  attributes <- validate_attributes(ft$attributes, attributes)
  ft$table <- ft$table[, c(attributes, ft$measures)]
  ft$attributes <- attributes
  ft$operations <- add_operation(ft$operations, "select_attributes", attributes)
  ft
}


#' Select measures of a flat table
#'
#' Select only the indicated measures from the flat table.
#'
#' @param ft A `flat_table` object.
#' @param measures A vector of names.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   select_measures(measures = c('Sepal.Length', 'Sepal.Width'))
#'
#' @export
select_measures <- function(ft, measures) UseMethod("select_measures")

#' @rdname select_measures
#'
#' @export
select_measures.flat_table <- function(ft, measures) {
  measures <- validate_measures(ft$measures, measures)
  ft$table <- ft$table[, c(ft$attributes, measures)]
  ft$measures <- measures
  ft$operations <- add_operation(ft$operations, "select_measures", measures)
  ft
}


#' Transform to attribute
#'
#' Transform measures into attributes. We can indicate if we want all the numbers
#' in the result to have the same length and the number of decimal places.
#'
#' If a number > 1 is specified in the `width` parameter, at least that length
#' will be obtained in the result, padded with blanks on the left.
#'
#' @param ft A `flat_table` object.
#' @param measures A vector of strings, measure names.
#' @param width An integer, string length.
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
#' ft <- flat_table('iris', iris) |>
#'   transform_to_attribute(
#'     measures = "Sepal.Length",
#'     width = 3,
#'     decimal_places = 2
#'   )
#'
#' @export
transform_to_attribute <- function(ft, measures, width, decimal_places, k_sep, decimal_sep) UseMethod("transform_to_attribute")

#' @rdname transform_to_attribute
#'
#' @export
transform_to_attribute.flat_table <-
  function(ft,
           measures,
           width = 1,
           decimal_places = 0,
           k_sep = ',',
           decimal_sep = '.') {
    stopifnot("Missing measure name." = !is.null(measures))
    measures <- validate_measures(ft$measures, measures)
    for (measure in measures) {
      if (decimal_places > 0) {
        values <- suppressWarnings(as.double(ft$table[, measure][[1]]))
      } else {
        values <- suppressWarnings(as.integer(ft$table[, measure][[1]]))
      }
      if (decimal_places > 0) {
        values2 <- formatC(
          values,
          format = "f",
          big.mark = k_sep,
          decimal.mark = decimal_sep,
          digits = decimal_places,
          width = width
        )
      } else {
        values2 <- formatC(
          values,
          format = "d",
          big.mark = k_sep,
          decimal.mark = decimal_sep,
          digits = decimal_places,
          width = width
        )
      }
      if (width > 1) {
        lmax <- max(nchar(values2))
        if (lmax > width) {
          if (decimal_places > 0) {
            values2 <- formatC(
              values,
              format = "f",
              big.mark = k_sep,
              decimal.mark = decimal_sep,
              digits = decimal_places,
              width = lmax
            )
          } else {
            values2 <- formatC(
              values,
              format = "d",
              big.mark = k_sep,
              decimal.mark = decimal_sep,
              digits = decimal_places,
              width = lmax
            )
          }
        }
      }
      ft$table[, measure][[1]] <-
        gsub("NA", ft$unknown_value, values2)
      ft$measures <- setdiff(ft$measures, measure)
      ft$attributes <- c(ft$attributes, measure)
    }
    ft$table <- ft$table[, c(ft$attributes, ft$measures)]
    ft$operations <-
      add_operation(
        ft$operations,
        "transform_to_attribute",
        measures,
        c(width,
          decimal_places),
        c(k_sep,
          decimal_sep)
      )
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
#' ft <- flat_table('iris', iris) |>
#'   transform_to_attribute(measures = "Sepal.Length", decimal_places = 2) |>
#'   transform_to_measure(attributes = "Sepal.Length", decimal_sep = ".")
#'
#' @export
transform_to_measure <- function(ft, attributes, k_sep, decimal_sep) UseMethod("transform_to_measure")

#' @rdname transform_to_measure
#'
#' @export
transform_to_measure.flat_table <-
  function(ft,
           attributes,
           k_sep = NULL,
           decimal_sep = NULL) {
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
    ft$operations <-
      add_operation(ft$operations,
                    "transform_to_measure",
                    attributes,
                    k_sep,
                    decimal_sep)
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
#' ft <- flat_table('iris', iris) |>
#'   transform_to_values(attribute = 'Characteristic',
#'                       measure = 'Value')
#'
#' ft <- flat_table('iris', iris) |>
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
transform_to_values.flat_table <-
  function(ft,
           attribute = NULL,
           measure = NULL,
           id_reverse = NULL) {
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
      ft$table <-
        tibble::add_column(ft$table, !!id_reverse := 1:nrow(ft$table), .before = 1)
      l <- nchar(sprintf("%d", nrow(ft$table)))
      fo <- paste0("r%0", l, "d")
      ft$table[, id_reverse] <-
        sprintf(fo, as.integer(ft$table[, id_reverse][[1]]))
      ft$attributes <- c(id_reverse, ft$attributes)
    }
    ft$table <- ft$table[, c(ft$attributes, ft$measures)]
    interval <- (length(ft$attributes) + 1):length(colnames(ft$table))
    ft$table <-
      tidyr::gather(ft$table, attribute, measure,!!interval, na.rm = TRUE)
    names(ft$table) <- c(ft$attributes, attribute, measure)
    ft$measures <- measure
    ft$attributes <- c(ft$attributes, attribute)
    ft$table <- ft$table[, c(ft$attributes, ft$measures)]
    ft$operations <-
      add_operation(ft$operations,
                    "transform_to_values",
                    attribute,
                    measure,
                    id_reverse)
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
#' ft <- flat_table('iris', iris) |>
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
    stop(sprintf(
      "'%s' is not defined as an attribute in the flat table.",
      attribute
    ))
  }
  ft$table <-
    tidyr::spread(ft$table,
                  key = !!attribute,
                  value = !!(ft$measures))
  ft$attributes <- setdiff(ft$attributes, attribute)
  ft$measures <- setdiff(names(ft$table), ft$attributes)
  ft$table <- ft$table[, c(ft$attributes, ft$measures)]
  ft$operations <-
    add_operation(ft$operations, "transform_from_values", attribute)
  ft
}

#' Separate measures in flat tables
#'
#' Separate measures listed as list items into flat tables. Each item in the
#' list is a vector of measures that is uniquely included along with the
#' attributes in a new flat table.
#'
#' A list of flat tables is returned. It assign the names to the result list.
#'
#' @param ft A `flat_table` object.
#' @param measures A list of string vectors, groups of measure names.
#' @param names A list of string, measure group names.
#'
#' @return A list of `flat_table` objects.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' lft <- flat_table('iris', iris) |>
#'   separate_measures(
#'     measures = list(
#'       c('Petal.Length'),
#'       c('Petal.Width'),
#'       c('Sepal.Length'),
#'       c('Sepal.Width')
#'     ),
#'     names = c('PL', 'PW', 'SL', 'SW')
#'   )
#'
#' @export
separate_measures <- function(ft, measures, names) UseMethod("separate_measures")

#' @rdname separate_measures
#'
#' @export
separate_measures.flat_table <- function(ft, measures = NULL, names = NULL) {
  stopifnot("Missing measure names." = !is.null(measures))
  stopifnot("Missing measure group names." = !is.null(names))
  stopifnot("Missing measure group names." = length(measures) == length(unique(names)))
  lft <- vector("list", length = length(measures))
  names(lft) <- names
  for (i in seq_along(measures)) {
    measures[[i]] <- validate_measures(ft$measures, measures[[i]])
    lft[[i]] <-
      flat_table(name = names[i], instances = ft$table[, c(ft$attributes, measures[[i]])],
                 unknown_value = ft$unknown_value)
    lft[[i]]$pk_attributes <- ft$pk_attributes
    lft[[i]]$lookup_tables <- ft$lookup_tables
    lft[[i]]$operations <- add_operation(ft$operations, "separate_measures", measures[[i]], names[i])
  }
  lft
}


#' Replace empty values with the unknown value
#'
#' Transforms the given attributes by replacing the empty values with the unknown
#' value.
#'
#' @param ft A `flat_table` object.
#' @param attributes A vector of names.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   replace_empty_values()
#'
#' @export
replace_empty_values <- function(ft, attributes) UseMethod("replace_empty_values")

#' @rdname replace_empty_values
#'
#' @export
replace_empty_values.flat_table <- function(ft, attributes = NULL) {
  ft <- replace_empty_values_table(ft, attributes)
  ft$operations <-
    add_operation(ft$operations, "replace_empty_values", attributes)
  ft
}


#' Replace strings
#'
#' Transforms the given attributes by replacing the string values with the
#' replacement value.
#'
#' @param ft A `flat_table` object.
#' @param attributes A vector of strings, attribute names.
#' @param string A character string to replace.
#' @param replacement A replacement for matched string.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   replace_string(
#'     attributes = 'Species',
#'     string = c('set'),
#'     replacement = c('Set')
#'   )
#'
#' @export
replace_string <- function(ft, attributes, string, replacement) UseMethod("replace_string")

#' @rdname replace_string
#'
#' @export
replace_string.flat_table <- function(ft, attributes = NULL, string, replacement = NULL) {
  attributes <- validate_attributes(ft$attributes, attributes)
  if (is.null(replacement)) {
    replacement <- ""
  }
  ft$table[, attributes] <-
    lapply(
      ft$table[, attributes],
      gsub,
      pattern = string,
      replacement = replacement,
      fixed = TRUE
    )
  ft$operations <-
    add_operation(ft$operations, "replace_string", attributes, string, replacement)
  ft
}
