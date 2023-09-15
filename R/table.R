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


#' Remove instance if all measures are na
#'
#' @param table A `tibble` object.
#' @param measures A vector of strings, measure names.
#'
#' @param table A `tibble` object.
#'
#' @keywords internal
remove_all_measures_na <- function(table, measures) {
  if (is_empty_string(measures)) {
    res <- table
  } else {
    keep <- rep(FALSE, nrow(table))
    for (m in measures) {
      keep <- keep | !is.na(table[, m][[1]])
    }
    res <- table[keep, ]
  }
  res
}


#' Replace empty values with the unknown value
#'
#' @param table A `tibble` object.
#' @param attributes A vector of names.
#' @param empty_values A vector of values that correspond to empty values.
#' @param unknown_value A string.
#'
#' @return A `tibble` object.
#'
#' @keywords internal
replace_empty_values_table <- function(table, attributes = NULL, empty_values = NULL, unknown_value) {
  # replace empty and NA with unknown_value (for join)
  table[, attributes] <-
    apply(table[, attributes, drop = FALSE], 2, function(x)
      gsub("\\s+", " ", trimws(x)))
  table[, attributes] <-
    apply(table[, attributes, drop = FALSE], 2, function(x)
      dplyr::na_if(x, ""))
  for (i in seq_along(empty_values)) {
    table[, attributes] <-
      apply(table[, attributes, drop = FALSE], 2, function(x)
        dplyr::na_if(x, empty_values[i]))
  }
  table[, attributes] <-
    apply(table[, attributes, drop = FALSE], 2, function(x)
      tidyr::replace_na(x, unknown_value))
  table
}


#' Prepare the instances table implemented by a `tibble` to join
#'
#' Transform all fields in the instances table to character type and replace
#' the `NA` values to facilitate the join operation.
#'
#' @param table A `tibble`, the instances table.
#' @param unknown_value A string, value used to replace NA values in dimensions.
#'
#' @return A `tibble`.
#' @keywords internal
prepare_to_join <- function(table, unknown_value) {
  n_row <- nrow(table)
  # all attributes of type character
  attributes <- colnames(table)
  table <- data.frame(lapply(table, as.character), stringsAsFactors = FALSE)
  colnames(table) <- attributes

  # replace NA with unknown_value (for join)
  table <- apply(table[, , drop = FALSE], 2, function(x)
    tidyr::replace_na(x, unknown_value))
  if (n_row == 1) {
    tibble::as_tibble_row(table)
  } else {
    tibble::as_tibble(table)
  }
}


#' Group table instances by keys aggregating the measures using the corresponding
#' aggregation function.
#'
#' @param table A `tibble`, the instances table.
#' @param keys A vector of strings, key names to group by.
#' @param measures A vector of strings, measures to aggregate.
#' @param agg_functions A vector of strings, aggregate functions.
#' @param nrow_agg A string, name of a new column to count the number of rows
#'   aggregated.
#'
#' @return A `tibble`.
#'
#' @importFrom rlang :=
#'
#' @keywords internal
group_by_keys <- function(table, keys, measures, agg_functions, nrow_agg) {
  if (!is.null(nrow_agg)) {
    table <- tibble::add_column(table, !!(nrow_agg) := as.integer(1))
    measures <- c(measures, nrow_agg)
    agg_functions <- c(agg_functions, "SUM")
  }

  ft_group <- dplyr::group_by_at(as.data.frame(table), dplyr::vars(tidyselect::all_of(keys)))
  agg <- list()
  for (i in seq_along(measures)) {
    if (agg_functions[i] == "MAX") {
      df <- dplyr::summarize_at(ft_group, dplyr::vars(measures[i]), max, na.rm = TRUE)
    } else if (agg_functions[i] == "MIN") {
      df <- dplyr::summarize_at(ft_group, dplyr::vars(measures[i]), min, na.rm = TRUE)
    } else {
      df <- dplyr::summarize_at(ft_group, dplyr::vars(measures[i]), sum, na.rm = TRUE)
    }
    agg <- c(agg, list(df))
  }
  dplyr::ungroup(purrr::reduce(agg, dplyr::inner_join, by = keys))
}
