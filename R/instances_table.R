# prepare_instances_to_join ----------------------------------------------------

#' Transform the instances table implemented by a `tibble` to join
#'
#' Transform all fields in the instances table to character type and replace
#' the `NA` values for a specific value.
#'
#' @param instances A `tibble`, the instances table.
#'
#' @return A `tibble`.
#' @keywords internal
prepare_instances_to_join <- function(instances) {
  n_row <- nrow(instances)
  # all attributes of type character
  attributes <- colnames(instances)
  instances <- data.frame(lapply(instances, as.character), stringsAsFactors = FALSE)
  colnames(instances) <- attributes

  # replace NA with unknown (for join)
  instances <- apply(instances[, , drop = FALSE], 2, function(x)
    tidyr::replace_na(x, "___UNKNOWN___"))
  if (n_row == 1) {
    tibble::as_tibble_row(instances)
  } else {
    tibble::as_tibble(instances)
  }
}

# add_surrogate_key ------------------------------------------------------------

#' Add the surrogate key from a dimension table to the instances table.
#'
#' @param instances A `tibble`, the instances table.
#' @param dimension A `tibble`, the dimension table.
#' @param surrogate_key A string, the surrogate key name.
#'
#' @return A `tibble`.
#' @keywords internal
add_surrogate_key <- function(instances, dimension, surrogate_key) {
  attributes <- colnames(dimension)
  attributes <- attributes[attributes != surrogate_key]
  dplyr::inner_join(instances, dimension, by = attributes)
}

# group_by_keys ----------------------------------------------------------------

#' Group instances by keys aggregating the measures using the corresponding
#' aggregation function.
#'
#' @param instances A `tibble`, the instances table.
#' @param keys A vector of strings, key names to group by.
#' @param measures A vector of strings, measures to aggregate.
#' @param agg_functions A vector of strings, aggregate functions.
#' @param nrow_agg A string, name of a new column to count the number of rows
#'   aggregated.
#'
#' @return A `tibble`.
#' @keywords internal
group_by_keys <- function(instances, keys, measures, agg_functions, nrow_agg) {
  # add the new measure to count the number of rows aggregated
  if (is.null(nrow_agg)) {
    nrow_agg <- 'nrow_agg'
  }
  instances <- tibble::add_column(instances, !!(nrow_agg) := as.integer(1))
  measures <- c(measures, nrow_agg)
  agg_functions <- c(agg_functions, "SUM")

  ft_group <- dplyr::group_by_at(as.data.frame(instances), dplyr::vars(one_of(keys)))
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
  purrr::reduce(agg, dplyr::inner_join, by = keys)
}
