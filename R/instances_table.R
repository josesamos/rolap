# prepare_instances_to_join ----------------------------------------------------

#' Transform the instances table implemented by a `tibble` to join
#'
#' Transform all fields in the instances table to character type and replace
#' the `NA` values for a specific value.
#'
#' @param instances A `tibble`, the instances table.
#' @param attributes A vector of attribute names.
#'
#' @return A `tibble`.
#' @keywords internal
prepare_instances_to_join <- function(instances, attributes = NULL) {
  n_row <- nrow(instances)
  # all attributes of type character
  if (is.null(attributes)) {
    attributes <- colnames(instances)
  } else {
    instances <- instances[, attributes]
  }
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
