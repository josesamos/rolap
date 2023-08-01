
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

