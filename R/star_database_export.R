
#' Generate a list of tibbles with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a list of tibbles, as this function does.
#'
#'
#' @param db A `star_database` object.
#'
#' @return A list of `tibble`
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' tl1 <- db1 |>
#'   as_tibble_list()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", list(db1, db2))
#' tl <- ct |>
#'   as_tibble_list()
#'
#' @export
as_tibble_list <- function(db) UseMethod("as_tibble_list")

#' @rdname as_tibble_list
#'
#' @export
as_tibble_list.star_database <- function(db) {
  l <- NULL
  lnames <- NULL
  for (d in names(db$dimensions)) {
    l <- c(l, list(db$dimensions[[d]]$table))
    lnames <- c(lnames, d)
  }
  for (f in names(db$facts)) {
    l <- c(l, list(db$facts[[f]]$table))
    lnames <- c(lnames, f)
  }
  names(l) <- lnames
  l
}


#' Generate a `dm` class with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a `dm` class, as this function does, in this way it can be
#' saved directly in a DBMS.
#'
#'
#' @param db A `star_database` object.
#' @param pk_facts A boolean, include primary key in fact tables.
#'
#' @return A `dm` object.
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' dm1 <- db1 |>
#'   as_dm_class()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", list(db1, db2))
#' dm <- ct |>
#'   as_dm_class()
#'
#' @export
as_dm_class <- function(db, pk_facts) UseMethod("as_dm_class")

#' @rdname as_dm_class
#'
#' @export
as_dm_class.star_database <- function(db, pk_facts = TRUE) {
  c <- dm::dm()
  for (d in names(db$dimensions)) {
    c <- c |>
      dm::dm(!!d := db$dimensions[[d]]$table) |>
      dm::dm_add_pk(!!d, !!db$dimensions[[d]]$surrogate_key) |>
      dm::dm_set_colors(darkgreen = !!d)
  }
  for (f in names(db$facts)) {
    c <- c |>
      dm::dm(!!f := db$facts[[f]]$table) |>
      dm::dm_set_colors(darkblue = !!f)
    for (s in db$facts[[f]]$surrogate_keys) {
      t <- gsub("_key", "", s)
      c <- c |>
        dm::dm_add_fk(!!f, !!s, !!t)
    }
    if (pk_facts) {
      c <- c |>
        dm::dm_add_pk(!!f, !!db$facts[[f]]$surrogate_keys)
    }
  }
  c
}



#' Generate a list of tibbles of flat tables
#'
#' Allows you to transform a star database into a flat table. If we have a
#' constellaton, it returns a list of flat tables.
#'
#'
#' @param db A `star_database` object.
#'
#' @return A list of `tibble`
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' tl1 <- db1 |>
#'   as_single_tibble_list()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", list(db1, db2))
#' tl <- ct |>
#'   as_single_tibble_list()
#'
#' @export
as_single_tibble_list <- function(db) UseMethod("as_single_tibble_list")

#' @rdname as_single_tibble_list
#'
#' @export
as_single_tibble_list.star_database <- function(db) {
  res <- vector("list", length = length(db$facts))
  names(res) <- names(db$facts)
  for (f in names(db$facts)) {
    res[[f]] <- db$facts[[f]]$table
    for (d in db$facts[[f]]$dim_int_names) {
      key <- db$dimensions[[d]]$surrogate_key
      res[[f]] <-
        dplyr::inner_join(res[[f]], db$dimensions[[d]]$table, by = key, suffix = c("", paste0('_',d)))
    }
    measures <- names(db$facts[[f]]$agg)
    attributes <- setdiff(names(res[[f]]), c(measures, db$facts[[f]]$surrogate_keys))
    res[[f]] <- res[[f]][, c(attributes, measures)]
  }
  res
}

