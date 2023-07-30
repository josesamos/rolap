#' `constellation` S3 class
#'
#' Creates a `constellation` object from a list of `star_database` objects. All
#' dimensions with the same name in the star schemas have to be conformable
#' (share the same structure, even though they have different instances).
#'
#' @param name A string.
#' @param stars A list of `star_database` objects.
#'
#' @return A `constellation` object.
#'
#' @family star database definition functions
#' @seealso \code{\link{star_schema}}, \code{\link{mrs_cause_schema}}, \code{\link{mrs_age_schema}}, \code{\link{ft_num}}, \code{\link{ft_age}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#' ct <- constellation("MRS", list(db1, db2))
#'
#' @export
constellation <- function(name = NULL, stars = NULL) {
  stopifnot(!is.null(name))
  stopifnot(length(stars) > 1)
  fct_names <- c()
  dim_names <- c()
  facts = vector("list", length = length(stars))
  for (s in seq_along(stars)) {
    stopifnot("star_database" %in% class(stars[[s]]))
    fct_names <- c(fct_names, names(stars[[s]]$instance$facts))
    dim_names <- c(dim_names, names(stars[[s]]$instance$dimensions))
    facts[s] <- stars[[s]]$instance$facts
  }
  fct_names <- unique(fct_names)
  stopifnot(length(stars) == length(fct_names))
  names(facts) <- fct_names

  # frequency of dimensions
  dim_freq <- table(dim_names)
  dimensions = vector("list", length = length(dim_freq))
  names(dimensions) <- names(dim_freq)

  # generate dimensions
  for (dn in names(dim_freq)) {
    if (dim_freq[dn] == 1) {
      for (s in seq_along(stars)) {
        for (d in seq_along(stars[[s]]$instance$dimensions)) {
          dim <- stars[[s]]$instance$dimensions[d]
          if (names(dim) == dn) {
            dimensions[dn] <- dim
            break
          }
        }
        if (names(dim) == dn) {
          break
        }
      }
    } else {
      to_conform <- vector("list", length = dim_freq[dn])
      i <- 1
      surrogate_key <- NULL
      attributes <- NULL
      for (s in seq_along(stars)) {
        for (d in seq_along(stars[[s]]$instance$dimensions)) {
          dim <- stars[[s]]$instance$dimensions[d]
          if (names(dim) == dn) {
            to_conform[i] <- dim
            i <- i + 1
            if (is.null(surrogate_key)) {
              surrogate_key <- dim[[1]]$surrogate_key
              attributes <-
                names(dim[[1]]$table)[names(dim[[1]]$table) != surrogate_key]
            }
            # join facts to original dimension
            facts[names(stars[[s]]$instance$facts)][[1]]$table <-
              dplyr::select(
                dplyr::inner_join(facts[names(stars[[s]]$instance$facts)][[1]]$table,
                                  dim[[1]]$table,
                                  by = surrogate_key),-tidyselect::all_of(surrogate_key)
              )
            break
          }
        }
        if (i > dim_freq[dn]) {
          break
        }
      }
      dimensions[[dn]] <- conform_dimensions(to_conform)
      # join new dimension to facts
      for (s in seq_along(stars)) {
        if (dn %in% names(stars[[s]]$instance$dimensions)) {
          facts[names(stars[[s]]$instance$facts)][[1]]$table <-
            dplyr::select(
              dplyr::inner_join(facts[names(stars[[s]]$instance$facts)][[1]]$table,
                                dimensions[dn][[1]]$table,
                                by = attributes),-tidyselect::all_of(attributes)
            )
        }
      }
    }
  }
  # reorder attributes in facts
  for (f in seq_along(facts)) {
    measures <-
      setdiff(names(facts[[f]]$table), facts[[f]]$surrogate_keys)
    facts[[f]]$table <-
      dplyr::select(facts[[f]]$table, tidyselect::all_of(c(facts[[f]]$surrogate_keys, measures)))
  }
  structure(list(
    name = name,
    facts = facts,
    dimensions = dimensions
  ), class = "constellation")
}

# as_tibble_list.constellation -------------------------------------------

#' Generate a list of tibbles with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a list of tibbles, as this function does.
#'
#'
#' @param db A `constellation` object.
#'
#' @return A list of `tibble` objects.
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#' ct <- constellation("MRS", list(db1, db2))
#' tl <- ct |>
#'   as_tibble_list()
#'
#' @export
as_tibble_list.constellation <- function(db) {
  as_tibble_list_common(db$dimensions, db$facts)
}

# as_dm_class.constellation -------------------------------------------

#' Generate a `dm` class with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a `dm` class, as this function does, in this way it can be
#' saved directly in a DBMS.
#'
#'
#' @param db A `constellation` object.
#'
#' @return A `dm` object.
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#' ct <- constellation("MRS", list(db1, db2))
#'
#' dm <- ct |>
#'   as_dm_class()
#'
#' @export
as_dm_class.constellation <- function(db) {
  as_dm_class_common(db$dimensions, db$facts)
}

