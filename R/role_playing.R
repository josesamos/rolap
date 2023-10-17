#' Define a role playing dimension and its associated dimensions
#'
#' The same dimension can play several roles in relation to the facts. We can
#' define the main dimension and the dimensions that play different roles.
#'
#' As a result, all the dimensions will have the same instances and, if we deem
#' it necessary, also the same name of their attributes (except the surrogate key).
#'
#' @param db A `star_database` object.
#' @param rpd A string, dimension name (role playing dimension).
#' @param roles A vector of strings, dimension names (dimension roles).
#' @param rpd_att_names A boolean, common attribute names taken from rpd dimension.
#' @param att_names A vector of strings, common attribute names.
#'
#' @return A `star_database` object.
#'
#' @family star database definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' s <- star_schema() |>
#'   define_facts(fact_schema(
#'     name = "mrs_cause",
#'     measures = c(
#'       "Pneumonia and Influenza Deaths",
#'       "All Deaths"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When",
#'     attributes = c(
#'       "Year",
#'       "WEEK",
#'       "Week Ending Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Available",
#'     attributes = c(
#'       "Data Availability Year",
#'       "Data Availability Week",
#'       "Data Availability Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Received",
#'     attributes = c(
#'       "Reception Year",
#'       "Reception Week",
#'       "Reception Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "where",
#'     attributes = c(
#'       "REGION",
#'       "State",
#'       "City"
#'     )
#'   ))
#'
#' db <- star_database(s, ft_cause_rpd) |>
#'   role_playing_dimension(
#'     rpd = "When",
#'     roles = c("When Available", "When Received"),
#'     rpd_att_names = TRUE
#'   )
#'
#' db <- star_database(s, ft_cause_rpd) |>
#'   role_playing_dimension("When",
#'                          c("When Available", "When Received"),
#'                          att_names = c("Year", "Week", "Date"))
#'
#' @export
role_playing_dimension <- function(db, rpd, roles, rpd_att_names, att_names) UseMethod("role_playing_dimension")

#' @rdname role_playing_dimension
#'
#' @export
role_playing_dimension.star_database <- function(db, rpd, roles, rpd_att_names = FALSE, att_names = NULL) {
  rpd <- unique(snakecase::to_snake_case(rpd))
  stopifnot("Only one value can be indicated in rpd." = length(rpd) == 1)
  roles <- unique(snakecase::to_snake_case(roles))
  stopifnot("At least one role must be indicated." = length(roles) >= 1)
  stopifnot("rpd should not be included in roles." = !(rpd %in% roles))
  att_names <- unique(att_names)
  dims <- c(rpd, roles)
  # have to be dimensions
  dim_names <- names(db$dimensions)
  # they should not be previously defined rpd
  prev_rpd <- NULL
  for (n in names(db$rpd)) {
    prev_rpd <-c(prev_rpd, db$rpd[[n]])
  }
  # they must have the same structure (number of attributes)
  n_att <- 0
  for (d in dims) {
    if (!(d %in% dim_names)) {
      stop(sprintf("'%s' is not a dimension name.", d))
    }
    if (d %in% prev_rpd) {
      stop(sprintf("'%s' is included in a previous rpd definition.", d))
    }
    if (n_att == 0) {
      n_att <- ncol(db$dimensions[[d]]$table)
    } else {
      stopifnot("rpd and roles must have the same number of attributes." = n_att == ncol(db$dimensions[[d]]$table))
    }
  }
  if (!is.null(att_names)) {
    stopifnot("The number of attributes does not match those of att_names." = n_att == length(att_names) + 1)
  }
  # they meet all the requirements

  # annotate rpd
  db$rpd[[rpd]] <- dims

  # rename attributes
  if (rpd_att_names == TRUE) {
    att_names <- names(db$dimensions[[rpd]]$table)[-1]
  }
  if (!is.null(att_names)) {
    for (d in dims) {
      names(db$dimensions[[d]]$table) <-
        c(names(db$dimensions[[d]]$table)[1], att_names)
    }
  } else {
    att_names <- ""
  }

  db <- share_dimensions(db, dims)
  db$operations[[1]] <- add_operation(db$operations[[1]], "role_playing_dimension", rpd, roles, att_names)
  db
}


# Internal ---------------------------------------------------------------------

#' Get rpd dimensions of a dimension
#'
#' @param db A `star_database` object.
#' @param name A string, dimension name.
#'
#' @return A vector of dimension names.
#'
#' @keywords internal
get_rpd_dimensions <- function(db, name) {
  res <- name
  for (i in seq_along(db$rpd)) {
    if (name %in% db$rpd[[i]]) {
      res <- db$rpd[[i]]
      break
    }
  }
  res
}


#' From a vector of dimensions, leave only one of each rpd.
#'
#' @param db A `star_database` object.
#' @param names A vector of strings, dimension names.
#'
#' @return A vector of dimension names.
#'
#' @keywords internal
simplify_rpd_dimensions <- function(db, names) {
  res <- NULL
  for (n in names) {
    included <- FALSE
    for (i in seq_along(db$rpd)) {
      if (n %in% db$rpd[[i]]) {
        res <- c(res, db$rpd[[i]][1])
        included <- TRUE
        break
      }
    }
    if (!included) {
      res <- c(res, n)
    }
  }
  unique(res)
}



#' From rpd dimensions, leave only contained in vector of names.
#'
#' @param db A `star_database` object.
#' @param names A vector of strings, dimension names.
#'
#' @return A list of vectors of dimension names.
#'
#' @keywords internal
filter_rpd_dimensions <- function(db, names) {
  rpd <- list()
  rpd_names <- NULL
  for (i in seq_along(db$rpd)) {
    db$rpd[[i]] <- intersect(db$rpd[[i]], names)
    if (length(db$rpd[[i]]) > 1) {
      rpd <- c(rpd, list(db$rpd[[i]]))
      rpd_names <- c(rpd_names, db$rpd[[i]][1])
    }
  }
  names(rpd) <- rpd_names
  rpd
}


#' Share the given dimensions in the database
#'
#' @param db `star_database` object.
#' @param dims Vector of dimension names.
#'
#' @return A `star_database` object.
#' @keywords internal
share_dimensions <- function(db, dims) {
  # merge dimensions
  to_conform <- vector("list", length = length(dims))
  for (i in seq_along(dims)) {
    to_conform[i] <- db$dimensions[dims[i]]
    if (i > 1) {
      # to be able to conform they must have the same columns.
      names(to_conform[[i]]$table) <- names(to_conform[[1]]$table)
    }
  }
  cd <- conform_dimensions(to_conform)

  for (i in seq_along(dims)) {
    surrogate_key <- db$dimensions[[dims[i]]]$surrogate_key
    all_att <- names(db$dimensions[[dims[i]]]$table)
    attributes <- all_att[all_att != surrogate_key]

    # join facts to original dimension
    for (f in seq_along(db$facts)) {
      if (dims[i] %in% db$facts[[f]]$dim_int_names) {
        db$facts[[f]]$table <-
          dplyr::select(
            dplyr::inner_join(db$facts[[f]]$table,
                              db$dimensions[[dims[i]]]$table,
                              by = surrogate_key),-tidyselect::all_of(surrogate_key)
          )
      }
    }

    # change dimension table keeping attribute names
    db$dimensions[[dims[i]]]$table <- cd$table
    names(db$dimensions[[dims[i]]]$table) <- all_att

    # join new dimension to facts
    for (f in seq_along(db$facts)) {
      if (dims[i] %in% db$facts[[f]]$dim_int_names) {
        db$facts[[f]]$table <-
          dplyr::select(
            dplyr::inner_join(db$facts[[f]]$table,
                              db$dimensions[[dims[i]]]$table,
                              by = attributes),-tidyselect::all_of(attributes)
          )
      }
    }
  }
  # reorder attributes in facts
  for (f in seq_along(db$facts)) {
    measures <-
      setdiff(names(db$facts[[f]]$table),
              db$facts[[f]]$surrogate_keys)
    db$facts[[f]]$table <-
      dplyr::select(db$facts[[f]]$table, tidyselect::all_of(c(db$facts[[f]]$surrogate_keys, measures)))
  }
  db
}
