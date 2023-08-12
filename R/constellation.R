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
#' @family star database and constellation definition functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
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
  stopifnot("Missing constellation name." = !is.null(name))
  stopifnot("A constellation must be made up of more than one star." = length(stars) > 1)
  fct_names <- c()
  dim_names <- c()
  facts <- vector("list", length = length(stars))
  rpd <- list()
  for (s in seq_along(stars)) {
    stopifnot("A constellation must be made up of stars." = methods::is(stars[[s]], "star_database"))
    fct_names <- c(fct_names, names(stars[[s]]$instance$facts))
    dim_names <- c(dim_names, names(stars[[s]]$instance$dimensions))
    facts[s] <- stars[[s]]$instance$facts
    rpd <- c(rpd, stars[[s]]$instance$rpd)
  }

  fct_names <- unique(fct_names)
  stopifnot("The stars of a constellation must have different names." = length(stars) == length(fct_names))
  names(facts) <- fct_names

  # frequency of dimensions
  dim_freq <- table(dim_names)
  dimensions = vector("list", length = length(dim_freq))
  names(dimensions) <- names(dim_freq)

  for (dn in names(dim_freq[dim_freq == 1])) {
    # finding the dimension in the component stars
    for (s in seq_along(stars)) {
      if (dn %in% names(stars[[s]]$instance$dimensions)) {
        dimensions[dn] <- stars[[s]]$instance$dimensions[dn]
        break # dim_freq == 1 and found
      }
    }
  }
  for (dn in names(dim_freq[dim_freq > 1])) {
    to_conform <- vector("list", length = dim_freq[dn])
    i <- 1
    surrogate_key <- NULL
    attributes <- NULL
    for (s in seq_along(stars)) {
      if (dn %in% names(stars[[s]]$instance$dimensions)) {
        dim <- stars[[s]]$instance$dimensions[dn]
        to_conform[i] <- dim
        i <- i + 1
        if (is.null(surrogate_key)) {
          surrogate_key <- dim[[1]]$surrogate_key
          attributes <-
            names(dim[[1]]$table)[names(dim[[1]]$table) != surrogate_key]
        }
        # join facts to original dimension
        facts[[names(stars[[s]]$instance$facts)]]$table <-
          dplyr::select(
            dplyr::inner_join(facts[[names(stars[[s]]$instance$facts)]]$table,
                              dim[[1]]$table,
                              by = surrogate_key),-tidyselect::all_of(surrogate_key)
          )
      }
      if (i > dim_freq[dn]) {
        break
      }
    }

    dimensions[[dn]] <- conform_dimensions(to_conform)

    # join new dimension to facts
    for (f in seq_along(facts)) {
      i <- 1
      if (dn %in% facts[[f]]$dim_int_names) {
        facts[[f]]$table <-
          dplyr::select(
            dplyr::inner_join(facts[[f]]$table,
                              dimensions[[dn]]$table,
                              by = attributes), -tidyselect::all_of(attributes)
          )
        i <- i + 1
      }
      if (i > dim_freq[dn]) {
        break
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

  rpd <- unify_rpd(rpd)

  c <- structure(list(
    name = name,
    facts = facts,
    dimensions = dimensions,
    rpd = rpd
  ), class = "constellation")
  rpd_in_constellation(c)
}

#' Unify lists of dimension names if there are any in common
#'
#' @param rpd A list of strings (dimension names).
#'
#' @keywords internal
unify_rpd <- function(rpd) {
  dims <- unlist(rpd, use.names=FALSE)
  if (length(dims) != length(unique(dims))) {
    rpd_res <- c(rpd[1])
    for (i in 2:length(rpd)) {
      included <- FALSE
      for (j in 1:length(rpd_res)) {
        if (length(intersect(rpd[[i]], rpd_res[[j]])) > 0) {
          rpd_res[[j]] <- unique(c(rpd_res[[j]], rpd[[i]]))
          included <- TRUE
        }
      }
      if (!included) {
        rpd_res <- c(rpd_res, rpd[i])
      }
    }
    unify_rpd(rpd_res)
  } else {
    rpd
  }
}


#' Transform role playing dimensions in constellation
#'
#' @param db A `constellation` object.
#'
#' @return A `constellation` object.
#'
rpd_in_constellation <- function(db) UseMethod("rpd_in_constellation")
#' @rdname rpd_in_constellation
#'
#' @keywords internal
rpd_in_constellation.constellation <- function(db) {
  # frequency of dimensions and shared dimensions
  dim_names <- c()
  for (i in seq_along(db$facts)) {
    dim_names <- c(dim_names, db$facts[[i]]$dim_int_names)
  }
  dim_freq <- table(dim_names)
  shared_dim <- names(dim_freq)[dim_freq > 1]

  #rpd dimensions
  rpd_dim <- unlist(db$rpd, use.names=FALSE)

  rpd_shared_dim <- intersect(shared_dim, rpd_dim)
  if (length(rpd_shared_dim) > 0) {
    # some rpd is a shared dimension
    for (i in seq_along(db$rpd)) {
      dims <- db$rpd[[i]]
      if (length(intersect(rpd_shared_dim, dims)) > 0) {
        db <- share_dimensions(db, dims)
      }
    }
  }
  db
}



#' @rdname as_tibble_list
#'
#' @export
as_tibble_list.constellation <- function(db) {
  as_tibble_list_common(db$dimensions, db$facts)
}

#' @rdname as_dm_class
#'
#' @export
as_dm_class.constellation <- function(db, pk_facts = TRUE) {
  as_dm_class_common(db$dimensions, db$facts, pk_facts)
}

