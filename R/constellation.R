#' Create constellation
#'
#' Creates a constellation from a list of `star_database` objects. A constellation
#' is also represented by a `star_database` object. All dimensions with the same
#' name in the star schemas have to be conformable (share the same structure, even
#' though they have different instances).
#'
#' @param name A string.
#' @param ... `star_database` objects.
#'
#' @return A `star_database` object.
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
#' ct1 <- constellation("MRS", db1, db2)
#'
#'
#' db3 <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
#'   role_playing_dimension(
#'     rpd = "When",
#'     roles = c("When Available", "When Received")
#'   )
#'
#' db4 <- star_database(mrs_age_schema_rpd, ft_age_rpd) |>
#'   role_playing_dimension(
#'     rpd = "When Arrived",
#'     roles = c("When Available")
#'   )
#' ct2 <- constellation("MRS", db3, db4)
#'
#' @export
constellation <- function(name = NULL, ...) {
  stars <- list(...)
  stopifnot("Missing constellation name." = !is.null(name))
  stopifnot("A constellation must be made up of more than one star." = length(stars) > 1)
  fct_names <- c()
  dim_names <- c()
  num_stars <- 0
  for (s in seq_along(stars)) {
    stopifnot("A constellation must be made up of stars." = methods::is(stars[[s]], "star_database"))
    num_stars <- num_stars + length(stars[[s]]$facts)
    fct_names <- c(fct_names, names(stars[[s]]$facts))
    dim_names <- c(dim_names, names(stars[[s]]$dimensions))
  }
  fct_names <- unique(fct_names)
  stopifnot("The stars of a constellation must have different names." = num_stars == length(fct_names))
  # frequency of dimensions
  dim_freq <- table(dim_names)

  stars <- share_dimension_instance_operations(stars, dim_freq)

  # prepare structures
  facts <- vector("list", length = num_stars)
  operations <- vector("list", length = num_stars)
  lookup_tables <- vector("list", length = num_stars)
  schemas <- vector("list", length = num_stars)
  dimensions = vector("list", length = length(dim_freq))
  rpd <- list()
  geo <- list()
  scd <- list()
  names(facts) <- fct_names
  names(operations) <- fct_names
  names(lookup_tables) <- fct_names
  names(schemas) <- fct_names
  names(dimensions) <- names(dim_freq)

  # facts, operations and rpd
  for (s in seq_along(stars)) {
    rpd <- c(rpd, stars[[s]]$rpd)
    for (f in seq_along(stars[[s]]$facts)) {
      sfn <- names(stars[[s]]$facts[f])
      operations[sfn] <- stars[[s]]$operations[f]
      lookup_tables[sfn] <- stars[[s]]$lookup_tables[f]
      schemas[sfn] <- stars[[s]]$schemas[f]
      facts[sfn] <- stars[[s]]$facts[f]
    }
  }

  # dimensions
  for (dn in names(dim_freq[dim_freq == 1])) {
    # finding the dimension in the component stars
    for (s in seq_along(stars)) {
      if (dn %in% names(stars[[s]]$dimensions)) {
        dimensions[dn] <- stars[[s]]$dimensions[dn]
        if (!is.null(stars[[s]]$geo[[dn]])) {
          geo[[dn]] <- stars[[s]]$geo[[dn]]
        }
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
      if (!is.null(stars[[s]]$geo[[dn]])) {
        if (is.null(geo[[dn]])) {
          geo[[dn]] <- stars[[s]]$geo[[dn]]
        } else {
          geo[[dn]] <- integrate_geo_dimensions(geo[[dn]], stars[[s]]$geo[[dn]])
        }
      }
      for (f in seq_along(stars[[s]]$facts)) {
        if (dn %in% stars[[s]]$facts[[f]]$dim_int_names) {
          dim <- stars[[s]]$dimensions[dn]
          to_conform[i] <- dim
          i <- i + 1
          if (is.null(surrogate_key)) {
            surrogate_key <- dim[[1]]$surrogate_key
            attributes <-
              names(dim[[1]]$table)[names(dim[[1]]$table) != surrogate_key]
          }
          # join facts to original dimension
          facts[[names(stars[[s]]$facts[f])]]$table <-
            dplyr::select(
              dplyr::inner_join(facts[[names(stars[[s]]$facts[f])]]$table,
                                dim[[1]]$table,
                                by = surrogate_key),-tidyselect::all_of(surrogate_key)
            )
        }
      }
    }

    dimensions[[dn]] <- conform_dimensions(to_conform)

    # join new dimension to facts
    for (f in seq_along(facts)) {
      if (dn %in% facts[[f]]$dim_int_names) {
        facts[[f]]$table <-
          dplyr::select(
            dplyr::inner_join(facts[[f]]$table,
                              dimensions[[dn]]$table,
                              by = attributes), -tidyselect::all_of(attributes)
          )
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
    operations = operations,
    lookup_tables = lookup_tables,
    schemas = schemas,
    refresh = list(),
    deploy = list(),
    facts = facts,
    dimensions = dimensions,
    rpd = rpd,
    geo = geo,
    scd = scd
  ), class = "star_database")
  c <- rpd_in_constellation(c)
  purge_dimension_instances_star_database(c)
}

#' Share dimension instance operations between all `star_database` objects
#'
#' @param stars A list of `star_database` objects.
#' @param dim_freq Dimension frequency table.
#'
#' @return A list of `star_database` objects.
#'
#' @keywords internal
share_dimension_instance_operations <- function(stars, dim_freq) {
  op_name  <- 'replace_attribute_values'
  for (dn in names(dim_freq[dim_freq > 1])) {
    op <- get_all_dimension_operations(op_name = op_name, name = dn, stars)
    if (nrow(op$operations) > 0) {
      stars <- delete_all_operations_found(stars, op)
      for (s in seq_along(stars)) {
        for (f in seq_along(stars[[s]]$facts)) {
          if (dn %in% stars[[s]]$facts[[f]]$dim_int_names) {
            next_op <- get_next_operation(op, op_name = op_name, name = dn, actual = NULL)
            while (!is.null(next_op)) {
              name <- string_to_vector(next_op$name)
              details <- string_to_vector(next_op$details)
              details2 <- string_to_vector(next_op$details2)
              # c(name, "|", attributes)
              stars[[s]] <-
                replace_attribute_values(
                  stars[[s]],
                  name = name[1],
                  attributes = name[-c(1, 2)],
                  old = details,
                  new = details2
                )
              next_op <- get_next_operation(op, op_name = op_name, name = dn, actual = next_op)
            }
            # all operations for a dimension have been carried out
            stars[[s]] <- group_dimension_instances(stars[[s]], dn)
          }
        }
      }
    }
  }
  stars
}


#' Gets the operations performed on a dimension in all `star_database` objects
#'
#' @param op_name A string, operation name.
#' @param name A string, element name.
#' @param stars A list of `star_database` objects.
#'
#' @return A `star_operations` object.
#'
#' @keywords internal
get_all_dimension_operations <- function(op_name, name, stars) {
  op <- star_operation()
  for (s in seq_along(stars)) {
    for (f in seq_along(stars[[s]]$facts)) {
      if (name %in% stars[[s]]$facts[[f]]$dim_int_names) {
        next_op <-
          get_next_operation(
            stars[[s]]$operations[[f]],
            op_name = op_name,
            name = name,
            actual = NULL
          )
        while (!is.null(next_op)) {
          op <-
            add_operation(
              op,
              op_name,
              name = next_op$name,
              details = next_op$details,
              details2 = next_op$details2
            )
          next_op <-
            get_next_operation(
              stars[[s]]$operations[[f]],
              op_name = op_name,
              name = name,
              actual = next_op
            )
        }
      }
    }
  }
  op
}


#' Delete in stars all operations found
#'
#' @param stars A list of `star_database` objects.
#' @param op A `star_operations` object.
#'
#' @return A list of `star_database` objects.
#'
#' @keywords internal
delete_all_operations_found <- function(stars, op) {
  for (s in seq_along(stars)) {
    for (f in seq_along(stars[[s]]$facts)) {
      stars[[s]]$operations[[f]] <-
        delete_operation_set(stars[[s]]$operations[[f]], op)
    }
  }
  stars
}


#' Unify lists of dimension names if there are any in common
#'
#' @param rpd A list of strings (dimension names).
#'
#' @return A list of strings (dimension names).
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
#' @keywords internal
rpd_in_constellation <- function(db) {
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

#' Get the names of the role playing dimensions
#'
#' Role playing dimensions are defined in star_databases. When integrating
#' several star_databases to form a constellation, role playing dimensions are
#' also integrated. This function allows you to see the result.
#'
#' @param db A `constellation` object.
#'
#' @return A list of vector of strings with dimension names.
#'
#' @family star database definition functions
#' @seealso \code{\link{star_schema}}, \code{\link{flat_table}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
#'   role_playing_dimension(
#'     rpd = "When",
#'     roles = c("When Available", "When Received")
#'   )
#'
#' db2 <- star_database(mrs_age_schema_rpd, ft_age_rpd) |>
#'   role_playing_dimension(
#'     rpd = "When Arrived",
#'     roles = c("When Available")
#'   )
#' rpd <- constellation("MRS", db1, db2) |>
#'   get_role_playing_dimension_names()
#'
#' @export
get_role_playing_dimension_names <- function(db) UseMethod("get_role_playing_dimension_names")

#' @rdname get_role_playing_dimension_names
#'
#' @export
get_role_playing_dimension_names.star_database <- function(db) {
  r <- db$rpd
  names(r) <- sprintf("rpd_%d", 1:length(r))
  for (i in seq_along(r)) {
    r[[i]] <- sort(r[[i]])
  }
  r
}

