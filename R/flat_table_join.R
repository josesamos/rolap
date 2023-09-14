#' Transform a flat table into a look up table
#'
#' Checks that the given attributes form a primary key of the table. Otherwise,
#' group the records so that they form a primary key. To carry out the groupings,
#' aggregation functions for attributes and measures must be provided.
#'
#' If no attribute is indicated, all the attributes are considered to form the
#' primary key.
#'
#' @param ft A `flat_table` object.
#' @param pk_attributes A vector of strings, attribute names.
#' @param attributes A vector of strings, rest of attribute names.
#' @param attribute_agg A vector of strings, attribute aggregation functions.
#' @param measures A vector of strings, measure names.
#' @param measure_agg A vector of strings, measure aggregation functions.
#'
#' @return A `flat_table` object.
#'
#' @family flat table join functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   lookup_table(
#'     measures = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'     measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
#'   )
#'
#' @export
lookup_table <- function(ft, pk_attributes, attributes, attribute_agg, measures, measure_agg) UseMethod("lookup_table")

#' @rdname lookup_table
#'
#' @export
lookup_table.flat_table <-
  function(ft,
           pk_attributes = NULL,
           attributes = NULL,
           attribute_agg = NULL,
           measures = NULL,
           measure_agg = NULL) {
    pk_attributes <- validate_attributes(ft$attributes, pk_attributes)
    ft$pk_attributes <- pk_attributes
    ft$table <- replace_empty_values_table(ft$table, pk_attributes, unknown_value = ft$unknown_value)
    pk <- unique(ft$table[, pk_attributes])
    if (nrow(pk) < nrow(ft$table)) {
      if (length(pk_attributes) == length(ft$attributes) + length(ft$measures)) {
        ft$table <- pk
      } else {
        if (!is.null(attributes)) {
          attributes <- validate_attributes(ft$attributes, attributes)
          stopifnot(
            "Each additional attribute must have an aggregation function." = length(attributes) == length(attribute_agg)
          )
        }
        if (length(ft$attributes) > length(pk_attributes) + length(attribute_agg)) {
          stop(
            "Attributes do not form a primary key. Aggregation functions (MAX, MIN) are needed for the rest of the attributes."
          )
        }
        if (!setequal(measures, ft$measures) |
            length(ft$measures) != length(measure_agg)) {
          stop(
            "Attributes do not form a primary key. Aggregation functions (MAX, MIN, SUM, MEAN) are needed for measures."
          )
        }
        attribute_agg <- toupper(attribute_agg)
        for (f in attribute_agg) {
          stopifnot("Aggregation functions for attributes must be MAX or MIN." = f %in% c('MAX', 'MIN'))
        }
        measure_agg <- toupper(measure_agg)
        for (f in measure_agg) {
          stopifnot(
            "Aggregation functions for measures must be MAX, MIN, SUM or MEAN." = f %in% c('MAX', 'MIN', 'SUM', 'MEAN')
          )
        }
        ft_group <-
          dplyr::group_by_at(as.data.frame(ft$table),
                             dplyr::vars(tidyselect::all_of(pk_attributes)))
        agg <- list()
        measures <- c(attributes, ft$measures)
        agg_functions <- c(attribute_agg, measure_agg)
        for (i in seq_along(measures)) {
          if (agg_functions[i] == "MEAN") {
            df <-
              dplyr::summarize_at(ft_group, dplyr::vars(measures[i]), mean, na.rm = TRUE)
          } else if (agg_functions[i] == "MAX") {
            df <-
              dplyr::summarize_at(ft_group, dplyr::vars(measures[i]), max, na.rm = TRUE)
          } else if (agg_functions[i] == "MIN") {
            df <-
              dplyr::summarize_at(ft_group, dplyr::vars(measures[i]), min, na.rm = TRUE)
          } else {
            df <-
              dplyr::summarize_at(ft_group, dplyr::vars(measures[i]), sum, na.rm = TRUE)
          }
          agg <- c(agg, list(df))
        }
        res <-
          dplyr::ungroup(purrr::reduce(agg, dplyr::inner_join, by = pk_attributes))
        ft$table <- res[, c(ft$attributes, ft$measures)]
      }
    }
    ft$operations <-
      add_operation(
        ft$operations,
        "lookup_table",
        pk_attributes,
        c(attributes, '|', attribute_agg),
        c(measures, '|', measure_agg)
      )
    ft
  }


#' Get the names of the primary key attributes of a flat table
#'
#' Obtain the names of the attributes that form the primary key of a flat table,
#' if defined.
#'
#' @param ft A `flat_table` object.
#' @param as_definition A boolean, as the definition of the vector in R.
#'
#' @return A vector of strings or a `tibble`, attribute names.
#'
#' @family flat table join functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   lookup_table(
#'     measures = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'     measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
#'   )
#' names <- ft |>
#'   get_pk_attribute_names()
#'
#' @export
get_pk_attribute_names <- function(ft, as_definition) UseMethod("get_pk_attribute_names")

#' @rdname get_pk_attribute_names
#'
#' @export
get_pk_attribute_names.flat_table <- function(ft, as_definition = FALSE) {
  transform_names(names = ft$pk_attributes, ordered = FALSE, as_definition)
}


#' Join a flat table with a lookup table
#'
#' To join a flat table with a lookup table, the attributes of the first table
#' that will be used in the operation are indicated. The lookup table must have
#' the primary key previously defined.
#'
#' If no attributes are indicated, those that form the primary key of the lookup
#' table are considered in the flat table.
#'
#' @param ft A `flat_table` object.
#' @param fk_attributes A vector of strings, attribute names.
#' @param lookup A `flat_table` object.
#'
#' @return A `flat_table` object.
#'
#' @family flat table join functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' lookup <- flat_table('iris', iris) |>
#'   lookup_table(
#'     measures = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'     measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
#'   )
#' ft <- flat_table('iris', iris) |>
#'   join_lookup_table(lookup = lookup)
#'
#' @export
join_lookup_table <- function(ft, fk_attributes, lookup) UseMethod("join_lookup_table")

#' @rdname join_lookup_table
#'
#' @export
join_lookup_table.flat_table <-
  function(ft, fk_attributes = NULL, lookup) {
    fk_attributes <- validate_lookup_parameters(ft, fk_attributes, lookup)
    ft$table <- replace_empty_values_table(ft$table, fk_attributes, unknown_value = ft$unknown_value)
    rest <-
      setdiff(c(lookup$attributes, lookup$measures),
              lookup$pk_attributes)
    t <- lookup$table[, c(lookup$pk_attributes, rest)]
    rest2 <- c()
    for (a in rest) {
      if (a %in% fk_attributes) {
        a <- paste0(a, '.lookup')
      }
      rest2 <- c(rest2, a)
    }
    names(t) <- c(fk_attributes, rest2)
    ft$table <-
      dplyr::left_join(ft$table, t, by = fk_attributes, suffix = c("", ".lookup"))
    # classify the new columns
    rest <- setdiff(names(ft$table), c(ft$attributes, ft$measures))
    types <- dplyr::summarise_all(ft$table, class)[, rest]
    names <- names(types)
    for (t in seq_along(types)) {
      if (types[t] %in% c("integer", "double", "integer64", "numeric")) {
        ft$measures <- c(ft$measures, names[t])
      } else {
        ft$attributes <- c(ft$attributes, names[t])
      }
    }
    ft$table <- ft$table[, c(ft$attributes, ft$measures)]
    pos <- length(ft$lookup_tables) + 1
    ft$lookup_tables[[pos]] <- lookup
    ft$operations <-
      add_operation(ft$operations, "join_lookup_table", fk_attributes, pos)
    ft
  }


#' Check the result of joining a flat table with a lookup table
#'
#' Before joining a flat table with a lookup table we can check the result to
#' determine if we need to adapt the values of some instances or add new elements
#' to the lookup table. This function returns the values of the foreign key of
#' the flat table that do not correspond to the primary key of the lookup table.
#'
#' If no attributes are indicated, those that form the primary key of the lookup
#' table are considered in the flat table.
#'
#' @param ft A `flat_table` object.
#' @param fk_attributes A vector of strings, attribute names.
#' @param lookup A `flat_table` object.
#'
#' @return A `tibble` with attribute values.
#'
#' @family flat table join functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' lookup <- flat_table('iris', iris) |>
#'   lookup_table(
#'     measures = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
#'     measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
#'   )
#' values <- flat_table('iris', iris) |>
#'   check_lookup_table(lookup = lookup)
#'
#' @export
check_lookup_table <- function(ft, fk_attributes, lookup) UseMethod("check_lookup_table")

#' @rdname check_lookup_table
#'
#' @export
check_lookup_table.flat_table <-
  function(ft, fk_attributes = NULL, lookup) {
    fk_attributes <- validate_lookup_parameters(ft, fk_attributes, lookup)
    pk <- unique(lookup$table[, lookup$pk_attributes])
    ft$table <- replace_empty_values_table(ft$table, fk_attributes, unknown_value = ft$unknown_value)
    fk <- unique(ft$table[, fk_attributes])
    names(pk) <- names(fk)
    dplyr::setdiff(fk, pk)
  }


#' Validate lookup parameters
#'
#' @param ft A `flat_table` object.
#' @param fk_attributes A vector of strings, attribute names.
#' @param lookup A `flat_table` object.
#'
#' @return A vector of strings, fk attribute names.
#'
#' @keywords internal
validate_lookup_parameters <- function(ft, fk_attributes, lookup) {
  stopifnot(
    "The lookup parameter does not include flat_table object." = methods::is(lookup, "flat_table")
  )
  stopifnot("The lookup parameter does not have a primary key defined." = length(lookup$pk_attributes) > 0)
  if (is.null(fk_attributes)) {
    fk_attributes <- lookup$pk_attributes
  }
  fk_attributes <- validate_attributes(ft$attributes, fk_attributes)
  stopifnot(
    "The foreign and primary keys do not match." = length(lookup$pk_attributes) == length(fk_attributes)
  )
  pk <- unique(lookup$table[, lookup$pk_attributes])
  stopifnot(
    "The lookup table has probably changed since its definition." = nrow(lookup$table) == nrow(pk)
  )
  fk_attributes
}
