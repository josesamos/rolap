


#' Checks the refresh of the selected star database from the given database
#'
#' Checks the refresh operation of the selected star database from the given
#' database. Once this operation is carried out, the results can be consulted on
#' the new instances in dimensions or existing instances in the facts.
#'
#' @param db A `star_database` object.
#' @param refresh_db A `star_database` object with the same structure with
#' new data.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
check_refesh <- function(db, refresh_db) {
  star <- names(refresh_db$facts)
  of <- db$facts[[star]]
  od <- db$dimensions[of$dim_int_names]
  rf <- refresh_db$facts[[star]]
  rd <- refresh_db$dimensions
  for (d in names(od)) {
    # rename dimension surrogate key
    names <- names(od[[d]]$table)
    i <- which(names == rd[[d]]$surrogate_key)
    names[i] <- paste0('original_', names[i])
    names(od[[d]]$table) <- names
    attributes <- names[-i]
    # rename fact surrogate key
    names <- names(of$table)
    i <- which(names == rd[[d]]$surrogate_key)
    names[i] <- paste0('original_', names[i])
    names(of$table) <- names

    rd[[d]]$table <- rd[[d]]$table |>
      dplyr::left_join(od[[d]]$table, by = attributes)
    rf$table <- rf$table |>
      dplyr::inner_join(rd[[d]]$table,
                        by = rd[[d]]$surrogate_key) |>
      dplyr::select(-tidyselect::all_of(attributes))
  }
  measures <-
    setdiff(names(of$table), c(of$surrogate_keys, paste0('original_', rf$surrogate_keys)))
  of$table$existing_fact <- TRUE
  rf$table <- rf$table |>
    dplyr::left_join(dplyr::select(of$table, -tidyselect::all_of(measures)),
                      by = paste0('original_', rf$surrogate_keys))
  rf$table$existing_fact[is.na(rf$table$existing_fact)] <-  FALSE
  rf <- list(rf)
  names(rf) <- star
  c(rf, rd)
}



#' Get existing fact instances
#'
#' From the planned update, it obtains the instances of the update facts that
#' are already included in the star database facts to be updated.
#'
#' When the update occurs, we need to determine what happens to these instances.
#'
#' @param sdbu A `star_database_update` object.
#'
#' @return A `tibble` object.
#'
#' @family star database refresh functions
#'
#' @examples
#'
#' f1 <- flat_table('ft_num', ft_cause_rpd) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   replace_attribute_values(
#'     name = "When Available",
#'     old = c('1962', '11', '1962-03-14'),
#'     new = c('1962', '3', '1962-01-15')
#'   ) |>
#'   group_dimension_instances(name = "When")
#' f2 <- flat_table('ft_num2', ft_cause_rpd) |>
#'   update_according_to(f1)
#' fact_instances <- f2 |>
#'   get_existing_fact_instances()
#'
#' @export
get_existing_fact_instances <- function(sdbu) UseMethod("get_existing_fact_instances")

#' @rdname get_existing_fact_instances
#'
#' @export
get_existing_fact_instances.star_database_update <- function(sdbu) {
  ft <- sdbu$combination[[1]]$table[sdbu$combination[[1]]$table$existing_fact, ]
  sk <- sdbu$combination[[1]]$surrogate_keys
  measures <- setdiff(names(ft), c(sk, paste0('original_', sk), 'existing_fact'))
  attributes <- NULL
  dimensions <- sdbu$combination[-1]
  for (d in names(dimensions)) {
    dim <- dimensions[[d]]$table
    key <- dimensions[[d]]$surrogate_key
    attributes <-
      c(attributes, setdiff(names(dim), c(key, paste0('original_', key))))
    ft <- ft |>
      dplyr::inner_join(dim, by = key)
  }
  ft <- ft |>
    dplyr::select(tidyselect::all_of(c(attributes, measures)))
  ft
}


#' Get new dimension instances
#'
#' From the planned update, it obtains the instances of the update dimensions
#' that are not included in the star database dimensions to be updated.
#'
#' @param sdbu A `star_database_update` object.
#'
#' @return A list of `tibble` objects.
#'
#' @family star database refresh functions
#'
#' @examples
#'
#' f1 <- flat_table('ft_num', ft_cause_rpd) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   replace_attribute_values(
#'     name = "When Available",
#'     old = c('1962', '11', '1962-03-14'),
#'     new = c('1962', '3', '1962-01-15')
#'   ) |>
#'   group_dimension_instances(name = "When")
#' f2 <- flat_table('ft_num2', ft_cause_rpd) |>
#'   update_according_to(f1)
#' dim_instances <- f2 |>
#'   get_new_dimension_instances()
#'
#' @export
get_new_dimension_instances <- function(sdbu) UseMethod("get_new_dimension_instances")

#' @rdname get_new_dimension_instances
#'
#' @export
get_new_dimension_instances.star_database_update <- function(sdbu) {
  dimensions <- sdbu$combination[-1]
  res <- list()
  for (d in names(dimensions)) {
    dim <- dimensions[[d]]$table
    pk <- dimensions[[d]]$surrogate_key
    pko <-  paste0('original_', pk)
    attributes <- setdiff(names(dim), c(pk, pko))
    dim <- dim[is.na(dim[pko]), ]
    dim <- dim |>
      dplyr::select(tidyselect::all_of(attributes))
    if (nrow(dim) > 0) {
      dim <- list(dim)
      names(dim) <- d
      res <- c(res, dim)
    }
  }
  res
}



# Class que se crea a partir de una star_database y una flat table con dos opciones
# según se quiera hacer con las instancias de los hechos que ya existan (las nuevas no hay problema)
# 1. sustituir las instancias de los hechos existentes con las nuevas que aparezcan
# 2. agregar las instancias de los hechos existentes y las nuevas que aparezcan
# se aplican las transformaciones sobre la flat table
# se actualiza la star_database (hechos y dimensiones)
# se generan nuevas instancias en dimensiones y hechos
# se generan actualizaciones sobre instancias existentes en los hechos
#
# Adicionalmente guardar y restaurar una star_database de una BDR.
