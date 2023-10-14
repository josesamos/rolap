
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
