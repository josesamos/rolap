
#' Checks the refresh of the selected star database from the given database
#'
#' Checks the refresh operation of the selected star database from the given
#' database. Once this operation is carried out, the results can be consulted on
#' the new instances in dimensions or existing instances in the facts.
#'
#' @param db A `star_database` object.
#' @param star A string, star database name or index in constellation, database
#' to refresh.
#' @param refresh_db A `star_database` object with the same structure with
#' new data.
#'
#' @return A `star_database` object.
#'
#' @family star database transformation functions
#' @seealso \code{\link{as_tibble_list}}, \code{\link{as_dm_class}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   check_refesh()
#'
#' @export
check_refesh <- function(db, star, refresh_db)
  UseMethod("check_refesh")

#' @rdname check_refesh
#'
#' @export
check_refesh.star_database <- function(db, star = 1, refresh_db) {
  sort(names(db$dimensions))
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
