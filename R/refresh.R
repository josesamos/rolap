
#' Select attributes of a flat table
#'
#' Select only the indicated attributes from the flat table.
#'
#' @param ft A `flat_table` object.
#' @param attributes A vector of names.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris) |>
#'   refresh(attributes = c('Species'))
#'
#' ft <- flat_table('ft_num', ft_num) |>
#'   refresh(attributes = c('Year', 'WEEK', 'Week Ending Date'))
#'
#' @export
refresh <- function(ft, attributes) UseMethod("refresh")

#' @rdname refresh
#'
#' @export
refresh.flat_table <- function(ft, attributes) {
  attributes <- validate_attributes(ft$attributes, attributes)
  ft$table <- ft$table[, c(attributes, ft$measures)]
  ft$attributes <- attributes
  ft$operations <- add_operation(ft$operations, "refresh", attributes)
  ft
}



#' Refresh a new flat table
#'
#' Refresh a new a flat table with the operations of another structure based on
#' a flat table.
#'
#' @param ft A `flat_table` object.
#' @param s_op An object with defined modification operations.
#'
#' @return A `flat_table` object.
#'
#' @family flat table transformation functions
#' @seealso \code{\link{flat_table}}
#'
#' @examples
#'
#' ft <- flat_table('iris', iris)
#'
#' ft <- flat_table('ft_num', ft_num)
#'
#' @export
refresh_new <- function(ft, s_op) UseMethod("refresh_new")

#' @rdname refresh_new
#'
#' @export
refresh_new.flat_table <- function(ft, s_op) {

  browser()

  stopifnot("The flat table to be refreshed can only have the definition operation." = nrow(ft$operations$operations) == 1)
  stopifnot("The flat table to be refreshed can only have the definition operation." = ft$operations$operations[1, 1] == "flat_table")
  operations <- s_op$operations$operations
  for (i in 1:nrow(operations)) {
    op <- operations[i, ]
    if (op$operation == "flat_table") {
      stopifnot("The operation of creating the flat table must be the first." = i == 1)
      ft <- interpret_operation_flat_table(ft, op)
    } else if (op$operation == "transform_to_measure") {
      ft <- interpret_operation_transform_to_measure(ft, op)
    } else if (op$operation == "transform_attribute_format") {
      ft <- interpret_operation_transform_attribute_format(ft, op)
    } else if (op$operation == "replace_empty_values") {
      ft <- interpret_operation_replace_empty_values(ft, op)
    } else if (op$operation == "add_custom_column") {
      ft <- interpret_operation_add_custom_column(ft, op)
    } else if (op$operation == "replace_attribute_values") {
      ft <- interpret_operation_replace_attribute_values(ft, op)
    } else if (op$operation == "join_lookup_table") {
      ft <- interpret_operation_join_lookup_table(ft, op, s_op$lookup_tables)
    }
  }

  ft
}

# 8          select_attributes


#' Interpret operation
#'
#               operation,    name,                   details,    details2
# add_operation("flat_table", c(name, unknown_value), attributes, measures),
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_flat_table <- function(ft, op) {
  name <- string_to_vector(op$name)
  unknown_value <- name[2]
  name <- name[1]
  attributes <- string_to_vector(op$details)
  measures <- string_to_vector(op$details2)
  stopifnot("The two tables must have the same attributes." = length(setdiff(attributes, ft$attributes)) == 0)
  stopifnot("The two tables must have the same measures." = length(setdiff(measures, ft$measures)) == 0)
  ft$name <- name
  ft$unknown_value <- unknown_value
  ft$attributes <- attributes
  ft$measures <- measures
  ft$table <- ft$table[, c(attributes, measures)]
  ft$operations$operations <- op
  ft
}


#' Interpret operation
#'
#'  operation,             name,       details, details2
#' "transform_to_measure", attributes, k_sep, decimal_sep
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_to_measure <- function(ft, op) {
  attributes <- string_to_vector(op$name)
  k_sep <- string_to_vector(op$details)
  decimal_sep <- string_to_vector(op$details2)
  transform_to_measure(ft, attributes, k_sep, decimal_sep)
}


#' Interpret operation
#'
#'  operation,                   name,       details,                  details2
#' "transform_attribute_format", attributes, c(width, decimal_places), c(k_sep, decimal_sep)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_attribute_format <- function(ft, op) {
  attributes <- string_to_vector(op$name)
  details <- string_to_vector(op$details)
  width <- as.integer(details[1])
  decimal_places <- as.integer(details[2])
  details2 <- string_to_vector(op$details2)
  k_sep <- details2[1]
  decimal_sep <- details2[2]
  transform_attribute_format(ft, attributes, width, decimal_places, k_sep, decimal_sep)
}


#' Interpret operation
#'
#'  operation,             name,       details,                  details2
#' "replace_empty_values", attributes, empty_values
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_empty_values <- function(ft, op) {
  attributes <- string_to_vector(op$name)
  empty_values <- string_to_vector(op$details)
  replace_empty_values(ft, attributes, empty_values)
}


#' Interpret operation
#'
#'  operation,          name, details,                  details2
#' "add_custom_column", name, as.character(list(definition))
#'
#' f <- function(...)
#' g <- as.character(list(f))
#' h <- eval(parse(text = g))
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_add_custom_column <- function(ft, op) {
  name <- string_to_vector(op$name)
  g <- string_to_vector(op$details)
  definition <- eval(parse(text = g))
  add_custom_column(ft, name, definition)
}


#' Interpret operation
#'
#'  operation,                 name,       details, details2
#' "replace_attribute_values", attributes, old,     new)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_attribute_values <- function(ft, op) {
  attributes <- string_to_vector(op$name)
  old <- string_to_vector(op$details)
  new <- string_to_vector(op$details2)
  replace_attribute_values(ft, attributes = attributes, old = old, new = new)
}


#' Interpret operation
#'
#'  operation,          name,          details, details2
#' "join_lookup_table", fk_attributes, pos)
#'
#' @param ft flat table
#' @param op operation
#' @param lookup_tables lookup tables
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_join_lookup_table <- function(ft, op, lookup_tables) {
  fk_attributes <- string_to_vector(op$name)
  pos <- as.integer(string_to_vector(op$details))
  lookup <- lookup_tables[[pos]]
  join_lookup_table(ft, fk_attributes, lookup)
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



