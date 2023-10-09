
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
  stopifnot("The flat table to be refreshed can only have the definition operation." = nrow(ft$operations$operations) == 1)
  stopifnot("The flat table to be refreshed can only have the definition operation." = ft$operations$operations[1, 1] == "flat_table")
  operations <- s_op$operations$operations
  for (i in 1:nrow(operations)) {
    op <- operations[i, ]
    if (op$operation == "flat_table") {
      stopifnot("The operation of creating the flat table must be the first." = i == 1)
      ft <- interpret_operation_flat_table(ft, op)
    } else if (op$operation == "join_lookup_table") {
      ft <- interpret_operation_join_lookup_table(ft, op, s_op$lookup_tables)
    } else if (op$operation %in% c("add_custom_column",
                                   "lookup_table",
                                   "remove_instances_without_measures",
                                   "replace_attribute_values",
                                   "replace_empty_values",
                                   "replace_string",
                                   "replace_unknown_values",
                                   "select_attributes",
                                   "select_instances",
                                   "select_instances_by_comparison",
                                   "select_measures",
                                   "separate_measures",
                                   "set_attribute_names",
                                   "set_measure_names",
                                   "snake_case",
                                   "transform_attribute_format",
                                   "transform_from_values",
                                   "transform_to_attribute",
                                   "transform_to_measure",
                                   "transform_to_values")) {
      ft <- eval(parse(text = paste0("interpret_operation_", op$operation, "(ft, op)")))
    } else {
      stop(sprintf("Operation %s is not considered", op$operation))
    }
  }
  ft
}



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


#' Interpret operation
#'
#'  operation,          name
#' "select_attributes", attributes)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_attributes <- function(ft, op) {
  attributes <- string_to_vector(op$name)
  select_attributes(ft, attributes = attributes)
}


#' Interpret operation
#'
#'  operation,       name,       details, details2
#' "replace_string", attributes, string, replacement)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_string <- function(ft, op) {
  attributes <- string_to_vector(op$name)
  string <- string_to_vector(op$details)
  replacement <- string_to_vector(op$details2)
  replace_string(ft, attributes, string, replacement)
}


#' Interpret operation
#'
#'  operation,       name,       details, details2
#' "select_instances", not, attributes, unlist(values)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_instances <- function(ft, op) {
  not <- as.logical(string_to_vector(op$name))
  attributes <- string_to_vector(op$details)
  values <- string_to_vector(op$details2)
  if (length(attributes) > 1) {
    m <- matrix(values, nrow = length(attributes))
    values <- split(m, col(m))
  }
  select_instances(ft, not, attributes, values)
}


#' Interpret operation
#'
#'  operation,       name,        details,                           details2
#' "lookup_table", pk_attributes, c(attributes, '|', attribute_agg), c(measures, '|', measure_agg)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_lookup_table <- function(ft, op) {
  pk_attributes <- string_to_vector(op$name)
  attributes <- NULL
  attribute_agg <- NULL
  measures <- NULL
  measure_agg <- NULL
  details <- string_to_vector(op$details)
  l <- length(details)
  if (l > 1) {
    s <- which(details == '|')
    if (s > 1) {
      attributes <- details[1:(s-1)]
    }
    if (s < l) {
      attribute_agg <- details[(s+1):l]
    }
  }
  details2 <- string_to_vector(op$details2)
  l <- length(details2)
  if (l > 1) {
    s <- which(details2 == '|')
    if (s > 1) {
      measures <- details2[1:(s-1)]
    }
    if (s < l) {
      measure_agg <- details2[(s+1):l]
    }
  }
  lookup_table(ft, pk_attributes, attributes, attribute_agg, measures, measure_agg)
}



#' Interpret operation
#'
#'  operation,               name,     details,                  details2
#' "transform_to_attribute", measures, c(width, decimal_places), c(k_sep, decimal_sep)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_to_attribute <- function(ft, op) {
  measures <- string_to_vector(op$name)
  details <- string_to_vector(op$details)
  width <- as.integer(details[1])
  decimal_places <- as.integer(details[2])
  details2 <- string_to_vector(op$details2)
  k_sep <- details2[1]
  decimal_sep <- details2[2]
  transform_to_attribute(ft, measures, width, decimal_places, k_sep, decimal_sep)
}



#' Interpret operation
#'
#'  operation,                       name,              details,            details2
#' "select_instances_by_comparison", c(not, n_ele_set), unlist(attributes), c(unlist(comparisons), unlist(values))
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_instances_by_comparison <- function(ft, op) {
  name <- string_to_vector(op$name)
  not <- as.logical(as.integer(string_to_vector(name[1])))
  n_ele_set <- as.integer(name[-1])
  att <- string_to_vector(op$details)
  details2 <- string_to_vector(op$details2)
  l <- length(details2)
  com <- details2[1:(l/2)]
  val <- details2[(l/2 + 1):l]
  n <- length(n_ele_set)
  if (n > 1) {
    attributes <- vector(mode = "list", length = n)
    comparisons <- vector(mode = "list", length = n)
    values <- vector(mode = "list", length = n)
    j <- 1
    for (i in 1:n) {
      k <- j + n_ele_set[i] - 1
      attributes[[i]] <- att[j:k]
      comparisons[[i]] <- com[j:k]
      values[[i]] <- val[j:k]
      j <- k + 1
    }
  } else {
    attributes <- att
    comparisons <- com
    values <- val
  }
  select_instances_by_comparison(ft, not, attributes, comparisons, values)
}


#' Interpret operation
#'
#'  operation,          name,   details
#' "select_measures", measures, na_rm
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_measures <- function(ft, op) {
  measures <- string_to_vector(op$name)
  na_rm <- as.logical(string_to_vector(op$details))
  select_measures(ft, measures, na_rm)
}


#' Interpret operation
#'
#'  operation,          name,     details, details2
#' "separate_measures", measures, names, na_rm)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_separate_measures <- function(ft, op) {
  measures <- string_to_vector(op$name)
  names <- string_to_vector(op$details)
  na_rm <- as.logical(string_to_vector(op$details2))
  separate_measures(ft, measures, names, na_rm)
}


#' Interpret operation
#'
#'  operation,            name, details, details2
#' "set_attribute_names", name, old,     new)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_set_attribute_names <- function(ft, op) {
  name <- string_to_vector(op$name)
  old <- string_to_vector(op$details)
  new <- string_to_vector(op$details2)
  set_attribute_names(ft, name, old, new)
}


#' Interpret operation
#'
#'  operation,            name, details, details2
#' "set_measure_names", name, old,     new)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_set_measure_names <- function(ft, op) {
  name <- string_to_vector(op$name)
  old <- string_to_vector(op$details)
  new <- string_to_vector(op$details2)
  set_measure_names(ft, name, old, new)
}


#' Interpret operation
#'
#'  operation,
#' "snake_case")
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_snake_case <- function(ft, op) {
  snake_case(ft)
}


#' Interpret operation
#'
#'  operation,              name,
#' "transform_from_values", attribute)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_from_values <- function(ft, op) {
  attribute <- string_to_vector(op$name)
  transform_from_values(ft, attribute)
}


#' Interpret operation
#'
#'  operation,            name,      details, details2
#' "transform_to_values", attribute, measure, c(id_reverse, na_rm))
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_to_values <- function(ft, op) {
  attribute <- string_to_vector(op$name)
  measure <- string_to_vector(op$details)
  details2 <- string_to_vector(op$details2)
  if (length(details2) == 2) {
    id_reverse <- details2[1]
    na_rm <- as.logical(details2[2])
  } else {
    id_reverse <- NULL
    na_rm <- as.logical(details2[1])
  }
  transform_to_values(ft, attribute, measure, id_reverse, na_rm)
}


#' Interpret operation
#'
#'  operation,                name,       details, details2
#' "replace_unknown_values",  attributes, value)
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_unknown_values <- function(ft, op) {
  attributes <- string_to_vector(op$name)
  value <- string_to_vector(op$details)
  replace_unknown_values(ft, attributes, value)
}


#' Interpret operation
#'
#'  operation,
#' "remove_instances_without_measures")
#'
#' @param ft flat table
#' @param op operation
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_remove_instances_without_measures <- function(ft, op) {
  remove_instances_without_measures(ft)
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



