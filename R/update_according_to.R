
#' Update a flat table according to another structure
#'
#' Update a flat table with the operations of another structure based on a flat
#' table.
#'
#' @param ft A `flat_table` object.
#' @param sdb A `star_database` object with defined modification operations.
#' @param star A string or integer, star database name or index in constellation.
#' @param sdb_operations A `star_database` object with new defined modification
#' operations.
#'
#' @return A `star_database_update` object.
#'
#' @family star database refresh functions
#' @seealso \code{\link{star_database}}
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
#'
#' @export
update_according_to <-
  function(ft,
           sdb,
           star,
           sdb_operations)
    UseMethod("update_according_to")

#' @rdname update_according_to
#'
#' @export
update_according_to.flat_table <-
  function(ft,
           sdb,
           star = 1,
           sdb_operations = NULL) {
    if (!is.null(sdb_operations)) {
      source_op <- sdb_operations
    } else {
      source_op <- sdb
    }
    if (methods::is(source_op, "flat_table")) {
      operations <- source_op$operations$operations
      lookup_tables <- source_op$lookup_tables
    } else if (methods::is(source_op, "star_database")) {
      operations <- source_op$operations[[star]]$operations
      lookup_tables <- source_op$lookup_tables[[star]]
      schema <- source_op$schemas[[star]]
    } else {
      stop(sprintf( "The %s class is not supported to update operations.", class(source_op)))
    }
    out_file <- tempfile()
    file <- file(out_file, open = "wt")
    writeLines("ft <- ", file)
    n <- nrow(operations)
    for (i in 1:n) {
      last_op <- i == n
      op <- operations[i, ]
      if (op$operation == "star_database") {
        ft <- interpret_operation_star_database(ft, op, schema, file, last_op)
      } else if (op$operation == "join_lookup_table") {
        ft <- interpret_operation_join_lookup_table(ft, op, lookup_tables, file, last_op)
      } else if (op$operation %in% c(
        "add_custom_column",
        "flat_table",
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
        "transform_to_values",
              # star_database only
        "group_dimension_instances",
        "role_playing_dimension"
      )) {
        ft <-
          eval(parse(text = paste0(
            "interpret_operation_", op$operation, "(ft, op, file, last_op)"
          )))
      } else {
        stop(sprintf("Operation %s is not considered", op$operation))
      }
    }
    close(file)
    r <- reformat_file(out_file, function_name = 'transform_instance_table')
    if (methods::is(sdb, "star_database")) {
      combination <- check_refesh(sdb, ft)
    } else {
      combination <- NULL
    }
    db <-
      structure(
        list(
          file = out_file,
          code = r,
          star_database = ft,
          combination = combination
        ),
        class = "star_database_update"
      )
  }



#' Interpret operation
#'
#               operation,    name,                   details,    details2
# add_operation("flat_table", c(name, unknown_value), attributes, measures),
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_flat_table <- function(ft, op, file, last_op) {
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
  if (!is.null(file)) {
    writeLines(c(
      paste0("  ", op$operation, "("),
      paste0("    name = ", sprintf('"%s",', name)),
      paste0("    instances = instance_df,"),
      paste0("    unknown_value = ", sprintf('"%s"', unknown_value)),
      line_last_op(last_op)
    ), file)
  }
  ft
}


#' Interpret operation
#'
#'  operation,             name,       details, details2
#' "transform_to_measure", attributes, k_sep, decimal_sep
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_to_measure <- function(ft, op, file, last_op) {
  attributes <- string_to_vector(op$name)
  k_sep <- string_to_vector(op$details)
  decimal_sep <- string_to_vector(op$details2)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      paste0("    k_sep = ", string_or_null(k_sep)),
      paste0("    decimal_sep = ", string_or_null(decimal_sep, last = TRUE)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  transform_to_measure(ft, attributes, k_sep, decimal_sep)
}


#' Interpret operation
#'
#'  operation,                   name,       details,                  details2
#' "transform_attribute_format", attributes, c(width, decimal_places), c(k_sep, decimal_sep, space_filling)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_attribute_format <- function(ft, op, file, last_op) {
  attributes <- string_to_vector(op$name)
  details <- string_to_vector(op$details)
  width <- as.integer(details[1])
  decimal_places <- as.integer(details[2])
  details2 <- string_to_vector(op$details2)
  k_sep <- details2[1]
  decimal_sep <- details2[2]
  space_filling <- as.logical(string_to_vector(details2[3]))
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      paste0("    width = ", sprintf('%d,', width)),
      paste0("    decimal_places = ", sprintf('%d,', decimal_places)),
      paste0("    k_sep = ", string_or_null(k_sep)),
      paste0("    decimal_sep = ", string_or_null(decimal_sep)),
      paste0("    space_filling = ", sprintf('%s', deparse(space_filling))),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  transform_attribute_format(ft, attributes, width, decimal_places, k_sep, decimal_sep, space_filling)
}


#' Interpret operation
#'
#'  operation,             name,       details,                  details2
#' "replace_empty_values", attributes, empty_values
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_empty_values <- function(ft, op, file, last_op) {
  attributes <- string_to_vector(op$name)
  empty_values <- string_to_vector(op$details)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      sprintf("    empty_values = %s", vector_presentation(empty_values)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
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
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_add_custom_column <- function(ft, op, file, last_op) {
  name <- string_to_vector(op$name)
  g <- string_to_vector(op$details)
  definition <- eval(parse(text = g))
  if (!is.null(file)) {
    writeLines(c(
      paste0("  ", op$operation, "("),
      paste0("    name = ", sprintf('"%s",', name)),
      paste0("    definition = **$FUNCTION$**"),
      line_last_op(last_op)
    ), file)
  }
  add_custom_column(ft, name, definition)
}


#' Interpret operation
#'
#'  operation,                 name,                     details, details2
#' "replace_attribute_values", attributes,               old,     new)
#' "replace_attribute_values", c(name, "|", attributes), old,     new)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_attribute_values <- function(ft, op, file, last_op) {
  attributes <- string_to_vector(op$name)
  name = NULL
  if (length(attributes) > 1) {
    if (attributes[2] == '|') {
      name <- attributes[1]
      attributes <- attributes[-c(1, 2)]
    }
  }
  old <- string_to_vector(op$details)
  new <- string_to_vector(op$details2)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf('    name = "%s",', ifelse(is.null(name), deparse(name), name)),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      sprintf("    old = %s,", vector_presentation(old)),
      sprintf("    new = %s", vector_presentation(new)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    if (is.null(name)) {
      l <- l[-2]
    }
    writeLines(l, file)
  }
  replace_attribute_values(ft, name, attributes, old, new)
}


#' Interpret operation
#'
#'  operation,          name,          details, details2
#' "join_lookup_table", fk_attributes, pos)
#'
#' @param ft flat table
#' @param op operation
#' @param lookup_tables lookup tables
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_join_lookup_table <- function(ft, op, lookup_tables, file, last_op) {
  fk_attributes <- string_to_vector(op$name)
  pos <- as.integer(string_to_vector(op$details))
  lookup <- lookup_tables[[pos]]
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    fk_attributes = %s,", vector_presentation(fk_attributes)),
      paste0("    lookup = **$LOOKUP$FLAT$TABLE$**"),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  join_lookup_table(ft, fk_attributes, lookup)
}


#' Interpret operation
#'
#'  operation,          name
#' "select_attributes", attributes)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_attributes <- function(ft, op, file, last_op) {
  attributes <- string_to_vector(op$name)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    attributes = %s", vector_presentation(attributes)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  select_attributes(ft, attributes = attributes)
}


#' Interpret operation
#'
#'  operation,       name,       details, details2
#' "replace_string", attributes, string, replacement)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_string <- function(ft, op, file, last_op) {
  attributes <- string_to_vector(op$name)
  string <- string_to_vector(op$details)
  replacement <- string_to_vector(op$details2)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      sprintf("    string = %s,", vector_presentation(string)),
      sprintf("    replacement = %s", vector_presentation(replacement)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  replace_string(ft, attributes, string, replacement)
}


#' Interpret operation
#'
#'  operation,       name,       details, details2
#' "select_instances", not, attributes, unlist(values)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_instances <- function(ft, op, file, last_op) {
  not <- as.logical(string_to_vector(op$name))
  attributes <- string_to_vector(op$details)
  values <- string_to_vector(op$details2)
  if (length(attributes) > 1) {
    m <- matrix(values, nrow = length(attributes))
    values <- split(m, col(m))
  }
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      paste0("    not = ", sprintf('%s,', deparse(not))),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      sprintf("    values = %s", vector_presentation(values)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
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
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_lookup_table <- function(ft, op, file, last_op) {
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
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    pk_attributes = %s,", vector_presentation(pk_attributes)),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      sprintf("    attribute_agg = %s,", vector_presentation(attribute_agg)),
      sprintf("    measures = %s,", vector_presentation(measures)),
      sprintf("    measure_agg = %s", vector_presentation(measure_agg)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
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
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_to_attribute <- function(ft, op, file, last_op) {
  measures <- string_to_vector(op$name)
  details <- string_to_vector(op$details)
  width <- as.integer(details[1])
  decimal_places <- as.integer(details[2])
  details2 <- string_to_vector(op$details2)
  k_sep <- details2[1]
  decimal_sep <- details2[2]
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    measures = %s,", vector_presentation(measures)),
      paste0("    width = ", sprintf('%d,', width)),
      paste0("    decimal_places = ", sprintf('%d,', decimal_places)),
      paste0("    k_sep = ", string_or_null(k_sep)),
      paste0("    decimal_sep = ", string_or_null(decimal_sep, last = TRUE)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  transform_to_attribute(ft, measures, width, decimal_places, k_sep, decimal_sep)
}



#' Interpret operation
#'
#'  operation,                       name,              details,            details2
#' "select_instances_by_comparison", c(not, n_ele_set), unlist(attributes), c(unlist(comparisons), unlist(values))
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_instances_by_comparison <- function(ft, op, file, last_op) {
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
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      paste0("    not = ", sprintf('%s,', deparse(not))),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      sprintf("    comparisons = %s,", vector_presentation(comparisons)),
      sprintf("    values = %s", vector_presentation(values)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
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
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_select_measures <- function(ft, op, file, last_op) {
  measures <- string_to_vector(op$name)
  na_rm <- as.logical(string_to_vector(op$details))
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    measures = %s,", vector_presentation(measures)),
      paste0("    na_rm = ", sprintf('%s', deparse(na_rm))),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  select_measures(ft, measures, na_rm)
}


#' Interpret operation
#'
#'  operation,          name,     details,            details2
#' "separate_measures", measures, c(name, names), na_rm)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_separate_measures <- function(ft, op, file, last_op) {
  measures <- as.list(string_to_vector(op$name))
  for (i in 1:length(measures)) {
    if (substr(measures[[i]], 1, 3) == "c(\"" |
        measures[[i]] == 'NULL' | measures[[i]] == 'character(0)') {
      measures[[i]] <- eval(parse(text = measures[[i]]))
    }
  }
  names <- string_to_vector(op$details)
  name <- names[1] # selected name
  names <- names[-1]
  na_rm <- as.logical(string_to_vector(op$details2))
  if (!is.null(file)) {
    if (last_op) {
      last_line <- sprintf("  magrittr::extract2('%s')", name)
    } else {
      last_line <- sprintf("  magrittr::extract2('%s') |>", name)
    }
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    measures = %s,", vector_presentation(measures)),
      sprintf("    names = %s,", vector_presentation(names)),
      paste0("    na_rm = ", sprintf('%s', deparse(na_rm))),
      "  ) |>",
      last_line
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  groups <- separate_measures(ft, measures, names, na_rm)
  groups[[name]]
}


#' Interpret operation
#'
#'  operation,            name, details, details2
#' "set_attribute_names", name, old,     new)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_set_attribute_names <- function(ft, op, file, last_op) {
  name <- string_to_vector(op$name)
  old <- string_to_vector(op$details)
  new <- string_to_vector(op$details2)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf('    name = "%s",', ifelse(is.null(name), deparse(name), name)),
      sprintf("    old = %s,", vector_presentation(old)),
      sprintf("    new = %s", vector_presentation(new)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    if (is.null(name)) {
      l <- l[-2]
    }
    writeLines(l, file)
  }
  set_attribute_names(ft, name, old, new)
}


#' Interpret operation
#'
#'  operation,            name, details, details2
#' "set_measure_names", name, old,     new)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_set_measure_names <- function(ft, op, file, last_op) {
  name <- string_to_vector(op$name)
  old <- string_to_vector(op$details)
  new <- string_to_vector(op$details2)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf('    name = "%s",', ifelse(is.null(name), deparse(name), name)),
      sprintf("    old = %s,", vector_presentation(old)),
      sprintf("    new = %s", vector_presentation(new)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    if (is.null(name)) {
      l <- l[-2]
    }
    writeLines(l, file)
  }
  set_measure_names(ft, name, old, new)
}


#' Interpret operation
#'
#'  operation,
#' "snake_case")
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_snake_case <- function(ft, op, file, last_op) {
  if (!is.null(file)) {
    writeLines(c(
      paste0("  ", op$operation, "("),
      line_last_op(last_op)
    ), file)
  }
  snake_case(ft)
}


#' Interpret operation
#'
#'  operation,              name,
#' "transform_from_values", attribute)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_from_values <- function(ft, op, file, last_op) {
  attribute <- string_to_vector(op$name)
  if (!is.null(file)) {
    writeLines(c(
      paste0("  ", op$operation, "("),
      paste0("    attribute = ", sprintf('"%s"', attribute)),
      line_last_op(last_op)
    ), file)
  }
  transform_from_values(ft, attribute)
}


#' Interpret operation
#'
#'  operation,            name,      details, details2
#' "transform_to_values", attribute, measure, c(id_reverse, na_rm))
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_transform_to_values <- function(ft, op, file, last_op) {
  attribute <- string_to_vector(op$name)
  measure <- string_to_vector(op$details)
  details2 <- string_to_vector(op$details2)
  if (length(details2) == 2) {
    id_reverse <- details2[1]
    id_reverse2 <- sprintf('"%s",', id_reverse)
    na_rm <- as.logical(details2[2])
  } else {
    id_reverse <- NULL
    id_reverse2 <- "NULL,"
    na_rm <- as.logical(details2[1])
  }
  if (!is.null(file)) {
    writeLines(c(
      paste0("  ", op$operation, "("),
      paste0("    attribute = ", sprintf('"%s",', attribute)),
      paste0("    measure = ", sprintf('"%s",', measure)),
      paste0("    id_reverse = ", id_reverse2),
      paste0("    na_rm = ", sprintf('%s', deparse(na_rm))),
      line_last_op(last_op)
    ), file)
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
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_replace_unknown_values <- function(ft, op, file, last_op) {
  attributes <- string_to_vector(op$name)
  value <- string_to_vector(op$details)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      sprintf("    attributes = %s,", vector_presentation(attributes)),
      paste0("    value = ", sprintf('"%s"', value)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  replace_unknown_values(ft, attributes, value)
}


#' Interpret operation
#'
#'  operation,
#' "remove_instances_without_measures")
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_remove_instances_without_measures <- function(ft, op, file, last_op) {
  if (!is.null(file)) {
    writeLines(c(
      paste0("  ", op$operation, "("),
      line_last_op(last_op)
    ), file)
  }
  remove_instances_without_measures(ft)
}


#' Interpret operation
#'
#'  operation,                name,    details,      details2
#' "star_database", names(db$schemas), unknown_value)
#'
#' @param ft flat table
#' @param op operation
#' @param schema multidimensional schema
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_star_database <- function(ft, op, schema, file, last_op) {
  unknown_value <- string_to_vector(op$details)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", "as_star_database", "("),
      paste0("    schema = **$STAR$SCHEMA$**"),
      line_last_op(last_op)
    )
    writeLines(l, file)
  }
  star_database_with_previous_operations(
    schema,
    instances = ft$table,
    unknown_value = unknown_value,
    operations = ft$operations,
    lookup_tables = ft$lookup_tables
  )
}


#' Interpret operation
#'
#'  operation,                name, details, details2
#' "role_playing_dimension",  rpd,  roles,   att_names)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_role_playing_dimension <- function(ft, op, file, last_op) {
  rpd <- string_to_vector(op$name)
  roles <- string_to_vector(op$details)
  att_names <- string_to_vector(op$details2)
  if (!is.null(file)) {
    l <- c(
      paste0("  ", op$operation, "("),
      paste0("    rpd = ", sprintf('"%s",', rpd)),
      sprintf("    roles = %s,", vector_presentation(roles)),
      sprintf("    att_names = %s", vector_presentation(att_names)),
      line_last_op(last_op)
    )
    l <- gsub("c('')", "NULL", l, fixed = TRUE)
    writeLines(l, file)
  }
  role_playing_dimension(ft, rpd, roles, rpd_att_names = FALSE, att_names =  att_names)
}


#' Interpret operation
#'
#'  operation,                  name
#' "group_dimension_instances", name)
#'
#' @param ft flat table
#' @param op operation
#' @param file file to write the code
#' @param last_op A boolean, is the last operation?
#'
#' @return A flat table.
#' @keywords internal
interpret_operation_group_dimension_instances <- function(ft, op, file, last_op) {
  name <- string_to_vector(op$name)
  if (!is.null(file)) {
    writeLines(c(
      paste0("  ", op$operation, "("),
      paste0("    name = ", sprintf('"%s"', name)),
      line_last_op(last_op)
    ), file)
  }
  group_dimension_instances(ft, name)
}


#' Get line last operation
#'
#' @param last_op A boolean, is the last operation?
#'
#' @return A string
#' @keywords internal
line_last_op <- function(last_op) {
  if (last_op) {
    res <- "  )"
  } else {
    res <- "  ) |>"
  }
  res
}


#' Get the representation to output
#'
#' @param value A string
#' @param last A boolean
#'
#' @return A string
#' @keywords internal
string_or_null <- function(value, last = FALSE) {
  if (last) {
    ifelse(
      is.null(value),
      sprintf('%s', deparse(value)),
      sprintf('"%s"', value)
    )
  } else {
    ifelse(
      is.null(value),
      sprintf('%s,', deparse(value)),
      sprintf('"%s",', value)
    )
  }
}


#' vector to string for presentation
#'
#' @param vector A vector
#'
#' @return A string
#' @keywords internal
vector_presentation <- function(vector) {
  paste(gsub("\"", "'", deparse(vector), fixed = TRUE), collapse = "")
}

#' Get line last operation
#'
#' @param out_file A string, file name.
#' @param function_name A string, name of the function to generate in the file.
#'
#' @return A string
#' @keywords internal
reformat_file <- function(out_file, function_name) {
  l <- readLines(out_file)
  lft <- sum(grepl("**$LOOKUP$FLAT$TABLE$**", l, fixed = TRUE))
  fun <- sum(grepl("**$FUNCTION$**", l, fixed = TRUE))
  sch <- sum(grepl("**$STAR$SCHEMA$**", l, fixed = TRUE))

  name <- paste0(function_name, " <- function(instance_df")
  if (lft > 0) {
    name <- paste0(name, ", lookup_ft")
    l <- gsub("**$LOOKUP$FLAT$TABLE$**", "lookup_ft", l, fixed = TRUE)
  }
  if (fun > 0) {
    name <- paste0(name, ", definition_fun")
    l <- gsub("**$FUNCTION$**", "definition_fun", l, fixed = TRUE)
  }
  if (sch > 0) {
    name <- paste0(name, ", star_sch")
    l <- gsub("**$STAR$SCHEMA$**", "star_sch", l, fixed = TRUE)
  }
  name <- paste0(name, ") {")
  file <- file(out_file, open = "wt")
  writeLines(name, file)
  l <- gsub("\"", "'", l, fixed = TRUE)
  n <- length(l)
  l[n] <- gsub(") |>", ")", l[n], fixed = TRUE)
  writeLines(paste0("  ", l), file)
  writeLines(c("", "  ft", "}", ""), file)
  name <- gsub(" <- function", "", name, fixed = TRUE)
  name <- gsub(" {", "", name, fixed = TRUE)
  writeLines(paste0("ft <- ", name), file)
  close(file)
  readLines(out_file)
}
