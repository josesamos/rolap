

#' Get a `geolayer` object
#'
#' From a `star_database` with at least one geoattribute, we obtain a `geolayer`
#' object that allows us to select the data to obtain a vector layer with
#' geographic information.
#'
#' If only one geographic attribute is defined, it is not necessary to indicate
#' the dimension or the attribute. By default, polygon geometry is considered.
#'
#' @param db An `star_database` object.
#' @param dimension A string, dimension name.
#' @param attribute A vector, attribute names.
#' @param geometry A string, geometry name.
#' @param include_nrow_agg A boolean, include default measure.
#'
#' @return A `geolayer` object.
#'
#' @family query functions
#'
#' @examples
#'
#' gl_polygon <- mrs_db_geo |>
#'   as_geolayer()
#'
#' gl_point <- mrs_db_geo |>
#'   as_geolayer(geometry = "point")
#'
#' @export
as_geolayer <- function(db,
                        dimension,
                        attribute,
                        geometry,
                        include_nrow_agg)
  UseMethod("as_geolayer")

#' @rdname as_geolayer
#' @export
as_geolayer.star_database <- function(db,
                                      dimension = NULL,
                                      attribute = NULL,
                                      geometry = NULL,
                                      include_nrow_agg = FALSE) {
  if (is.null(dimension)) {
    if (length(db$geo) == 1) {
      dimension <- names(db$geo)
    } else {
      stop("A dimension name must be indicated.")
    }
  }
  stopifnot("One dimension must be indicated (only one)." = length(dimension) == 1)
  validate_names(names(db$geo), dimension, concept = 'geodimension')
  if (is.null(attribute)) {
    if (length(db$geo[[dimension]]) == 1) {
      attribute <- names(db$geo[[dimension]])
    } else {
      stop("An attribute name must be indicated.")
    }
  }
  geoatt <- get_geoattribute_name(attribute)
  validate_names(names(db$geo[[dimension]]), geoatt, concept = 'geoattribute')
  if (is.null(geometry)) {
    if (length(db$geo[[dimension]][[geoatt]]) == 1) {
      geometry <- names(db$geo[[dimension]][[geoatt]])
    } else {
      geometry <- "polygon"
    }
  }
  stopifnot("gometry must be 'point' or 'polygon'." = geometry %in% c("polygon", "point"))
  geo <- db$geo[[dimension]][[geoatt]][[geometry]]
  gt <- sf::st_drop_geometry(geo)
  gt_att <- names(gt)
  data <- unify_facts_and_dimensions(db, dimension, include_nrow_agg)
  data_att <- setdiff(names(data), gt_att)
  data <- data[, c(gt_att, data_att)]
  metadata <- data |>
    dplyr::select(-tidyselect::all_of(c(gt_att, "value")))
  metadata_att <- names(metadata)
  metadata <- metadata |>
    dplyr::group_by_at(dplyr::vars(tidyselect::all_of(metadata_att))) |>
    dplyr::summarise(.groups = "drop")
  n <- as.character(nrow(metadata))
  l <- nchar(n)
  metadata <- tibble::add_column(
    metadata,
    variable = paste0('var_', sprintf(sprintf("%%0%dd", l), 1:as.integer(n))),
    .before = 1
  )

  data <- data |>
    dplyr::inner_join(metadata, by = metadata_att) |>
    dplyr::select(tidyselect::all_of(c(gt_att, "variable", "value")))
  # data$value <- as.numeric(data$value)
  data <- data |>
    tidyr::spread("variable", "value")
  data <- dplyr::full_join(data, geo, by = gt_att)
  data <- sf::st_as_sf(data)

  structure(list(
    geoattribute = gt_att,
    variables = metadata,
    geolayer = data
  ),
  class = "geolayer")
}


#' Get geographic information layer
#'
#' Get the geographic information layer from a `geolayer` object.
#'
#' By default, rows that are NA for all variables are eliminated.
#'
#' @param gl A `geolayer` object.
#' @param keep_all_variables_na A boolean, keep rows with all variables NA.
#'
#' @return A `sf` object.
#'
#' @family query functions
#'
#' @examples
#'
#' gl <- mrs_db_geo |>
#'   as_geolayer()
#'
#' l <- gl |>
#'   get_layer()
#'
#' @export
get_layer <- function(gl, keep_all_variables_na)
  UseMethod("get_layer")

#' @rdname get_layer
#' @export
get_layer.geolayer <- function(gl, keep_all_variables_na = FALSE) {
  variable <- unique(gl$variables$variable)
  if (!keep_all_variables_na) {
    gl$geolayer <- gl$geolayer |>
      dplyr::filter(!dplyr::if_all(tidyselect::all_of(variable), is.na))
  }
  gl$geolayer
}


#' Set geographic layer
#'
#' If for some reason we modify the geographic layer, for example, to add a new
#' calculated variable, we can set that layer to become the new geographic layer
#' of the `geolayer` object using this function.
#'
#' @param gl A `geolayer` object.
#' @param layer A `sf` object.
#'
#' @return A `geolayer` object.
#'
#' @family query functions
#'
#' @examples
#'
#' gl <- mrs_db_geo |>
#'   as_geolayer()
#'
#' l <- gl |>
#'   get_layer()
#'
#' l$tpc_001 <- l$var_002 * 100 / l$var_001
#'
#' gl <- gl |>
#'   set_layer(l)
#'
#' @export
set_layer <- function(gl, layer)
  UseMethod("set_layer")

#' @rdname set_layer
#' @export
set_layer.geolayer <- function(gl, layer) {
  gl$geolayer <- layer
  vars <- names(layer)
  gl$variables <- gl$variables[gl$variables$variable %in% vars,]
  gl
}


#' Get the variables layer
#'
#' The variables layer includes the names and description through various fields
#' of the variables contained in the geolayer.
#'
#' The way to select the variables we want to work with is to filter this layer
#' and subsequently set it as the object's variables layer using the `set_variables()`
#' function.
#'
#' @param gl A `geolayer` object.
#'
#' @return A `tibble` object.
#'
#' @family query functions
#'
#' @examples
#'
#' gl <- mrs_db_geo |>
#'   as_geolayer()
#'
#' v <- gl |>
#'   get_variables()
#'
#' @export
get_variables <- function(gl)
  UseMethod("get_variables")

#' @rdname get_variables
#' @export
get_variables.geolayer <- function(gl) {
  gl$variables
}


#' Set variables layer
#'
#' The variables layer includes the names and description through various fields
#' of the variables contained in the reports.
#'
#' When we set the variables layer, after filtering it, the data layer is also
#' filtered keeping only the variables from the variables layer.
#'
#' By default, rows that are NA for all variables are eliminated.
#'
#' @param gl A `geolayer` object.
#' @param variables A `tibble` object.
#' @param keep_all_variables_na A boolean, keep rows with all variables NA.
#'
#' @return A `sf` object.
#'
#' @family query functions
#'
#' @examples
#'
#' gl <- mrs_db_geo |>
#'   as_geolayer()
#'
#' v <- gl |>
#'   get_variables()
#'
#' v <- v |>
#'   dplyr::filter(year == '1966' | year == '2016')
#'
#' gl_sel <- gl |>
#'   set_variables(v)
#'
#' @export
set_variables <- function(gl, variables, keep_all_variables_na)
  UseMethod("set_variables")

#' @rdname set_variables
#' @export
set_variables.geolayer <- function(gl, variables, keep_all_variables_na = FALSE) {
  original_vars <- gl$variables$variable
  gl$variables <- variables
  new_vars <- gl$variables$variable
  vars_to_delete <- setdiff(original_vars, new_vars)
  layer_vars <- names(gl$geolayer)
  remaining_vars <- setdiff(layer_vars, vars_to_delete)
  gl$geolayer <- gl$geolayer |>
    dplyr::select(tidyselect::all_of(remaining_vars))
  gl$geolayer <- gl |>
    get_layer(keep_all_variables_na)
  gl
}


#' Get variable description
#'
#' Obtain a description of the variables whose name is indicated. If no name is
#' indicated, all are returned.
#'
#' Using the parameter `only_values`, we can obtain only the combination of values
#' or also the combination of names with values.
#'
#' @param gl A `geolayer` object.
#' @param name A string vector.
#' @param only_values A boolean, add names to component values.
#'
#' @return A string vector.
#'
#' @family query functions
#'
#' @examples
#'
#' gl <- mrs_db_geo |>
#'   as_geolayer()
#'
#' vd <- gl |>
#'   get_variable_description()
#'
#' @export
get_variable_description <- function(gl, name, only_values)
  UseMethod("get_variable_description")

#' @rdname get_variable_description
#' @export
get_variable_description.geolayer <- function(gl, name = NULL, only_values = FALSE) {
  if (is.null(name)) {
    name <- gl$variables$variable
  } else {
    name <- name[name %in% gl$variables$variable]
  }
  vars <- gl$variables[gl$variables$variable %in% name, -1]
  vars <- as.data.frame(vars)
  if (!only_values) {
    col_names <- colnames(vars)
    for (j in seq_along(col_names)) {
      vars[, j] <- paste0(col_names[j], " = ", vars[, j])
    }
  }
  des <- apply(vars, 1, paste, collapse=", ")
  names(des) <- name
  des
}


#' Save as `GeoPackage`
#'
#' Save the geolayer (geographic information layer) and the variables layer in a
#' file in `GeoPackage` format to be able to work with other tools.
#'
#' If the file name is not indicated, it defaults to the name of the geovariable.
#'
#' By default, rows that are NA for all variables are eliminated.
#'
#' The `GeoPackage` format only allows defining a maximum of 1998 columns. If the
#' number of variables and columns in the geographic layer exceeds this number,
#' it cannot be saved in this format.
#'
#' @param gl A `geolayer` object.
#' @param dir A string.
#' @param name A string, file name.
#' @param keep_all_variables_na A boolean, keep rows with all variables NA.
#'
#' @return A string, file name.
#'
#' @family query functions
#'
#' @examples
#'
#' gl <- mrs_db_geo |>
#'   as_geolayer()
#'
#' f <- gl |>
#'   as_GeoPackage(dir = tempdir())
#'
#' @export
as_GeoPackage <- function(gl, dir, name, keep_all_variables_na)
  UseMethod("as_GeoPackage")

#' @rdname as_GeoPackage
#' @export
as_GeoPackage.geolayer <- function(gl, dir = NULL, name = NULL, keep_all_variables_na = FALSE) {
  stopifnot(
    "The maximum number of columns supported by this format (1998 cols.) has been exceeded." = ncol(gl$geolayer) < 1999
  )
  if (!is.null(dir)) {
    dir <- name_with_nexus(dir)
  }
  if (is.null(name)) {
    name <- paste(gl$geoattribute, collapse = "_")
  }
  name <- tools::file_path_sans_ext(name)
  file <- paste0(dir, name, '.gpkg')

  layer <- gl |>
    get_layer(keep_all_variables_na)

  sf::st_write(
    obj = layer,
    dsn = file,
    layer = "geolayer",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = gl$variables,
    dsn = file,
    layer = "variables",
    append = FALSE,
    quiet = TRUE
  )
  file
}



#' Do all fact tables have the same granularity?
#'
#' @param db A `star_database` object.
#' @param names A vector of strings, fact names.
#'
#' @return A boolean.
#'
#' @keywords internal
same_granularity_facts <- function(db, names) {
  fk <- db$facts[[names[1]]]$surrogate_keys
  lfk <- length(fk)
  for (f in names) {
    com <- intersect(db$facts[[f]]$surrogate_keys, fk)
    if (length(com) != length(db$facts[[f]]$surrogate_keys) | length(com) != lfk) {
      stop("The fact tables do not have the same granularity.")
    }
  }
  TRUE
}

#' Unify facts and dimensions in a flat table
#'
#' @param db A `star_database` object.
#' @param dimension A vector of strings, dimension names.
#' @param include_nrow_agg A boolean.
#'
#' @return A `tibble`.
#'
#' @keywords internal
unify_facts_and_dimensions <- function(db, dimension, include_nrow_agg) {
  facts <- NULL
  for (f in names(db$facts)) {
    if (dimension %in% db$facts[[f]]$dim_int_names) {
      facts <- c(facts, f)
    }
  }
  stopifnot("There are no facts related to the selected dimension." = length(facts) > 0)
  same_granularity_facts(db, facts)
  if (length(facts) > 1) {
    include_fact_name <- TRUE
  } else {
    include_fact_name <- FALSE
  }
  table <- NULL
  for (f in facts) {
    t <- db$facts[[f]]$table
    begin <- length(db$facts[[f]]$surrogate_keys) + 1
    lnt <- length(names(db$facts[[f]]$table))
    end <- lnt
    if (!include_nrow_agg) {
      end <- end - 1
    }
    if (begin > end) {
      end <- begin
    }
    if (end < lnt) {
      t <- t[, -lnt]
    }
    v <- names(t)[begin:end]
    t <-  tidyr::gather(t, "measure", "value", tidyselect::all_of(v))
    if (include_fact_name) {
      t <-
        tibble::add_column(t, facts = db$facts[[f]]$name, .before = 1)
    }
    table <- rbind(table, t)
  }
  surrogate_keys <- db$facts[[f]]$surrogate_keys
  measures <- setdiff(names(table), surrogate_keys)
  # geodimension first
  dim_names <- setdiff(db$facts[[f]]$dim_int_names, dimension)
  dim_names <- c(dimension, dim_names)
  for (d in dim_names) {
    key <- db$dimensions[[d]]$surrogate_key
    table <- table |>
      dplyr::inner_join(db$dimensions[[d]]$table, by = key, suffix = c("", paste0('_',d)))
  }
  att_dim <- setdiff(names(table), c(surrogate_keys, measures))
  table[, c(att_dim, measures)]
}


