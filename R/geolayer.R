

#' Get a `geolayer` object
#'
#' From a `star_database` with at least one geoattribute, we obtain a `geolayer`
#' object that allows us to select the data to obtain a vector layer with
#' geographic information.
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
#' geo <- db |>
#'   as_geolayer()
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
                                      geometry = "polygon",
                                      include_nrow_agg = FALSE) {
  if (is.null(dimension)) {
    if (length(db$geo) == 1) {
      dimension <- names(db$geo)
    } else {
      stop("A dimension name must be indicated since there is more than one.")
    }
  }
  if (is.null(attribute)) {
    if (length(db$geo[[dimension]]) == 1) {
      attribute <- names(db$geo[[dimension]])
    } else {
      stop("An attribute name must be indicated since there is more than one.")
    }
  }
  geoatt <- get_geoattribute_name(attribute)
  validate_names(names(db$geo), dimension, concept = 'geodimension')
  validate_names(names(db$geo[[dimension]]), geoatt, concept = 'geoattribute')
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
  data <- dplyr::left_join(data, geo, by = gt_att)
  data <- sf::st_as_sf(data)

  structure(list(
    variables = metadata,
    geolayer = data
  ),
  class = "geolayer")
}


#' Get geographic layer
#'
#' Get the geographic layer.
#'
#' @param geo A `geolayer` object.
#'
#' @return A `sf` object.
#'
#' @family query functions
#'
#' @examples
#'
#'   get_geolayer()
#'
#' @export
get_geolayer <- function(geo)
  UseMethod("get_geolayer")

#' @rdname get_geolayer
#' @export
get_geolayer.geolayer <- function(geo) {
  geo$geolayer
}


#' Get the variables layer
#'
#' The variables layer includes the names and description through various fields
#' of the variables contained in the reports.
#'
#' The way to select the variables we want to work with is to filter this layer
#' and subsequently set it as the object's variables layer using the `set_variables()`
#' function.
#'
#' @param geo A `geolayer` object.
#'
#' @return A `tibble` object.
#'
#' @family query functions
#'
#' @examples
#'
#' db <- anrc_2021_x01 |>
#'   select_report(report = "B01002-Median Age By Sex")
#'
#' geo <- db |>
#'   as_geolayer()
#'
#' variables <- geo |>
#'   get_variables()
#'
#' @export
get_variables <- function(geo)
  UseMethod("get_variables")

#' @rdname get_variables
#' @export
get_variables.geolayer <- function(geo) {
  geo$variables
}

#' Set variables layer
#'
#' The variables layer includes the names and description through various fields
#' of the variables contained in the reports.
#'
#' When we set the variables layer, after filtering it, the data layer is also
#' filtered keeping only the variables from the variables layer.
#'
#' @param geo A `geolayer` object.
#' @param variables A `tibble` object.
#'
#' @return A `sf` object.
#'
#' @family query functions
#'
#' @examples
#'
#' db <- anrc_2021_x01 |>
#'   select_report(report = "B01002-Median Age By Sex")
#'
#' geo <- db |>
#'   as_geolayer()
#'
#' variables <- geo |>
#'   get_variables()
#'
#' variables <- dplyr::filter(variables, item2 == "Female")
#'
#' geo2 <- geo |>
#'   set_variables(variables)
#'
#' @export
set_variables <- function(geo, variables)
  UseMethod("set_variables")

#' @rdname set_variables
#' @export
set_variables.geolayer <- function(geo, variables) {
  geo$variables <- variables
  variable <- unique(variables$variable)
  names <- names(geo$geolayer)
  i <- grep('GEOID_Data', names, fixed = TRUE)
  names <- c(names[1:i], variable)
  geo$geolayer <- geo$geolayer[, names]
  geo
}


#' Save as `GeoPackage`
#'
#' Save the data layer (geographic information layer), the variables layer and the
#' data source description layer in a file in `GeoPackage` format to be able to
#' work with other tools.
#'
#' The `GeoPackage` format only allows defining a maximum of 1998 columns. If the
#' number of variables and columns in the geographic layer exceeds this number,
#' it cannot be saved in this format.
#'
#' @param geo A `geolayer` object.
#' @param dir A string.
#' @param name A string, file name.
#'
#' @return A string, file name.
#'
#' @family query functions
#'
#' @examples
#'
#' db <- anrc_2021_x01 |>
#'   select_report(report = "B01002-Median Age By Sex")
#'
#' geo <- db |>
#'   as_geolayer()
#'
#' dir <- tempdir()
#' file <- geo |>
#'   as_GeoPackage(dir)
#'
#' @export
as_GeoPackage <- function(geo, dir, name)
  UseMethod("as_GeoPackage")

#' @rdname as_GeoPackage
#' @export
as_GeoPackage.geolayer <- function(geo, dir = NULL, name = NULL) {
  stopifnot(
    "The maximum number of columns supported by this format (1998 cols.) has been exceeded." = ncol(geo$geolayer) < 1999
  )
  if (is.null(name)) {
    name <- geo$origin[1, "area_code"]
  }
  if (!is.null(dir)) {
    dir <- name_with_nexus(dir)
  }
  name <- tools::file_path_sans_ext(name)
  file <- paste0(dir, name, '.gpkg')

  sf::st_write(
    obj = geo$geolayer,
    dsn = file,
    layer = "data",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = geo$variables,
    dsn = file,
    layer = "variables",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = geo$origin,
    dsn = file,
    layer = "origin",
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
    i <- begin:end
    t <-  tidyr::gather(t, "measure", "value", i)
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


