

#' Get an `geolayer` object
#'
#' Once we have selected the topics that interest us and, possibly also the reports
#' or subreports, we obtain an `geolayer` object with which we can represent
#' or export the geographic layer along with the data of interest more easily.
#'
#' @param act An `acs_5yr_topic` object.
#'
#' @return An `geolayer` object.
#'
#' @family data exploitation and export functions
#'
#' @examples
#'
#' act <- anrc_2021_x01 |>
#'   select_report(report = "B01002-Median Age By Sex")
#'
#' geo <- act |>
#'   as_geolayer()
#'
#' @export
as_geolayer <- function(act)
  UseMethod("as_geolayer")

#' @rdname as_geolayer
#' @export
as_geolayer.acs_5yr_topic <- function(act) {
  data <- act$data
  data <-
    tidyr::gather(data, "measure", "value", (length(names(data)) - 1):length(names(data)))
  data$Short_Name[data$measure == 'estimate'] <- gsub(
    pattern = '_',
    replacement = 'e',
    data$Short_Name[data$measure == 'estimate'],
    fixed = TRUE
  )
  data$Short_Name[data$measure == 'margin_of_error'] <- gsub(
    pattern = '_',
    replacement = 'm',
    data$Short_Name[data$measure == 'margin_of_error'],
    fixed = TRUE
  )

  metadata <- data |>
    dplyr::select(-tidyselect::all_of(c("GEOID", "value")))
  metadata <- transform_metadata_rest(metadata)
  n <- as.character(nrow(metadata))
  l <- nchar(n)
  metadata <- tibble::add_column(
    metadata,
    variable = paste0('V', sprintf(sprintf("%%0%dd", l), 1:as.integer(n))),
    .before = 1
  )

  data <- data |>
    dplyr::inner_join(metadata, by = c('year', 'Short_Name'), suffix = c("", ".y")) |>
    dplyr::select(tidyselect::all_of(c("GEOID", "variable", "value")))
  names(data) <- c("GEOID_Data", "variable", "value")
  data$value <- as.numeric(data$value)
  data <- data |>
    tidyr::spread("variable", "value")
  data <- dplyr::inner_join(act$geo, data, by = "GEOID_Data")

  area <- names(act$area)
  area_code <- act$area
  year <- act$years
  file <- act$files
  topic_code <- act$topic[1]
  topic <- names(topic_code)
  origin <-
    data.frame(
      area,
      area_code,
      year,
      file,
      topic,
      topic_code,
      row.names = 1:length(year),
      stringsAsFactors = FALSE
    )
  if (length(act$topic) > 1) {
    for (i in 2:length(act$topic)) {
      topic_code <- act$topic[i]
      topic <- names(topic_code)
      origin <-
        rbind(
          origin,
          data.frame(
            area,
            area_code,
            year,
            file,
            topic,
            topic_code,
            row.names = 1:length(year),
            stringsAsFactors = FALSE
          )
        )
    }
  }
  structure(list(
    origin = origin,
    metadata = metadata,
    data = data
  ),
  class = "geolayer")
}



#' @rdname get_geo_layer
#' @export
get_geo_layer.geolayer <- function(glc) {
  glc$data
}


#' Get the metadata layer
#'
#' The metadata layer includes the names and description through various fields
#' of the variables contained in the reports.
#'
#' The way to select the variables we want to work with is to filter this layer
#' and subsequently set it as the object's metadata layer using the `set_metadata()`
#' function.
#'
#' @param geo An `geolayer` object.
#'
#' @return A `tibble` object.
#'
#' @family data exploitation and export functions
#'
#' @examples
#'
#' act <- anrc_2021_x01 |>
#'   select_report(report = "B01002-Median Age By Sex")
#'
#' geo <- act |>
#'   as_geolayer()
#'
#' metadata <- geo |>
#'   get_metadata()
#'
#' @export
get_metadata <- function(geo)
  UseMethod("get_metadata")

#' @rdname get_metadata
#' @export
get_metadata.geolayer <- function(geo) {
  geo$metadata
}

#' Set metadata layer
#'
#' The metadata layer includes the names and description through various fields
#' of the variables contained in the reports.
#'
#' When we set the metadata layer, after filtering it, the data layer is also
#' filtered keeping only the variables from the metadata layer.
#'
#' @param geo An `geolayer` object.
#' @param metadata A `tibble` object.
#'
#' @return A `sf` object.
#'
#' @family data exploitation and export functions
#'
#' @examples
#'
#' act <- anrc_2021_x01 |>
#'   select_report(report = "B01002-Median Age By Sex")
#'
#' geo <- act |>
#'   as_geolayer()
#'
#' metadata <- geo |>
#'   get_metadata()
#'
#' metadata <- dplyr::filter(metadata, item2 == "Female")
#'
#' geo2 <- geo |>
#'   set_metadata(metadata)
#'
#' @export
set_metadata <- function(geo, metadata)
  UseMethod("set_metadata")

#' @rdname set_metadata
#' @export
set_metadata.geolayer <- function(geo, metadata) {
  geo$metadata <- metadata
  variable <- unique(metadata$variable)
  names <- names(geo$data)
  i <- grep('GEOID_Data', names, fixed = TRUE)
  names <- c(names[1:i], variable)
  geo$data <- geo$data[, names]
  geo
}


#' Save as `GeoPackage`
#'
#' Save the data layer (geographic information layer), the metadata layer and the
#' data source description layer in a file in `GeoPackage` format to be able to
#' work with other tools.
#'
#' The `GeoPackage` format only allows defining a maximum of 1998 columns. If the
#' number of variables and columns in the geographic layer exceeds this number,
#' it cannot be saved in this format.
#'
#' @param geo An `geolayer` object.
#' @param dir A string.
#' @param name A string, file name.
#'
#' @return A string, file name.
#'
#' @family data exploitation and export functions
#'
#' @examples
#'
#' act <- anrc_2021_x01 |>
#'   select_report(report = "B01002-Median Age By Sex")
#'
#' geo <- act |>
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
    "The maximum number of columns supported by this format (1998 cols.) has been exceeded." = ncol(geo$data) < 1999
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
    obj = geo$data,
    dsn = file,
    layer = "data",
    append = FALSE,
    quiet = TRUE
  )
  sf::st_write(
    obj = geo$metadata,
    dsn = file,
    layer = "metadata",
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

