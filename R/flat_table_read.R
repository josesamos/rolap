#' Import flat table file
#'
#' Reads a text file and creates a `flat_table` object. The file is expected to
#' contain a flat table whose first row contains the name of the columns. All
#' columns are considered to be of type String.
#'
#' When multiple files are handled, the file name may contain information
#' associated with the flat table, it could be the table page information if the
#' name of a new field in which to store it is indicated in the page parameter.
#'
#' We can also indicate the value that is used in the data with undefined values.
#'
#' @param name A string, flat table name.
#' @param file A string, name of a text file.
#' @param sep Column separator character.
#' @param page A string, name of the new field in which to include the name of
#' the file.
#' @param unknown_value A string, value used to replace empty and NA values in
#' attributes.
#'
#' @return A `flat_table` object.
#'
#' @family flat table definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' file <-
#'   system.file("extdata/mrs",
#'               "mrs_122_us_cities_1962_2016_new.csv",
#'               package = "rolap")
#'
#' ft <- read_flat_table_file('mrs_new', file)
#'
#' @export
read_flat_table_file <-
  function(name,
           file,
           sep = ',',
           page = NULL,
           unknown_value = NULL) {
    ft <- readr::read_delim(
      file,
      delim = sep,
      id = page,
      col_types = readr::cols(.default = readr::col_character())
    )
    flat_table(name, ft, unknown_value)
  }

#' Import all flat table files in a folder
#'
#' Reads all text files in a folder and creates a `flat_table` object. Each file
#' is expected to contain a flat table, all with the same structure, whose first
#' row contains the name of the columns. All columns are considered to be of type
#' String.
#'
#' When multiple files are handled, the file name may contain information
#' associated with the flat table, it could be the table page information if the
#' name of a new field in which to store it is indicated.
#'
#' We can also indicate the value that is used in the data with undefined values.
#'
#' In some situations all the files have the same structure but the column names
#' may change slightly. In these cases it can be useful to transform the names to
#' snake case or consider for all the files the names of the columns of the first
#' one. These operations can be indicated by the corresponding parameters.
#'
#' @param folder A string, folder name.
#' @inheritParams read_flat_table_file
#' @param same_columns A boolean, indicates whether all tables have the same
#' columns in the same order.
#' @param snake_case A boolean, indicates if we want to transform the names of
#' the columns to snake case.
#'
#' @return A `flat_table` object.
#'
#' @family flat table definition functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' file <- system.file("extdata/mrs", package = "rolap")
#'
#' ft <- read_flat_table_folder('mrs_new', file)
#'
#' @export
read_flat_table_folder <-
  function (name,
            folder,
            sep = ',',
            page = NULL,
            unknown_value = NULL,
            same_columns = FALSE,
            snake_case = FALSE) {
    lf <- list.files(path = folder, full.names = TRUE)
    lft <- purrr::map(
      lf,
      readr::read_delim,
      id = page,
      name_repair = "minimal",
      delim = sep,
      col_types = readr::cols(.default = readr::col_character())
    )
    if (same_columns) {
      names <- names(lft[[1]])
      for (i in seq_along(lft)) {
        names(lft[[i]]) <- names
      }
    }
    if (snake_case) {
      for (i in seq_along(lft)) {
        names(lft[[i]]) <- snakecase::to_snake_case(names(lft[[i]]))
      }
    }
    for (i in seq_along(lft)) {
      names <- names(lft[[i]])
      w <- which(names == "")
      names[w] <- paste0("_c_", w, "_")
      names(lft[[i]]) <- names
    }
    flat_table(name, dplyr::bind_rows(lft),
               unknown_value)
  }
