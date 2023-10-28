#' Multiple value key
#'
#' Gets the keys that have multiple values associated with them. The first field
#' in the table is the key, the rest of fields are the values.
#'
#' If a name is indicated in the `col_as_vector` parameter, it includes a column
#' with the data in vector form to be used in other functions.
#'
#' @param tb A `tibble`.
#' @param col_as_vector A string, name of the column to include a vector of values.
#'
#' @return A `tibble`.
#'
#' @family utility functions
#'
#' @examples
#'
#' tb <- unique(ft[, c('WEEK', 'Week Ending Date')])
#' mvk <- multiple_value_key(tb)
#'
#' @export
multiple_value_key <- function(tb, col_as_vector = NULL) {
  t <- table(tb[, 1])
  v <- names(t[t > 1])

  r <- tb[tb[, 1][[1]] %in% v, ]
  key <- r[, 1][[1]]
  names(key) <- 1:length(key)
  key <- sort(key)
  r <- r[names(key), ]
  if (!is.null(col_as_vector)) {
    r <- add_dput_column(r, col_as_vector)
  }
  r
}

