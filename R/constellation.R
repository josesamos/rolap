#' `constellation` S3 class
#'
#' Creates a `constellation` object from a list of `star_database` objects. All
#' dimensions with the same name in the star schemas have to be conformable
#' (share the same structure, even though they have different instances).
#'
#' @param name A string.
#' @param stars A list of `star_database` objects.
#'
#' @return A `constellation` object.
#'
#' @family star schema and constellation definition functions
#'
#' @examples
#'
#'
#' @export
constellation <- function(name = NULL, stars = NULL) {
  stopifnot(!is.null(name))
  stopifnot(length(stars) > 1)
  names <- c()
  facts = vector("list", length = length(stars))
  for (s in seq_along(stars)) {
    stopifnot(class(stars[[s]]) == "star_database")
    names <- c(names, names(stars[[s]]$instance$facts))
    facts[s] <- stars[[s]]$instance$facts
  }
  names <- unique(names)
  stopifnot(length(stars) == length(names))
  names(facts) <- names

  dimensions <- stars[[1]]$instance$dimensions
  dimension_names <- names(dimensions)
  for (s in 2:length(stars)) {
    for (d in seq_along(stars[[s]]$instance$dimensions)) {
      dim <- stars[[s]]$instance$dimensions[d]
      if (!(names(dim) %in% dimension_names)) {
        dimension_names <- c(dimension_names, names(dim))
        dimensions[length(dimensions) + 1] <- dim
        names(dimensions) <- dimension_names
      } else {
        # hay que unificar dimensiones
      }
    }

  }

  structure(list(name = name, facts = facts, dimensions = dimensions), class = "constellation")
}
