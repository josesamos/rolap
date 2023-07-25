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
  dim_names <- c()
  facts = vector("list", length = length(stars))
  for (s in seq_along(stars)) {
    stopifnot(class(stars[[s]]) == "star_database")
    names <- c(names, names(stars[[s]]$instance$facts))
    dim_names <- c(dim_names, names(stars[[s]]$instance$dimensions))
    facts[s] <- stars[[s]]$instance$facts
  }
  names <- unique(names)
  stopifnot(length(stars) == length(names))
  names(facts) <- names

  # frequency of dimensions
  dim_freq <- table(dim_names)
  dimensions = vector("list", length = length(dim_freq))
  names(dimensions) <- names(dim_freq)
  for (s in seq_along(stars)) {
    for (d in seq_along(stars[[s]]$instance$dimensions)) {
      dim <- stars[[s]]$instance$dimensions[d]
      if (dim_freq[names(dim)] == 1) {
        dimensions[names(dim)] <- dim
      } else {
        # hay que unificar dimensiones
      }
    }

  }

  structure(list(name = name, facts = facts, dimensions = dimensions), class = "constellation")
}
