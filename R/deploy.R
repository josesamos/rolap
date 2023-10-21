#' Deploy a star database in a relational database
#'
#'
#' @param db A `star_database` object.
#' @param name A string, name of the deployment.
#' @param connect A function that returns a `DBI::DBIConnection` object.
#' @param disconnect A function that receives a `DBI::DBIConnection` object as a
#' parameter and close the connection.
#' @param file A string, name of the file to store the object.
#'
#' @return A `star_database` object.
#'
#' @family star database refresh functions
#'
#' @examples
#'
#' f1 <-
#'   flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
#'                                       ft_cause_rpd$WEEK != '4',]) |>
#'   as_star_database(mrs_cause_schema_rpd) |>
#'   role_playing_dimension(rpd = "When",
#'                          roles = c("When Available", "When Received"))
#' f2 <- flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
#'                                            ft_cause_rpd$WEEK != '2',])
#' f2 <- f2 |>
#'   update_according_to(f1)
#'
#' f1 <- f1 |>
#'   incremental_refresh(f2)
#'
#' @export
deploy <- function(db, name, connect, disconnect, file)
  UseMethod("deploy")

#' @rdname deploy
#'
#' @export
deploy.star_database <-
  function(db, name, connect, disconnect = NULL, file = NULL) {
    stopifnot("Missing deployment name." = !is.null(name))
    if (length(db$deploy) == 0 & is.null(file)) {
      stop("Missing deployment file.")
    }
    if (length(db$deploy) == 0) {
      db$deploy <- vector("list", length = 2)
      names(db$deploy) <- c('file', 'databases')
      db$deploy$file <- file
    }
    if (is.null(disconnect)) {
      disconnect <- default_disconnect
    }
    name <- snakecase::to_snake_case(name)
    database_names <- names(db$deploy$databases)
    if (!(name %in% database_names)) {
      database <- vector("list", length = 2)
      names(database) <- c('connect', 'disconnect')
      db$deploy$databases <- c(db$deploy$databases, list(database))
      names(db$deploy$databases) <- c(names(db$deploy$databases), name)
    }
    db$deploy$databases[[name]]$connect <- connect
    db$deploy$databases[[name]]$disconnect <- disconnect
    saveRDS(db, file = db$deploy$file)
    con <- db$deploy$databases[[name]]$connect()
    res <- as_rdb(db, con, overwrite = TRUE)
    db$deploy$databases[[name]]$disconnect(con)
    db
  }


#' Default disconnect function
#'
#' Generate sql code for the first refresh operation.
#'
#' @param con A `DBI::DBIConnection` object.
#'
#' @return TRUE, invisibly.
#'
#' @keywords internal
default_disconnect <- function(con) {
  DBI::dbDisconnect(con)
}



