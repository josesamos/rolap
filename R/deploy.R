#' Deploy a star database in a relational database
#'
#' To deploy the star database, we must indicate a name for the deployment, a
#' connection function and a disconnection function from the database. If it is
#' the first deployment, we must also indicate the name of a local file where the
#' star database will be stored.
#'
#' If the disconnection function consists only of calling `DBI::dbDisconnect(con)`,
#' there is no need to indicate it, it is taken by default.
#'
#' As a result, it exports the tables from the star database to the connection
#' database and from now on will keep them updated with each periodic refresh.
#' Additionally, it will also keep a copy of the star database updated on file,
#' which can be used when needed.
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
#' mrs_rdb_file <- tempfile("mrs", fileext = ".rdb")
#' mrs_sqlite_file <- tempfile("mrs", fileext = ".sqlite")
#'
#' mrs_sqlite_connect <- function() {
#'   DBI::dbConnect(RSQLite::SQLite(),
#'                  dbname = mrs_sqlite_file)
#' }
#'
#' mrs_db <- mrs_db |>
#'   deploy(
#'     name = "mrs",
#'     connect = mrs_sqlite_connect,
#'     file = mrs_rdb_file
#'   )
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
      file <- tools::file_path_sans_ext(file)
      file <- paste0(file, '.rds')
      db$deploy$file <- file
    }
    if (is.null(disconnect)) {
      disconnect <- default_disconnect
    }
    name <- snakecase::to_snake_case(name)
    database_names <- names(db$deploy$databases)
    if (!(name %in% database_names)) {
      database <- vector("list", length = 3)
      names(database) <- c('connect', 'disconnect', 'pending_sql')
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
#' Disconnect function that is used if no other is indicated in the parameter of
#' the deploy function.
#'
#' @param con A `DBI::DBIConnection` object.
#'
#' @return TRUE, invisibly.
#'
#' @keywords internal
default_disconnect <- function(con) {
  DBI::dbDisconnect(con)
}


#' Cancel deployment
#'
#'
#' @param db A `star_database` object.
#' @param name A string, name of the deployment.
#'
#' @return A `star_database` object.
#'
#' @family star database refresh functions
#'
#' @examples
#'
#' mrs_rdb_file <- tempfile("mrs", fileext = ".rdb")
#' mrs_sqlite_file <- tempfile("mrs", fileext = ".sqlite")
#'
#' mrs_sqlite_connect <- function() {
#'   DBI::dbConnect(RSQLite::SQLite(),
#'                  dbname = mrs_sqlite_file)
#' }
#'
#' mrs_db <- mrs_db |>
#'   deploy(
#'     name = "mrs",
#'     connect = mrs_sqlite_connect,
#'     file = mrs_rdb_file
#'   )
#'
#' mrs_db <- mrs_db |>
#'   cancel_deployment(name = "mrs")
#'
#' @export
cancel_deployment <- function(db, name) UseMethod("cancel_deployment")

#' @rdname cancel_deployment
#'
#' @export
cancel_deployment.star_database <- function(db, name) {
  stopifnot("Missing deployment name." = !is.null(name))
  name <- snakecase::to_snake_case(name)
  database_names <- names(db$deploy$databases)
  stopifnot("The name does not correspond to any deployment name." = name %in% database_names)
  i <- which(database_names == name)
  if (length(i) > 0) {
    db$deploy$databases <- db$deploy$databases[-i]
  }
  db
}


#' Refresh deployments
#'
#' Generate sql code for the first refresh operation.
#'
#' @param db A `star_database` object.
#' @param internal A boolean.
#'
#' @return A `star_database` object.
#'
#' @keywords internal
refresh_deployments <- function(db, internal) {
  if (length(db$deploy$databases) > 0) {
    sql <- NULL
    for (r in seq_along(db$refresh)) {
      sql <- c(sql, generate_refresh_sql(db$refresh[[r]]))
    }
    for (d in seq_along(db$deploy$databases)) {
      pending_sql <- c(db$deploy$databases[[d]]$pending_sql, sql)
      db$deploy$databases[[d]]$pending_sql <- pending_sql
      # control errors begin
      con <- db$deploy$databases[[d]]$connect()
      for (s in pending_sql) {
        res <- DBI::dbExecute(con, s)
      }
      db$deploy$databases[[d]]$disconnect(con)
      # end
      db$deploy$databases[[d]]$pending_sql <- NULL
    }
  }

  if (length(internal) == 0) {
    db$refresh <- list()
  } else {
    if (internal != 'DONTDELETE') {
      db$refresh <- list()
    }
  }
  if (length(db$deploy$file) > 0) {
    saveRDS(db, file = db$deploy$file)
  }
  db
}


#' Load star_database (from a RDS file)
#'
#'
#' @param file A string, name of the file that stores the object.
#'
#' @return A `star_database` object.
#'
#' @family star database refresh functions
#'
#' @examples
#'
#' mrs_rdb_file <- tempfile("mrs", fileext = ".rdb")
#' mrs_sqlite_file <- tempfile("mrs", fileext = ".sqlite")
#'
#' mrs_sqlite_connect <- function() {
#'   DBI::dbConnect(RSQLite::SQLite(),
#'                  dbname = mrs_sqlite_file)
#' }
#'
#' mrs_db <- mrs_db |>
#'   deploy(
#'     name = "mrs",
#'     connect = mrs_sqlite_connect,
#'     file = mrs_rdb_file
#'   )
#'
#' mrs_db2 <- load_star_database(mrs_rdb_file)
#'
#' @export
load_star_database <- function(file) {
  file <- tools::file_path_sans_ext(file)
  file <- paste0(file, '.rds')
  db <- readRDS(file)
  db
}
