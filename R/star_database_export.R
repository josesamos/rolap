
#' Generate a list of tibbles with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a list of tibbles, as this function does.
#'
#' @param db A `star_database` object.
#'
#' @return A list of `tibble`
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' tl1 <- db1 |>
#'   as_tibble_list()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", db1, db2)
#' tl <- ct |>
#'   as_tibble_list()
#'
#' @export
as_tibble_list <- function(db) UseMethod("as_tibble_list")

#' @rdname as_tibble_list
#'
#' @export
as_tibble_list.star_database <- function(db) {
  l <- NULL
  lnames <- NULL
  for (d in names(db$dimensions)) {
    l <- c(l, list(db$dimensions[[d]]$table))
    lnames <- c(lnames, d)
  }
  for (f in names(db$facts)) {
    l <- c(l, list(db$facts[[f]]$table))
    lnames <- c(lnames, f)
  }
  names(l) <- lnames
  l
}


#' Generate a `dm` class with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a `dm` class, as this function does, in this way it can be
#' saved directly in a DBMS.
#'
#' @param db A `star_database` object.
#' @param pk_facts A boolean, include primary key in fact tables.
#' @param fk A boolean, include foreign key in fact tables.
#'
#' @return A `dm` object.
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' dm1 <- db1 |>
#'   as_dm_class()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", db1, db2)
#' dm <- ct |>
#'   as_dm_class()
#'
#' @export
as_dm_class <- function(db, pk_facts, fk) UseMethod("as_dm_class")

#' @rdname as_dm_class
#'
#' @export
as_dm_class.star_database <- function(db, pk_facts = TRUE, fk = TRUE) {
  c <- dm::dm()
  for (d in names(db$dimensions)) {
    c <- c |>
      dm::dm(!!d := db$dimensions[[d]]$table) |>
      dm::dm_add_pk(!!d, !!db$dimensions[[d]]$surrogate_key) |>
      dm::dm_set_colors(darkgreen = !!d)
  }
  for (f in names(db$facts)) {
    c <- c |>
      dm::dm(!!f := db$facts[[f]]$table) |>
      dm::dm_set_colors(darkblue = !!f)
    if (fk) {
      for (s in db$facts[[f]]$surrogate_keys) {
        t <- gsub("_key", "", s)
        c <- c |>
          dm::dm_add_fk(!!f, !!s, !!t)
      }
    }
    if (pk_facts) {
      c <- c |>
        dm::dm_add_pk(!!f, !!db$facts[[f]]$surrogate_keys)
    }
  }
  c
}



#' Generate a list of tibbles of flat tables
#'
#' Allows you to transform a star database into a flat table. If we have a
#' constellation, it returns a list of flat tables.
#'
#' @param db A `star_database` object.
#'
#' @return A list of `tibble`
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' tl1 <- db1 |>
#'   as_single_tibble_list()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", db1, db2)
#' tl <- ct |>
#'   as_single_tibble_list()
#'
#' @export
as_single_tibble_list <- function(db) UseMethod("as_single_tibble_list")

#' @rdname as_single_tibble_list
#'
#' @export
as_single_tibble_list.star_database <- function(db) {
  res <- vector("list", length = length(db$facts))
  names(res) <- names(db$facts)
  for (f in names(db$facts)) {
    res[[f]] <- db$facts[[f]]$table
    for (d in db$facts[[f]]$dim_int_names) {
      key <- db$dimensions[[d]]$surrogate_key
      res[[f]] <-
        dplyr::inner_join(res[[f]], db$dimensions[[d]]$table, by = key, suffix = c("", paste0('_',d)))
    }
    measures <- names(db$facts[[f]]$agg)
    attributes <- setdiff(names(res[[f]]), c(measures, db$facts[[f]]$surrogate_keys))
    res[[f]] <- res[[f]][, c(attributes, measures)]
  }
  res
}


#' Generate tables in a relational database
#'
#' Given a connection to a relational database, it stores the facts and
#' dimensions in the form of tables. Tables can be overwritten.
#'
#' @param db A `star_database` object.
#' @param con A `DBI::DBIConnection` object.
#' @param overwrite A boolean, allow overwriting tables in the database.
#'
#' @return A list of `tibble`
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' my_db <- DBI::dbConnect(RSQLite::SQLite())
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' db <- db |>
#'   as_rdb(my_db)
#'
#' DBI::dbDisconnect(my_db)
#'
#' @export
as_rdb <- function(db, con, overwrite) UseMethod("as_rdb")

#' @rdname as_rdb
#'
#' @export
as_rdb.star_database <- function(db, con, overwrite = FALSE) {
  if (overwrite) {
    tables <- DBI::dbListTables(con)
    dimensions <- get_dimension_names(db)
    facts <- get_fact_names(db)
    dimensions <- intersect(tables, dimensions)
    facts <- intersect(tables, facts)
    for (f in facts) {
      DBI::dbRemoveTable(con, f)
    }
    for (d in dimensions) {
      DBI::dbRemoveTable(con, d)
    }
  }
  db_dm <- as_dm_class(db)
  dm::copy_dm_to(con, db_dm, temporary = FALSE)
  db
}


#' Draw tables
#'
#' Draw the tables of the ROLAP star diagrams.
#'
#' @param db A `star_database` object.
#'
#' @return A `star_database` object.
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' \donttest{
#' db <- db |>
#'   draw_tables()
#' }
#'
#' @export
draw_tables <- function(db) UseMethod("draw_tables")

#' @rdname draw_tables
#'
#' @export
draw_tables.star_database <- function(db) {
  db_dm <- db |>
    as_dm_class(pk_facts = FALSE)
  db_dm |>
    dm::dm_draw(rankdir = "LR", view_type = "all")
  db
}


#' Generate a xlsx file with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as a xlsx file, as this function does.
#'
#' @param db A `star_database` object.
#' @param file A string, name of a file.
#'
#' @return A string, name of a file.
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' tl1 <- db1 |>
#'   as_xlsx_file()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", db1, db2)
#' tl <- ct |>
#'   as_xlsx_file(file = tempfile())
#'
#' @export
as_xlsx_file <- function(db, file) UseMethod("as_xlsx_file")

#' @rdname as_xlsx_file
#'
#' @export
as_xlsx_file.star_database <- function(db, file = NULL) {
  if (is.null(file)) {
    file <- tempfile()
  }
  file <- tools::file_path_sans_ext(file)
  file <- paste0(file, '.xlsx')

  l <- as_tibble_list(db)
  names <- names(l)
  xlsx::write.xlsx(
    as.data.frame(l[[1]]),
    file = file,
    sheetName = names[1],
    row.names = FALSE,
    showNA = FALSE
  )
  if (length(l) > 1) {
    for (i in 2:length(l)) {
      xlsx::write.xlsx(
        as.data.frame(l[[i]]),
        file = file,
        sheetName = names[i],
        append = TRUE,
        row.names = FALSE,
        showNA = FALSE
      )
    }
  }
  file
}


#' Generate csv files with fact and dimension tables
#'
#' To port databases to other work environments it is useful to be able to
#' export them as csv files, as this function does.
#'
#' @param db A `star_database` object.
#' @param dir A string, name of a dir.
#' @param type An integer, 1: uses "." for the decimal point and a comma for the
#' separator; 2: uses a comma for the decimal point and a semicolon for the
#' separator.
#'
#' @return A string, name of a dir.
#'
#' @family star database exportation functions
#' @seealso \code{\link{star_database}}
#'
#' @examples
#'
#' db1 <- star_database(mrs_cause_schema, ft_num) |>
#'   snake_case()
#' tl1 <- db1 |>
#'   as_csv_files()
#'
#' db2 <- star_database(mrs_age_schema, ft_age) |>
#'   snake_case()
#'
#' ct <- constellation("MRS", db1, db2)
#' tl <- ct |>
#'   as_csv_files(dir = tempdir())
#'
#' @export
as_csv_files <- function(db, dir, type) UseMethod("as_csv_files")

#' @rdname as_csv_files
#'
#' @export
as_csv_files.star_database <- function(db, dir = NULL, type = 1) {
  if (is.null(dir)) {
    dir <- tempdir()
  }
  l <- as_tibble_list(db)
  names <- names(l)
  for (i in seq_along(l)) {
    file <- paste0(dir, '/', names[i], '.csv')
    if (type == 1) {
      utils::write.csv(l[[i]], file = file, row.names = FALSE)
    } else {
      utils::write.csv2(l[[i]], file = file, row.names = FALSE)
    }
  }
  dir
}
