test_that("refresh", {
  #############################################################

  mrs_rdb_file <- tempfile("mrs", fileext = ".rdb")
  mrs_sqlite_file <- tempfile("mrs", fileext = ".sqlite")

  mrs_sqlite_connect <- function() {
    DBI::dbConnect(RSQLite::SQLite(),
                   dbname = mrs_sqlite_file)
  }
  mrs_sqlite_disconnect <- function(con) {
    DBI::dbDisconnect(con)
  }

  mrs_db2 <- mrs_db

  mrs_db <- mrs_db |>
    deploy(
      name = "mrs",
      connect = mrs_sqlite_connect,
      disconnect = mrs_sqlite_disconnect,
      file = mrs_rdb_file
    )

  mrs_db2 <- mrs_db2 |>
    deploy(
      name = "mrs",
      connect = mrs_sqlite_connect,
      file = mrs_rdb_file
    )

  query1 <- NULL
  l_db <- mrs_db |>
    as_tibble_list()
  names <- names(l_db)
  for (i in seq_along(l_db)){
    query1 <- c(query1, sprintf("name: %s, %d rows\n", names[i], nrow(l_db[[i]])))
  }

  query2 <- head(sort(l_db[['where']]$city), 15)


  ## ---------------------------------------------------------------------------------------------------------
  mrs_con <- mrs_sqlite_connect()

  query3 <- NULL
  tables <- DBI::dbListTables(mrs_con)
  for (t in tables) {
    res <- DBI::dbGetQuery(mrs_con, sprintf('SELECT COUNT(*) FROM `%s`', t))
    query3 <- c(query3, sprintf("%s: %d\n",t, res[[1]]))
  }

  query4 <- DBI::dbGetQuery(mrs_con, 'SELECT city FROM `where` ORDER BY city LIMIT 15')

  mrs_sqlite_disconnect(mrs_con)


  #############################################################
  expect_equal({
    length(mrs_db2$deploy$databases$mrs$disconnect)
  },
  {
    1
  })


  #############################################################
  expect_equal({
    mrs_db$deploy$databases
  },
  {
    list(mrs = list(
      connect = function ()
      {
        DBI::dbConnect(RSQLite::SQLite(), dbname = mrs_sqlite_file)
      },
      disconnect = function (con)
      {
        DBI::dbDisconnect(con)
      },
      pending_sql = NULL
    ))
  })


  #############################################################
  expect_equal({
    query1
  },
  {
    c(
      "name: when, 1966 rows\n",
      "name: where, 120 rows\n",
      "name: who, 5 rows\n",
      "name: mrs_cause, 3342 rows\n",
      "name: mrs_age, 16565 rows\n"
    )
  })


  #############################################################
  expect_equal({
    query3
  },
  {
    c("mrs_age: 16565\n",
      "mrs_cause: 3342\n",
      "when: 1966\n",
      "where: 120\n",
      "who: 5\n")
  })


  #############################################################
  expect_equal({
    query4
  },
  {
    structure(list(
      city = c(
        "Akron",
        "Albany",
        "Albuquerque",
        "Allentown",
        "Atlanta",
        "Austin",
        "Baton Rouge",
        "Berkeley",
        "Birmingham",
        "Boise",
        "Bridgeport",
        "Buffalo",
        "Cambridge",
        "Camden",
        "Canton"
      )
    ),
    class = "data.frame",
    row.names = c(NA,-15L))
  })


  #############################################################
})
