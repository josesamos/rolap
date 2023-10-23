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
  names <- sort(names(l_db))
  for (name in names){
    query1 <- c(query1, sprintf("name: %s, %d rows\n", name, nrow(l_db[[name]])))
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

  ## ----------------------------------------------------------------------------------------------------------
  mrs_db_age_refresh <- mrs_ft_new |>
    update_according_to(mrs_db, star = "mrs_age")

  mrs_db_cause_refresh <- mrs_ft_new |>
    update_according_to(mrs_db, star = "mrs_cause")

  mrs_db <- mrs_db |>
    incremental_refresh(mrs_db_age_refresh) |>
    incremental_refresh(mrs_db_cause_refresh, existing_instances = "group")


  query5 <- NULL
  l_db <- mrs_db |>
    as_tibble_list()
  names <- sort(names(l_db))
  for (name in names){
    query5 <- c(query5, sprintf("name: %s, %d rows\n", name, nrow(l_db[[name]])))
  }

  query6 <- head(sort(l_db[['where']]$city), 15)


  ## ---------------------------------------------------------------------------------------------------------
  mrs_con <- mrs_sqlite_connect()

  query7 <- NULL
  tables <- DBI::dbListTables(mrs_con)
  for (t in tables) {
    res <- DBI::dbGetQuery(mrs_con, sprintf('SELECT COUNT(*) FROM `%s`', t))
    query7 <- c(query7, sprintf("%s: %d\n",t, res[[1]]))
  }

  query8 <- DBI::dbGetQuery(mrs_con, 'SELECT city FROM `where` ORDER BY city LIMIT 15')

  mrs_sqlite_disconnect(mrs_con)


  ## ---------------------------------------------------------------------------------------------------------
  ## ---------------------------------------------------------------------------------------------------------
  mrs_db_new <- load_star_database(mrs_rdb_file)

  mrs_db_new <- mrs_db_new |>
    incremental_refresh(mrs_db_age_refresh, existing_instances = "delete") |>
    incremental_refresh(mrs_db_cause_refresh, existing_instances = "delete")


  query9 <- NULL
  l_db <- mrs_db_new |>
    as_tibble_list()
  names <- sort(names(l_db))
  for (name in names){
    query9 <- c(query9, sprintf("name: %s, %d rows\n", name, nrow(l_db[[name]])))
  }

  query10 <- head(sort(l_db[['where']]$city), 15)


  ## ---------------------------------------------------------------------------------------------------------
  mrs_con <- mrs_sqlite_connect()

  query11 <- NULL
  tables <- DBI::dbListTables(mrs_con)
  for (t in tables) {
    res <- DBI::dbGetQuery(mrs_con, sprintf('SELECT COUNT(*) FROM `%s`', t))
    query11 <- c(query11, sprintf("%s: %d\n",t, res[[1]]))
  }

  query12 <- DBI::dbGetQuery(mrs_con, 'SELECT city FROM `where` ORDER BY city LIMIT 15')

  mrs_sqlite_disconnect(mrs_con)



  #############################################################
  expect_equal({
    mrs_db |> get_deployment_names()
  },
  {
    "mrs"
  })


  #############################################################
  expect_equal({
    names(mrs_db)
  },
  {
    names(mrs_db_new)
  })


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
      }
    ))
  })


  #############################################################
  expect_equal({
    query1
  },
  {
    c(
      "name: mrs_age, 16565 rows\n",
      "name: mrs_cause, 3342 rows\n",
      "name: when, 1966 rows\n",
      "name: where, 120 rows\n",
      "name: who, 5 rows\n"
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
    query2
  },
  {
    c(
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
  expect_equal({
    query5
  },
  {
    c(
      "name: mrs_age, 18228 rows\n",
      "name: mrs_cause, 3677 rows\n",
      "name: when, 2076 rows\n",
      "name: where, 122 rows\n",
      "name: who, 5 rows\n"
    )
  })


  #############################################################
  expect_equal({
    query7
  },
  {
    c("mrs_age: 18228\n",
      "mrs_cause: 3677\n",
      "when: 2076\n",
      "where: 122\n",
      "who: 5\n")
  })


  #############################################################
  expect_equal({
    query6
  },
  {
    c(
      "Akron",
      "Albany",
      "Albuquerque",
      "Allentown",
      "Atlanta",
      "Austin",
      "Baltimore",
      "Baton Rouge",
      "Berkeley",
      "Birmingham",
      "Boise",
      "Boston",
      "Bridgeport",
      "Buffalo",
      "Cambridge"
    )
  })


  #############################################################
  expect_equal({
    query8
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
        "Baltimore",
        "Baton Rouge",
        "Berkeley",
        "Birmingham",
        "Boise",
        "Boston",
        "Bridgeport",
        "Buffalo",
        "Cambridge"
      )
    ),
    class = "data.frame",
    row.names = c(NA,-15L))
  })

  #############################################################
  expect_equal({
    query9
  },
  {
    c(
      "name: mrs_age, 16365 rows\n",
      "name: mrs_cause, 3302 rows\n",
      "name: when, 1954 rows\n",
      "name: where, 120 rows\n",
      "name: who, 5 rows\n"
    )
  })


  #############################################################
  expect_equal({
    query11
  },
  {
    c("mrs_age: 16365\n",
      "mrs_cause: 3302\n",
      "when: 1954\n",
      "where: 120\n",
      "who: 5\n")
  })


  #############################################################
  expect_equal({
    query10
  },
  {
    c(
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
  })


  #############################################################
  expect_equal({
    query12
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
  expect_equal({
    res <-  mrs_db |> cancel_deployment(name = "mrs")
    res |> get_deployment_names()
  },
  {
    character(0)
  })


  #############################################################
})
