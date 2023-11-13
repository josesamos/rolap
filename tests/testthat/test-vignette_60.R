test_that("refresh", {
  #############################################################

  mrs_cause <- mrs_db |>
    get_star_database("mrs_cause")

  mrs_dm <- mrs_db |>
    as_dm_class()

  tl1 <- mrs_db |>
    as_tibble_list()

  tl2 <- mrs_db |>
    as_single_tibble_list()

  con <- DBI::dbConnect(RSQLite::SQLite())
  mrs_db |>
    as_rdb(con)
  tables <- DBI::dbListTables(con)
  DBI::dbDisconnect(con)

  f <- mrs_db |>
    as_xlsx_file(file = tempfile())

  d <- mrs_db |>
    as_csv_files(dir = tempdir())

  lf <- list.files(d, pattern = "*.csv")



  #############################################################
  expect_equal({
    mrs_cause |> get_table_names()
  },
  {
    c("mrs_cause", "when", "where")
  })



  #############################################################
  expect_equal({
    class(mrs_dm)
  },
  {
    "dm"
  })



  #############################################################
  expect_equal({
    names(tl1)
  },
  {
    c("when", "where", "who", "mrs_cause", "mrs_age")
  })



  #############################################################
  expect_equal({
    names(tl2)
  },
  {
    c("mrs_cause", "mrs_age")
  })



  #############################################################
  expect_equal({
    tables
  },
  {
    c("mrs_age", "mrs_cause", "when", "where", "who")
  })



  #############################################################
  expect_equal({
    tools::file_ext(f)
  },
  {
    "xlsx"
  })



  #############################################################
  expect_equal({
    lf
  },
  {
    c("mrs_age.csv",
      "mrs_cause.csv",
      "when.csv",
      "where.csv",
      "who.csv")
  })



  #############################################################
})


test_that("as_multistar()",
          {
            db1 <- star_database(mrs_cause_schema, ft_num) |>
              snake_case()
            ms1 <- db1 |>
              as_multistar()

            db2 <- star_database(mrs_age_schema, ft_age) |>
              snake_case()

            ct <- constellation("MRS", db1, db2)
            ms <- ct |>
              as_multistar()

            expect_equal({
              class(ms1) == "multistar" & class(ms) == "multistar"
            }, {
              TRUE
            })

            expect_equal({
              names(ms1)
            }, {
              c("fact", "dimension")
            })

            expect_equal({
              names(ms)
            }, {
              c("fact", "dimension")
            })

            expect_equal({
              names(ms$fact)
            }, {
              c("mrs_cause", "mrs_age")
            })

            expect_equal({
              names(ms$dimension)
            }, {
              c("when", "where", "who")
            })

            expect_equal({
              class(ms1$fact[[1]])
            }, {
              c("tbl_df", "tbl", "data.frame", "fact_table")
            })

            expect_equal({
              class(ms1$dimension[[1]])
            }, {
              c("tbl_df", "tbl", "data.frame", "dimension_table")
            })

          })

