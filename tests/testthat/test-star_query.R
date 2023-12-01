test_that("star_query() creates a star query", {
  sq <- mrs_db |>
    star_query()

  sq_1 <- sq |>
    select_dimension(name = "where",
                     attributes = c("city", "state")) |>
    select_dimension(name = "when",
                     attributes = "year") |>
    select_fact(name = "mrs_age")

  mrs_db_1 <- mrs_db |>
    run_query(sq_1)


  sq_2 <- sq |>
    select_dimension(name = "where",
                     attributes = c("city", "state")) |>
    select_dimension(name = "when",
                     attributes = "year") |>
    select_fact(name = "mrs_age",
                measures = c("all_deaths", "all_deaths", "nrow_agg"),
                agg_functions = c("MAX", "SUM", "SUM"))

  mrs_db_2 <- mrs_db |>
    run_query(sq_2)


  sq_3 <- sq |>
    select_dimension(name = "where",
                     attributes = c("city", "state")) |>
    select_dimension(name = "when",
                     attributes = "year") |>
    select_fact(name = "mrs_age",
                measures = c("all_deaths", "all_deaths", "nrow_agg"),
                agg_functions = c("MAX", "SUM", "SUM"),
                new = c("measure1", "measure2", "measure3"),
                nrow_agg = "nrow")

  mrs_db_3 <- mrs_db |>
    run_query(sq_3)



  expect_equal(sq$query,
               list(fact = list(), dimension = list()))

  expect_equal(mrs_db_1$facts$mrs_age$agg,
               c(nrow_agg_sq = "SUM"))

  expect_equal(names(mrs_db_1$facts$mrs_age$table),
               c("when_key", "where_key", "nrow_agg_sq"))

  expect_equal(
    mrs_db_2$facts$mrs_age$agg,
    c(
      max_all_deaths = "MAX",
      all_deaths = "SUM",
      nrow_agg = "SUM",
      nrow_agg_sq = "SUM"
    )
  )

  expect_equal(
    names(mrs_db_2$facts$mrs_age$table),
    c(
      "when_key",
      "where_key",
      "max_all_deaths",
      "all_deaths",
      "nrow_agg",
      "nrow_agg_sq"
    )
  )

  expect_equal(
    mrs_db_3$facts$mrs_age$agg,
    c(
      measure1 = "MAX",
      measure2 = "SUM",
      measure3 = "SUM",
      nrow = "SUM"
    )
  )

  expect_equal(
    names(mrs_db_3$facts$mrs_age$table),
    c(
      "when_key",
      "where_key",
      "measure1",
      "measure2",
      "measure3",
      "nrow"
    )
  )

})
