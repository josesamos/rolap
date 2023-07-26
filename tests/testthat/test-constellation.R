test_that("multiplication works", {
  expect_equal({
    when <- dimension_schema(name = "When",
                             attributes = c("Year"))

    where <- dimension_schema(name = "Where",
                              attributes = c("REGION",
                                             "State",
                                             "City"))

    s1 <- star_schema() |>
      define_facts(
        name = "MRS Cause",
        measures = c("Pneumonia and Influenza Deaths",
                     "All Deaths")
      ) |>
      define_dimension(when) |>
      define_dimension(where)

    ft1 <- ft |>
      dplyr::mutate(`Pneumonia and Influenza Deaths` = as.integer(`Pneumonia and Influenza Deaths`)) |>
      dplyr::mutate(`All Deaths` = as.integer(`All Deaths`)) |>
      dplyr::filter(Year > "1962")

    db1 <- star_database(s1, ft1) |>
      snake_case()


    s2 <- star_schema() |>
      define_facts(name = "MRS Age",
                   measures = c("All Deaths")) |>
      define_dimension(when) |>
      define_dimension(where) |>
      define_dimension(name = "Who",
                       attributes = c("Age"))

    ft2 <- ft |>
      dplyr::select(-`Pneumonia and Influenza Deaths`, -`All Deaths`) |>
      tidyr::gather("Age", "All Deaths", 7:11) |>
      dplyr::mutate(`All Deaths` = as.integer(`All Deaths`)) |>
      dplyr::mutate(Age = stringr::str_replace(Age, " \\(all cause deaths\\)", "")) |>
      dplyr::filter(Year < "1964") |>
      dplyr::filter(City != "Boston" & City != "Bridgeport") |>
      dplyr::filter(WEEK >= "8")

    db2 <- star_database(s2, ft2) |>
      snake_case()

    constellation("MRS", list(db1, db2))
  }, {
    structure(list(
      name = "MRS",
      facts = list(
        mrs_cause = structure(
          list(
            name = "mrs_cause",
            surrogate_keys = c("when_key", "where_key"),
            dim_int_names = c("when", "where"),
            table = structure(
              list(
                when_key = c(2L, 2L, 2L, 3L, 3L, 3L, 3L),
                where_key = c(1L,
                              2L, 3L, 1L, 2L, 3L, 4L),
                pneumonia_and_influenza_deaths = c(2L,
                                                   12L, 10L, 8L, 3L, 22L, 13L),
                all_deaths = c(46L, 192L,
                               276L, 45L, 53L, 569L, 84L),
                nrow_agg = c(1L, 3L, 1L,
                             1L, 1L, 2L, 3L)
              ),
              row.names = c(NA,-7L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          ),
          class = "fact_table"
        ),
        mrs_age = structure(
          list(
            name = "mrs_age",
            surrogate_keys = c("when_key", "where_key",
                               "who_key"),
            dim_int_names = c("when", "where", "who"),
            table = structure(
              list(
                when_key = c(1L, 1L, 1L, 1L, 1L, 2L, 2L, 2L, 2L, 2L),
                where_key = c(4L, 4L, 4L, 4L, 4L, 2L, 2L, 2L, 2L, 2L),
                who_key = c(1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L),
                all_deaths = c(0L, 2L, 7L, 29L, 1L, 2L, 3L, 14L, 27L,
                               3L),
                nrow_agg = c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L,
                             1L)
              ),
              row.names = c(NA,-10L),
              class = c("tbl_df", "tbl",
                        "data.frame")
            )
          ),
          class = "fact_table"
        )
      ),
      dimensions = list(
        when = structure(
          list(
            name = "when",
            surrogate_key = "when_key",
            table = structure(
              list(
                when_key = 1:3,
                year = c("1962",
                         "1963", "1964")
              ),
              row.names = c(NA,-3L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          ),
          class = "dimension_table"
        ),
        where = structure(
          list(
            name = "where",
            surrogate_key = "where_key",
            table = structure(
              list(
                where_key = 1:4,
                region = c("1", "1", "1", "1"),
                state = c("CT", "CT", "MA", "MA"),
                city = c("Bridgeport",
                         "Hartford", "Boston", "Cambridge")
              ),
              row.names = c(NA,-4L),
              class = c("tbl_df", "tbl", "data.frame")
            )
          ),
          class = "dimension_table"
        ),
        who = structure(
          list(
            name = "who",
            surrogate_key = "who_key",
            table = structure(
              list(
                who_key = 1:5,
                age = c(
                  "1-24 years",
                  "25-44 years",
                  "45-64 years",
                  "65+ years",
                  "<1 year"
                )
              ),
              row.names = c(NA,-5L),
              class = c("tbl_df", "tbl", "data.frame")
            )
          ),
          class = "dimension_table"
        )
      )
    ), class = "constellation")
  })
})
