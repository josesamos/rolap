test_that("constellation() define constellation", {
  expect_equal({
    ft1 <- ft_num  |>
      dplyr::filter(City != "Cambridge") |>
      dplyr::filter(Year <= "1963")

    db1 <- star_database(mrs_cause_schema, ft1) |>
      snake_case()

    ft2 <- ft_age  |>
      dplyr::filter(City != "Boston" & City != "Bridgeport") |>
      dplyr::filter(Year >= "1963")

    db2 <- star_database(mrs_age_schema, ft2) |>
      snake_case()

    ct <- constellation("MRS", list(db1, db2))
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
                when_key = c(1L, 1L, 1L, 2L, 2L, 2L),
                where_key = c(1L,
                              2L, 3L, 1L, 2L, 3L),
                pneumonia_and_influenza_deaths = c(9L,
                                                   5L, 23L, 2L, 12L, 10L),
                all_deaths = c(131L, 104L, 555L,
                               46L, 192L, 276L),
                nrow_agg = c(3L, 2L, 2L, 1L, 3L, 1L)
              ),
              row.names = c(NA,-6L),
              class = c("tbl_df", "tbl",
                        "data.frame")
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
                when_key = c(2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L,
                             3L, 3L, 3L, 3L, 3L),
                where_key = c(2L, 2L, 2L, 2L, 2L,
                              2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L),
                who_key = c(1L,
                            2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L),
                all_deaths = c(5L, 13L, 51L, 107L, 16L, 0L, 2L, 16L,
                               28L, 7L, 0L, 4L, 25L, 52L, 3L),
                nrow_agg = c(3L, 3L,
                             3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L)
              ),
              row.names = c(NA,-15L),
              class = c("tbl_df", "tbl", "data.frame")
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
              class = c("tbl_df", "tbl",
                        "data.frame")
            )
          ),
          class = "dimension_table"
        )
      )
    ), class = "constellation")
  })
})

test_that("constellation() define constellation", {
  expect_equal({
    ft1 <- ft_num  |>
      dplyr::filter(City != "Cambridge") |>
      dplyr::filter(Year <= "1963")

    db1 <- star_database(mrs_cause_schema, ft1) |>
      snake_case()

    ft2 <- ft_age  |>
      dplyr::filter(City != "Boston" & City != "Bridgeport") |>
      dplyr::filter(Year >= "1963")

    db2 <- star_database(mrs_age_schema, ft2) |>
      snake_case()

    s <- star_schema() |>
      define_facts(fact_schema(name = "MRS Cause 2")) |>
      define_dimension(dimension_schema(name = "When",
                                        attributes = c("Year"))) |>
      define_dimension(dimension_schema(
        name = "Where",
        attributes = c("REGION",
                       "City",
                       "State")
      ))
    db3 <- star_database(s, ft_num) |>
      snake_case()

    ct <- constellation("MRS", list(db1, db2, db3))
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
                when_key = c(1L, 1L, 1L, 2L, 2L, 2L),
                where_key = c(1L,
                              2L, 3L, 1L, 2L, 3L),
                pneumonia_and_influenza_deaths = c(9L,
                                                   5L, 23L, 2L, 12L, 10L),
                all_deaths = c(131L, 104L, 555L,
                               46L, 192L, 276L),
                nrow_agg = c(3L, 2L, 2L, 1L, 3L, 1L)
              ),
              row.names = c(NA,-6L),
              class = c("tbl_df", "tbl",
                        "data.frame")
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
                when_key = c(2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L,
                             3L, 3L, 3L, 3L, 3L),
                where_key = c(2L, 2L, 2L, 2L, 2L,
                              2L, 2L, 2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L),
                who_key = c(1L,
                            2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L),
                all_deaths = c(5L, 13L, 51L, 107L, 16L, 0L, 2L, 16L,
                               28L, 7L, 0L, 4L, 25L, 52L, 3L),
                nrow_agg = c(3L, 3L,
                             3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L, 3L, 3L, 3L, 3L, 3L)
              ),
              row.names = c(NA,-15L),
              class = c("tbl_df", "tbl", "data.frame")
            )
          ),
          class = "fact_table"
        ),
        mrs_cause_2 = structure(
          list(
            name = "mrs_cause_2",
            surrogate_keys = c("when_key",
                               "where_key"),
            dim_int_names = c("when", "where"),
            table = structure(
              list(
                when_key = c(1L, 1L, 1L, 1L, 2L, 2L, 2L, 3L, 3L, 3L,
                             3L),
                where_key = c(3L, 1L, 4L, 2L, 3L, 1L, 2L, 3L, 1L,
                              4L, 2L),
                nrow_agg = c(2L, 3L, 1L, 2L, 1L, 1L, 3L, 2L,
                             1L, 3L, 1L)
              ),
              row.names = c(NA,-11L),
              class = c("tbl_df",
                        "tbl", "data.frame")
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

test_that("as_tibble_list() export constellation as a list of tibbles", {
  expect_equal({
    ft1 <- ft_num  |>
      dplyr::filter(City != "Cambridge") |>
      dplyr::filter(Year <= "1963")

    db1 <- star_database(mrs_cause_schema, ft1) |>
      snake_case()

    ft2 <- ft_age  |>
      dplyr::filter(City != "Boston" & City != "Bridgeport") |>
      dplyr::filter(Year >= "1963")

    db2 <- star_database(mrs_age_schema, ft2) |>
      snake_case()

    constellation("MRS", list(db1, db2)) |>
      as_tibble_list()
  }, {
    list(
      when = structure(
        list(
          when_key = 1:3,
          year = c("1962", "1963",
                   "1964")
        ),
        row.names = c(NA,-3L),
        class = c("tbl_df", "tbl",
                  "data.frame")
      ),
      where = structure(
        list(
          where_key = 1:4,
          region = c("1",
                     "1", "1", "1"),
          state = c("CT", "CT", "MA", "MA"),
          city = c("Bridgeport",
                   "Hartford", "Boston", "Cambridge")
        ),
        row.names = c(NA,-4L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      ),
      who = structure(
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
      ),
      mrs_cause = structure(
        list(
          when_key = c(1L, 1L, 1L, 2L, 2L, 2L),
          where_key = c(1L, 2L,
                        3L, 1L, 2L, 3L),
          pneumonia_and_influenza_deaths = c(9L, 5L,
                                             23L, 2L, 12L, 10L),
          all_deaths = c(131L, 104L, 555L, 46L,
                         192L, 276L),
          nrow_agg = c(3L, 2L, 2L, 1L, 3L, 1L)
        ),
        row.names = c(NA,-6L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      mrs_age = structure(
        list(
          when_key = c(2L, 2L, 2L, 2L, 2L, 3L, 3L, 3L, 3L, 3L, 3L,
                       3L, 3L, 3L, 3L),
          where_key = c(2L, 2L, 2L, 2L, 2L, 2L, 2L,
                        2L, 2L, 2L, 4L, 4L, 4L, 4L, 4L),
          who_key = c(1L, 2L, 3L,
                      4L, 5L, 1L, 2L, 3L, 4L, 5L, 1L, 2L, 3L, 4L, 5L),
          all_deaths = c(5L,
                         13L, 51L, 107L, 16L, 0L, 2L, 16L, 28L, 7L, 0L, 4L, 25L, 52L,
                         3L),
          nrow_agg = c(3L, 3L, 3L, 3L, 3L, 1L, 1L, 1L, 1L, 1L,
                       3L, 3L, 3L, 3L, 3L)
        ),
        row.names = c(NA,-15L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      )
    )
  })
})
