test_that("star_database() define a a star database", {
  expect_equal({
    s <- star_schema() |>
      define_facts(fact_schema(
        name = "MRS Cause",
        measures = c("Pneumonia and Influenza Deaths",
                     "All Deaths")
      )) |>
      define_dimension(dimension_schema(name = "When",
                                        attributes = c("Year"))) |>
      define_dimension(dimension_schema(
        name = "Where",
        attributes = c("REGION",
                       "State")
      ))
    star_database(s, ft_num)
  },
  structure(list(
    schema = structure(list(
      facts = list(mrs_cause = structure(
        list(
          name = "MRS Cause",
          measures = c("Pneumonia and Influenza Deaths",
                       "All Deaths"),
          agg_functions = NULL,
          nrow_agg = NULL
        ),
        class = "fact_schema"
      )),
      dimensions = list(
        when = structure(list(
          name = "When", attributes = "Year"
        ), class = "dimension_schema"),
        where = structure(list(
          name = "Where", attributes = c("REGION",
                                         "State")
        ), class = "dimension_schema")
      )
    ), class = "star_schema"),
    instance = list(
      facts = list(mrs_cause = structure(
        list(
          name = "MRS Cause",
          surrogate_keys = c("when_key", "where_key"),
          dim_int_names = c("when",
                            "where"),
          table = structure(
            list(
              when_key = c(1L, 1L,
                           2L, 2L, 3L, 3L),
              where_key = c(1L, 2L, 1L, 2L, 1L, 2L),
              `Pneumonia and Influenza Deaths` = c(14L, 27L, 14L,
                                                   10L, 11L, 35L),
              `All Deaths` = c(235L, 594L, 238L, 276L,
                               98L, 653L),
              nrow_agg = c(5L, 3L, 4L, 1L, 2L, 5L)
            ),
            class = c("tbl_df",
                      "tbl", "data.frame"),
            row.names = c(NA,-6L)
          )
        ),
        class = "fact_table"
      )),
      dimensions = list(
        when = structure(
          list(
            name = "When",
            surrogate_key = "when_key",
            table = structure(
              list(
                when_key = 1:3,
                Year = c("1962", "1963", "1964")
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
            name = "Where",
            surrogate_key = "where_key",
            table = structure(
              list(
                where_key = 1:2,
                REGION = c("1",
                           "1"),
                State = c("CT", "MA")
              ),
              row.names = c(NA,-2L),
              class = c("tbl_df", "tbl", "data.frame")
            )
          ),
          class = "dimension_table"
        )
      )
    )
  ), class = "star_database"))
})

test_that("star_database() define a a star database", {
  expect_equal({
    s <- star_schema() |>
      define_facts(fact_schema(name = "MRS Cause")) |>
      define_dimension(dimension_schema(name = "When",
                                        attributes = c("Year"))) |>
      define_dimension(dimension_schema(
        name = "Where",
        attributes = c("REGION",
                       "State")
      ))
    star_database(s, ft_num)
  },
  structure(list(
    schema = structure(list(
      facts = list(mrs_cause = structure(
        list(
          name = "MRS Cause",
          measures = NULL,
          agg_functions = NULL,
          nrow_agg = NULL
        ),
        class = "fact_schema"
      )),
      dimensions = list(
        when = structure(list(
          name = "When", attributes = "Year"
        ), class = "dimension_schema"),
        where = structure(list(
          name = "Where", attributes = c("REGION",
                                         "State")
        ), class = "dimension_schema")
      )
    ), class = "star_schema"),
    instance = list(
      facts = list(mrs_cause = structure(
        list(
          name = "MRS Cause",
          surrogate_keys = c("when_key", "where_key"),
          dim_int_names = c("when",
                            "where"),
          table = structure(
            list(
              when_key = c(1L, 1L,
                           2L, 2L, 3L, 3L),
              where_key = c(1L, 2L, 1L, 2L, 1L, 2L),
              nrow_agg = c(5L, 3L, 4L, 1L, 2L, 5L)
            ),
            class = c("tbl_df",
                      "tbl", "data.frame"),
            row.names = c(NA,-6L)
          )
        ),
        class = "fact_table"
      )),
      dimensions = list(
        when = structure(
          list(
            name = "When",
            surrogate_key = "when_key",
            table = structure(
              list(
                when_key = 1:3,
                Year = c("1962", "1963", "1964")
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
            name = "Where",
            surrogate_key = "where_key",
            table = structure(
              list(
                where_key = 1:2,
                REGION = c("1",
                           "1"),
                State = c("CT", "MA")
              ),
              row.names = c(NA,-2L),
              class = c("tbl_df", "tbl", "data.frame")
            )
          ),
          class = "dimension_table"
        )
      )
    )
  ), class = "star_database"))
})



test_that("snake_case() transform a a star database in snake case", {
  expect_equal({
    s <- star_schema() |>
      define_facts(fact_schema(
        name = "MRS Cause",
        measures = c("Pneumonia and Influenza Deaths",
                     "All Deaths")
      )) |>
      define_dimension(dimension_schema(name = "When",
                                        attributes = c("Year"))) |>
      define_dimension(dimension_schema(
        name = "Where",
        attributes = c("REGION",
                       "State")
      ))
    snake_case(star_database(s, ft_num))
  },
  structure(list(
    schema = structure(list(
      facts = list(mrs_cause = structure(
        list(
          name = "MRS Cause",
          measures = c("Pneumonia and Influenza Deaths",
                       "All Deaths"),
          agg_functions = NULL,
          nrow_agg = NULL
        ),
        class = "fact_schema"
      )),
      dimensions = list(
        when = structure(list(
          name = "When", attributes = "Year"
        ), class = "dimension_schema"),
        where = structure(list(
          name = "Where", attributes = c("REGION",
                                         "State")
        ), class = "dimension_schema")
      )
    ), class = "star_schema"),
    instance = list(
      facts = list(mrs_cause = structure(
        list(
          name = "mrs_cause",
          surrogate_keys = c("when_key", "where_key"),
          dim_int_names = c("when",
                            "where"),
          table = structure(
            list(
              when_key = c(1L, 1L,
                           2L, 2L, 3L, 3L),
              where_key = c(1L, 2L, 1L, 2L, 1L, 2L),
              pneumonia_and_influenza_deaths = c(14L, 27L, 14L,
                                                 10L, 11L, 35L),
              all_deaths = c(235L, 594L, 238L, 276L,
                             98L, 653L),
              nrow_agg = c(5L, 3L, 4L, 1L, 2L, 5L)
            ),
            class = c("tbl_df",
                      "tbl", "data.frame"),
            row.names = c(NA,-6L)
          )
        ),
        class = "fact_table"
      )),
      dimensions = list(
        when = structure(
          list(
            name = "when",
            surrogate_key = "when_key",
            table = structure(
              list(
                when_key = 1:3,
                year = c("1962", "1963", "1964")
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
                where_key = 1:2,
                region = c("1",
                           "1"),
                state = c("CT", "MA")
              ),
              row.names = c(NA,-2L),
              class = c("tbl_df", "tbl", "data.frame")
            )
          ),
          class = "dimension_table"
        )
      )
    )
  ), class = "star_database"))
})

test_that("as_tibble_list() export star_database as a list of tibbles", {
  expect_equal({
    ft1 <- ft_num  |>
      dplyr::filter(City != "Cambridge") |>
      dplyr::filter(Year <= "1963")

    db1 <- star_database(mrs_cause_schema, ft1) |>
      snake_case()

    db1 |>
      as_tibble_list()
  }, {
    list(
      when = structure(
        list(when_key = 1:2, year = c("1962", "1963")),
        row.names = c(NA,-2L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      where = structure(
        list(
          where_key = 1:3,
          region = c("1", "1", "1"),
          state = c("CT",
                    "CT", "MA"),
          city = c("Bridgeport", "Hartford", "Boston")
        ),
        row.names = c(NA,-3L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      mrs_cause = structure(
        list(
          when_key = c(1L, 1L, 1L, 2L,
                       2L, 2L),
          where_key = c(1L, 2L, 3L, 1L, 2L, 3L),
          pneumonia_and_influenza_deaths = c(9L,
                                             5L, 23L, 2L, 12L, 10L),
          all_deaths = c(131L, 104L, 555L, 46L,
                         192L, 276L),
          nrow_agg = c(3L, 2L, 2L, 1L, 3L, 1L)
        ),
        class = c("tbl_df",
                  "tbl", "data.frame"),
        row.names = c(NA,-6L)
      )
    )
  })
})
