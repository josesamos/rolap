test_that("star_database() define a a star database", {
  expect_equal({
    s <- star_schema() |>
      define_facts(fact_schema(
        name = "mrs_cause",
        measures = c("Pneumonia and Influenza Deaths",
                     "All Deaths")
      )) |>
      define_dimension(dimension_schema(name = "when",
                                        attributes = c("Year"))) |>
      define_dimension(dimension_schema(
        name = "where",
        attributes = c("REGION",
                       "State")
      ))
    star_database(s, ft_num)
  },
  structure(
    list(
      schema = structure(list(
        facts = structure(
          list(
            name = "mrs_cause",
            measures = c("Pneumonia and Influenza Deaths",
                         "All Deaths"),
            agg_functions = c("SUM", "SUM"),
            nrow_agg = NULL
          ),
          class = "fact_schema"
        ),
        dimensions = list(
          when = structure(list(
            name = "when", attributes = "Year"
          ), class = "dimension_schema"),
          where = structure(list(
            name = "where", attributes = c("REGION",
                                           "State")
          ), class = "dimension_schema")
        )
      ), class = "star_schema"),
      facts = list(mrs_cause = structure(
        list(
          name = "mrs_cause",
          surrogate_keys = c("when_key", "where_key"),
          facts = structure(
            list(
              when_key = c(1L, 1L, 2L, 2L, 3L, 3L),
              where_key = c(1L,
                            2L, 1L, 2L, 1L, 2L),
              `Pneumonia and Influenza Deaths` = c(14L,
                                                   27L, 14L, 10L, 11L, 35L),
              `All Deaths` = c(235L,
                               594L, 238L, 276L, 98L, 653L),
              nrow_agg = c(5L, 3L,
                           4L, 1L, 2L, 5L)
            ),
            class = c("tbl_df", "tbl", "data.frame"),
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
            dimension = structure(
              list(
                when_key = 1:3,
                Year = c("1962",
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
            dimension = structure(
              list(
                where_key = 1:2,
                REGION = c("1", "1"),
                State = c("CT",
                          "MA")
              ),
              row.names = c(NA,-2L),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          ),
          class = "dimension_table"
        )
      )
    ),
    class = "star_database"
  ))
})
