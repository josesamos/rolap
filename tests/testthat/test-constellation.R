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

    ct <- constellation("MRS", db1, db2)
  }, {
    structure(
      list(
        name = "MRS",
        operations = list(
          mrs_cause = structure(list(
            operations = structure(
              list(
                operation = c("star_database",
                              "snake_case"),
                name = c("mrs_cause", ""),
                details = c("___UNKNOWN___",
                            ""),
                details2 = c("", ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation"),
          mrs_age = structure(list(
            operations = structure(
              list(
                operation = c("star_database",
                              "snake_case"),
                name = c("mrs_age", ""),
                details = c("___UNKNOWN___",
                            ""),
                details2 = c("", ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation")
        ),
        lookup_tables = list(mrs_cause = NULL, mrs_age = NULL),
        schemas = list(
          mrs_cause = structure(list(
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
                name = "When",
                attributes = "Year"
              ), class = "dimension_schema"),
              where = structure(list(
                name = "Where",
                attributes = c("REGION",
                               "State", "City")
              ), class = "dimension_schema")
            )
          ), class = "star_schema"),
          mrs_age = structure(list(
            facts = list(mrs_age = structure(
              list(
                name = "MRS Age",
                measures = "All Deaths",
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
                name = "Where",
                attributes = c("REGION",
                               "State", "City")
              ), class = "dimension_schema"),
              who = structure(list(
                name = "Who", attributes = "Age"
              ), class = "dimension_schema")
            )
          ), class = "star_schema")
        ),
        refresh = list(),
        facts = list(
          mrs_cause = structure(
            list(
              name = "mrs_cause",
              surrogate_keys = c("when_key", "where_key"),
              agg = c(
                pneumonia_and_influenza_deaths = "SUM",
                all_deaths = "SUM",
                nrow_agg = "SUM"
              ),
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
              agg = c(all_deaths = "SUM", nrow_agg = "SUM"),
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
        ),
        rpd = list()
      ),
      class = "star_database"
    )
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

    ct <- constellation("MRS", db1, db2, db3)
  }, {
    structure(
      list(
        name = "MRS",
        operations = list(
          mrs_cause = structure(list(
            operations = structure(
              list(
                operation = c("star_database",
                              "snake_case"),
                name = c("mrs_cause", ""),
                details = c("___UNKNOWN___",
                            ""),
                details2 = c("", ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation"),
          mrs_age = structure(list(
            operations = structure(
              list(
                operation = c("star_database",
                              "snake_case"),
                name = c("mrs_age", ""),
                details = c("___UNKNOWN___",
                            ""),
                details2 = c("", ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation"),
          mrs_cause_2 = structure(list(
            operations = structure(
              list(
                operation = c("star_database", "snake_case"),
                name = c("mrs_cause_2",
                         ""),
                details = c("___UNKNOWN___", ""),
                details2 = c("",
                             ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation")
        ),
        lookup_tables = list(
          mrs_cause = NULL,
          mrs_age = NULL,
          mrs_cause_2 = NULL
        ),
        schemas = list(
          mrs_cause = structure(list(
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
                name = "When",
                attributes = "Year"
              ), class = "dimension_schema"),
              where = structure(list(
                name = "Where",
                attributes = c("REGION",
                               "State", "City")
              ), class = "dimension_schema")
            )
          ), class = "star_schema"),
          mrs_age = structure(list(
            facts = list(mrs_age = structure(
              list(
                name = "MRS Age",
                measures = "All Deaths",
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
                name = "Where",
                attributes = c("REGION",
                               "State", "City")
              ), class = "dimension_schema"),
              who = structure(list(
                name = "Who", attributes = "Age"
              ), class = "dimension_schema")
            )
          ), class = "star_schema"),
          mrs_cause_2 = structure(list(
            facts = list(mrs_cause_2 = structure(
              list(
                name = "MRS Cause 2",
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
                name = "Where",
                attributes = c("REGION",
                               "City", "State")
              ), class = "dimension_schema")
            )
          ), class = "star_schema")
        ),
        refresh = list(),
        facts = list(
          mrs_cause = structure(
            list(
              name = "mrs_cause",
              surrogate_keys = c("when_key", "where_key"),
              agg = c(
                pneumonia_and_influenza_deaths = "SUM",
                all_deaths = "SUM",
                nrow_agg = "SUM"
              ),
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
              agg = c(all_deaths = "SUM", nrow_agg = "SUM"),
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
              agg = c(nrow_agg = "SUM"),
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
        ),
        rpd = list()
      ),
      class = "star_database"
    )
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
    db3 <- star_database(s, ft1) |>
      snake_case()

    ct <- constellation("MRS", db1, db3)
    constellation("MRS", db2, ct)
  }, {
    structure(
      list(
        name = "MRS",
        operations = list(
          mrs_age = structure(list(
            operations = structure(
              list(
                operation = c("star_database",
                              "snake_case"),
                name = c("mrs_age", ""),
                details = c("___UNKNOWN___",
                            ""),
                details2 = c("", ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation"),
          mrs_cause = structure(list(
            operations = structure(
              list(
                operation = c("star_database",
                              "snake_case"),
                name = c("mrs_cause", ""),
                details = c("___UNKNOWN___",
                            ""),
                details2 = c("", ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation"),
          mrs_cause_2 = structure(list(
            operations = structure(
              list(
                operation = c("star_database", "snake_case"),
                name = c("mrs_cause_2",
                         ""),
                details = c("___UNKNOWN___", ""),
                details2 = c("",
                             ""),
                order = c(1, 2)
              ),
              row.names = c(NA,-2L),
              class = "data.frame"
            )
          ), class = "star_operation")
        ),
        lookup_tables = list(
          mrs_age = NULL,
          mrs_cause = NULL,
          mrs_cause_2 = NULL
        ),
        schemas = list(
          mrs_age = structure(list(
            facts = list(mrs_age = structure(
              list(
                name = "MRS Age",
                measures = "All Deaths",
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
                name = "Where",
                attributes = c("REGION",
                               "State", "City")
              ), class = "dimension_schema"),
              who = structure(list(
                name = "Who", attributes = "Age"
              ), class = "dimension_schema")
            )
          ), class = "star_schema"),
          mrs_cause = structure(list(
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
                name = "When",
                attributes = "Year"
              ), class = "dimension_schema"),
              where = structure(list(
                name = "Where",
                attributes = c("REGION",
                               "State", "City")
              ), class = "dimension_schema")
            )
          ), class = "star_schema"),
          mrs_cause_2 = structure(list(
            facts = list(mrs_cause_2 = structure(
              list(
                name = "MRS Cause 2",
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
                name = "Where",
                attributes = c("REGION",
                               "City", "State")
              ), class = "dimension_schema")
            )
          ), class = "star_schema")
        ),
        refresh = list(),
        facts = list(
          mrs_age = structure(
            list(
              name = "mrs_age",
              surrogate_keys = c("when_key",
                                 "where_key", "who_key"),
              agg = c(all_deaths = "SUM", nrow_agg = "SUM"),
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
          mrs_cause = structure(
            list(
              name = "mrs_cause",
              surrogate_keys = c("when_key",
                                 "where_key"),
              agg = c(
                pneumonia_and_influenza_deaths = "SUM",
                all_deaths = "SUM",
                nrow_agg = "SUM"
              ),
              dim_int_names = c("when",
                                "where"),
              table = structure(
                list(
                  when_key = c(1L, 1L,
                               1L, 2L, 2L, 2L),
                  where_key = c(1L, 2L, 3L, 1L, 2L, 3L),
                  pneumonia_and_influenza_deaths = c(9L, 5L, 23L, 2L,
                                                     12L, 10L),
                  all_deaths = c(131L, 104L, 555L, 46L, 192L,
                                 276L),
                  nrow_agg = c(3L, 2L, 2L, 1L, 3L, 1L)
                ),
                row.names = c(NA,-6L),
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
              agg = c(nrow_agg = "SUM"),
              dim_int_names = c("when",
                                "where"),
              table = structure(
                list(
                  when_key = c(1L, 1L,
                               1L, 2L, 2L, 2L),
                  where_key = c(3L, 1L, 2L, 3L, 1L, 2L),
                  nrow_agg = c(2L, 3L, 2L, 1L, 1L, 3L)
                ),
                row.names = c(NA,-6L),
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
        ),
        rpd = list()
      ),
      class = "star_database"
    )
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
    db3 <- star_database(s, ft1) |>
      snake_case()

    ct <- constellation("MRS", db1, db3)
    constellation("MRS", db2, ct)
  }, {
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
    db3 <- star_database(s, ft1) |>
      snake_case()

    ct <- constellation("MRS", db2, db1)
    constellation("MRS", ct, db3)
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

    constellation("MRS", db1, db2) |>
      as_tibble_list()
  }, {
    list(
      when = structure(
        list(
          when_key = 1:3,
          year = c("1962", "1963",
                   "1964")
        ),
        row.names = c(NA, -3L),
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
        row.names = c(NA, -4L),
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
        row.names = c(NA, -5L),
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
        row.names = c(NA, -6L),
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
        row.names = c(NA, -15L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      )
    )
  })
})

test_that("constellation() define constellation with rpd", {
  expect_equal({
    db1 <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))

    db2 <- star_database(mrs_age_schema_rpd, ft_age_rpd) |>
      role_playing_dimension(rpd = "When Arrived",
                             roles = c("When Available"))
    db <- constellation("MRS", db1, db2)
    c(
      db$rpd,
      nrow(db$dimensions$when$table),
      nrow(db$dimensions$when_available$table),
      nrow(db$dimensions$when_arrived$table),
      nrow(db$dimensions$when_received$table),
      names(db$dimensions$when$table),
      names(db$dimensions$when_available$table),
      names(db$dimensions$when_arrived$table),
      names(db$dimensions$when_received$table)
    )
  }, {
    list(
      when = c("when", "when_available", "when_received", "when_arrived"),
      25L,
      25L,
      25L,
      25L,
      "when_key",
      "Year",
      "WEEK",
      "Week Ending Date",
      "when_available_key",
      "Data Availability Year",
      "Data Availability Week",
      "Data Availability Date",
      "when_arrived_key",
      "Arrival Year",
      "Arrival Week",
      "Arrival Date",
      "when_received_key",
      "Reception Year",
      "Reception Week",
      "Reception Date"
    )
  })
})

test_that("constellation() get_role_playing_dimension_names()", {
  expect_equal({
    db1 <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))

    db2 <- star_database(mrs_age_schema_rpd, ft_age_rpd) |>
      role_playing_dimension(rpd = "When Arrived",
                             roles = c("When Available"))
    constellation("MRS", db1, db2) |>
      get_role_playing_dimension_names()
  }, {
    list(rpd_1 = c("when", "when_arrived", "when_available", "when_received"))
  })
})

test_that("constellation() replace_attribute_values() with role_playing_dimension()",
          {
            expect_equal({
              db1 <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
                role_playing_dimension(rpd = "When",
                                       roles = c("When Available", "When Received"))

              db2 <-
                star_database(mrs_age_schema_rpd, ft_age_rpd) |>
                role_playing_dimension(rpd = "When Arrived",
                                       roles = c("When Available"))
              db <- constellation("MRS", db1, db2)
              db <- db |> replace_attribute_values(
                name = "When Available",
                old = c('1962', '11', '1962-03-14'),
                new = c('1962', '3', '1962-01-15')
              )
              c(
                db$operations$mrs_cause$operation$operation,
                db$rpd,
                nrow(db$dimensions$when$table),
                nrow(db$dimensions$when_available$table),
                nrow(db$dimensions$when_received$table),
                names(db$dimensions$when$table),
                names(db$dimensions$when_available$table),
                names(db$dimensions$when_received$table),
                as.vector(db$dimensions$when$table$WEEK),
                as.vector(
                  db$dimensions$when_available$table$`Data Availability Week`
                ),
                as.vector(db$dimensions$when_received$table$`Reception Week`)
              )
            }, {
              list(
                "star_database",
                "role_playing_dimension",
                "replace_attribute_values",
                when = c("when", "when_available", "when_received", "when_arrived"),
                25L,
                25L,
                25L,
                "when_key",
                "Year",
                "WEEK",
                "Week Ending Date",
                "when_available_key",
                "Data Availability Year",
                "Data Availability Week",
                "Data Availability Date",
                "when_received_key",
                "Reception Year",
                "Reception Week",
                "Reception Date",
                "1",
                "3",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "4",
                "4",
                "5",
                "5",
                "5",
                "6",
                "6",
                "6",
                "6",
                "7",
                "7",
                "8",
                "8",
                "9",
                "9",
                "9",
                "1",
                "3",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "4",
                "4",
                "5",
                "5",
                "5",
                "6",
                "6",
                "6",
                "6",
                "7",
                "7",
                "8",
                "8",
                "9",
                "9",
                "9",
                "1",
                "3",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "4",
                "4",
                "5",
                "5",
                "5",
                "6",
                "6",
                "6",
                "6",
                "7",
                "7",
                "8",
                "8",
                "9",
                "9",
                "9"
              )
            })
          })

test_that("constellation() replace_attribute_values() with role_playing_dimension()",
          {
            expect_equal({
              db1 <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
                role_playing_dimension(rpd = "When",
                                       roles = c("When Available", "When Received"))

              db2 <-
                star_database(mrs_age_schema_rpd, ft_age_rpd) |>
                role_playing_dimension(rpd = "When Arrived",
                                       roles = c("When Available"))
              db <- constellation("MRS", db1, db2)

              db <- db |> replace_attribute_values(
                name = "When Available",
                old = c('1962', '11', '1962-03-14'),
                new = c('1962', '3', '1962-01-15')
              ) |>
                group_dimension_instances(name = "When") |>
                group_dimension_instances(name = "Who")

              c(
                db$operations$mrs_cause$operation$operation,
                db$operations$mrs_age$operation$operation,
                db$rpd,
                nrow(db$dimensions$when$table),
                nrow(db$dimensions$when_available$table),
                nrow(db$dimensions$when_received$table),
                names(db$dimensions$when$table),
                names(db$dimensions$when_available$table),
                names(db$dimensions$when_received$table),
                as.vector(db$dimensions$when$table$WEEK),
                as.vector(
                  db$dimensions$when_available$table$`Data Availability Week`
                ),
                as.vector(db$dimensions$when_received$table$`Reception Week`)
              )
            }, {
              list(
                "star_database",
                "role_playing_dimension",
                "replace_attribute_values",
                "group_dimension_instances",
                "star_database",
                "role_playing_dimension",
                "replace_attribute_values",
                "group_dimension_instances",
                "group_dimension_instances",
                when = c("when", "when_available",
                         "when_received", "when_arrived"),
                24L,
                24L,
                24L,
                "when_key",
                "Year",
                "WEEK",
                "Week Ending Date",
                "when_available_key",
                "Data Availability Year",
                "Data Availability Week",
                "Data Availability Date",
                "when_received_key",
                "Reception Year",
                "Reception Week",
                "Reception Date",
                "1",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "4",
                "4",
                "5",
                "5",
                "5",
                "6",
                "6",
                "6",
                "6",
                "7",
                "7",
                "8",
                "8",
                "9",
                "9",
                "9",
                "1",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "4",
                "4",
                "5",
                "5",
                "5",
                "6",
                "6",
                "6",
                "6",
                "7",
                "7",
                "8",
                "8",
                "9",
                "9",
                "9",
                "1",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "4",
                "4",
                "5",
                "5",
                "5",
                "6",
                "6",
                "6",
                "6",
                "7",
                "7",
                "8",
                "8",
                "9",
                "9",
                "9"
              )
            })
          })
