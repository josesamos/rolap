
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
  structure(
    list(
      name = "mrs_cause",
      operations = list(mrs_cause = structure(list(
        operations = structure(
          list(
            operation = c("define_dimension",
                          "define_dimension", "define_facts"),
            name = c("When", "Where",
                     "MRS Cause"),
            details = c(
              "Year",
              "REGION<|>State",
              "Pneumonia and Influenza Deaths<|>All Deaths<|>nrow_agg"
            ),
            details2 = c("", "", "SUM<|>SUM<|>SUM"),
            order = c(1,
                      2, 3)
          ),
          row.names = c(NA,-3L),
          class = "data.frame"
        )
      ), class = "star_operation")),
      facts = list(mrs_cause = structure(
        list(
          name = "MRS Cause",
          surrogate_keys = c("when_key", "where_key"),
          agg = c(
            `Pneumonia and Influenza Deaths` = "SUM",
            `All Deaths` = "SUM",
            nrow_agg = "SUM"
          ),
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
      ),
      rpd = list()
    ),
    class = "star_database"
  ))
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
  structure(
    list(
      name = "mrs_cause",
      operations = list(mrs_cause = structure(list(
        operations = structure(
          list(
            operation = c("define_dimension",
                          "define_dimension", "define_facts"),
            name = c("When", "Where",
                     "MRS Cause"),
            details = c("Year", "REGION<|>State", "nrow_agg"),
            details2 = c("", "", "SUM"),
            order = c(1, 2, 3)
          ),
          row.names = c(NA,-3L),
          class = "data.frame"
        )
      ), class = "star_operation")),
      facts = list(mrs_cause = structure(
        list(
          name = "MRS Cause",
          surrogate_keys = c("when_key", "where_key"),
          agg = c(nrow_agg = "SUM"),
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
      ),
      rpd = list()
    ),
    class = "star_database"
  ))
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
  structure(
    list(
      name = "mrs_cause",
      operations = list(mrs_cause = structure(list(
        operations = structure(
          list(
            operation = c(
              "define_dimension",
              "define_dimension",
              "define_facts",
              "snake_case"
            ),
            name = c("When",
                     "Where", "MRS Cause", ""),
            details = c(
              "Year",
              "REGION<|>State",
              "Pneumonia and Influenza Deaths<|>All Deaths<|>nrow_agg",
              ""
            ),
            details2 = c("", "", "SUM<|>SUM<|>SUM", ""),
            order = c(1,
                      2, 3, 4)
          ),
          row.names = c(NA,-4L),
          class = "data.frame"
        )
      ), class = "star_operation")),
      facts = list(mrs_cause = structure(
        list(
          name = "mrs_cause",
          surrogate_keys = c("when_key", "where_key"),
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
      ),
      rpd = list()
    ),
    class = "star_database"
  ))
})

test_that("set_attribute_names() and get_attribute_names()",
          {
            expect_equal({
              db <- star_database(mrs_cause_schema, ft_num) |>
                set_attribute_names(name = "where",
                                    new = c("Region",
                                            "State",
                                            "City"))
              c(
                db$operations$mrs_cause$operation$operation,
                db |>
                  get_attribute_names(name = "where")
              )
            }, {
              c(
                "define_dimension",
                "define_dimension",
                "define_facts",
                "set_attribute_names",
                "Region",
                "State",
                "City"
              )
            })
          })

test_that("set_attribute_names() and get_attribute_names()",
          {
            expect_equal({
              db <- star_database(mrs_cause_schema, ft_num) |>
                set_attribute_names(name = "where",
                                    old = c("REGION"),
                                    new = c("Region"))
              c(
                db$operations$mrs_cause$operation$operation,
                db |>
                  get_attribute_names(name = "where")
              )
            }, {
              c(
                "define_dimension",
                "define_dimension",
                "define_facts",
                "set_attribute_names",
                "Region",
                "State",
                "City"
              )
            })
          })

test_that("set_measure_names() and get_measure_names()", {
  expect_equal({
    db <- star_database(mrs_cause_schema, ft_num) |>
      set_measure_names(new = c("Pneumonia and Influenza",
                                "All",
                                "Rows Aggregated"))
    c(db$operations$mrs_cause$operation$operation,
      db |>
        get_measure_names())
  }, {
    c(
      "define_dimension",
      "define_dimension",
      "define_facts",
      "set_measure_names",
      "Pneumonia and Influenza",
      "All",
      "Rows Aggregated"
    )
  })
})

test_that("get_similar_attribute_values()", {
  expect_equal({
    db <- star_database(mrs_cause_schema, ft_num)
    db$dimensions$where$table$City[2] <- " BrId  gEport "
    db |> get_similar_attribute_values("where", col_as_vector = 'dput_instance')
  }, {
    list(structure(
      list(
        REGION = c("1", "1"),
        State = c("CT", "CT"),
        City = c(" BrId  gEport ", "Bridgeport"),
        dput_instance = c("c('1', 'CT', ' BrId  gEport ')",
                          "c('1', 'CT', 'Bridgeport')")
      ),
      row.names = c(NA, -2L),
      class = c("tbl_df",
                "tbl", "data.frame")
    ))
  })
})

test_that("get_similar_attribute_values()", {
  expect_equal({
    db <- star_database(mrs_cause_schema, ft_num)
    db$dimensions$where$table$City[2] <- " BrId  gEport "
    db$dimensions$where$table$State[1] <- " c   T "
    db$dimensions$when$table$Year[3] <- '1963.'
    db |> get_similar_attribute_values(col_as_vector = 'dput_instance')
  }, {
    list(when = list(structure(
      list(
        Year = c("1963", "1963."),
        dput_instance = c("c('1963')",
                          "c('1963.')")
      ),
      row.names = c(NA,-2L),
      class = c("tbl_df", "tbl",
                "data.frame")
    )),
    where = list(structure(
      list(
        REGION = c("1",
                   "1"),
        State = c(" c   T ", "CT"),
        City = c("Bridgeport", " BrId  gEport "),
        dput_instance = c("c('1', ' c   T ', 'Bridgeport')", "c('1', 'CT', ' BrId  gEport ')")
      ),
      row.names = c(NA,-2L),
      class = c("tbl_df", "tbl", "data.frame")
    )))
  })
})

test_that("get_similar_attribute_values()", {
  expect_equal({
    db <- star_database(mrs_cause_schema, ft_num)
    db$dimensions$where$table$City[2] <- " BrId  gEport "
    db$dimensions$where$table$State[1] <- " c   T "
    db$dimensions$when$table$Year[3] <- '1963.'
    db |> get_similar_attribute_values("where",
                                       attributes = c('City', 'State'),
                                       col_as_vector = 'dput_instance')
  }, {
    list(structure(
      list(
        City = c(" BrId  gEport ", "Bridgeport"),
        State = c("CT", " c   T "),
        dput_instance = c("c(' BrId  gEport ', 'CT')",
                          "c('Bridgeport', ' c   T ')")
      ),
      row.names = c(NA,-2L),
      class = c("tbl_df",
                "tbl", "data.frame")
    ))
  })
})

test_that("get_similar_attribute_values_individually()", {
  expect_equal({
    db <- star_database(mrs_cause_schema, ft_num)
    db$dimensions$where$table$City[2] <- " BrId  gEport "
    db$dimensions$where$table$State[1] <- " c   T "
    db$dimensions$when$table$Year[3] <- '1963.'
    db |> get_similar_attribute_values_individually()
  }, {
    list(when = list(structure(
      list(Year = c("1963", "1963.")),
      row.names = c(NA,-2L),
      class = c("tbl_df", "tbl", "data.frame")
    )),
    where = list(
      structure(
        list(State = c(" c   T ", "CT")),
        row.names = c(NA,-2L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      structure(
        list(City = c(" BrId  gEport ", "Bridgeport")),
        row.names = c(NA,-2L),
        class = c("tbl_df", "tbl", "data.frame")
      )
    ))
  })
})


test_that("get_unique_attribute_values()", {
  expect_equal({
    star_database(mrs_cause_schema, ft_num) |>
      get_unique_attribute_values()
  }, {
    list(
      when = structure(
        list(Year = c("1962", "1963", "1964")),
        row.names = c(NA,-3L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      where = structure(
        list(
          REGION = c("1", "1", "1", "1"),
          State = c("CT", "CT", "MA",
                    "MA"),
          City = c("Bridgeport", "Hartford", "Boston", "Cambridge")
        ),
        row.names = c(NA,-4L),
        class = c("tbl_df", "tbl", "data.frame")
      )
    )
  })
})


test_that("get_unique_attribute_values()", {
  expect_equal({
    star_database(mrs_cause_schema, ft_num) |>
      get_unique_attribute_values(name = "where")
  }, {
    structure(
      list(
        REGION = c("1", "1", "1", "1"),
        State = c("CT",
                  "CT", "MA", "MA"),
        City = c("Bridgeport", "Hartford", "Boston",
                 "Cambridge")
      ),
      row.names = c(NA,-4L),
      class = c("tbl_df", "tbl",
                "data.frame")
    )
  })
})


test_that("get_unique_attribute_values()", {
  expect_equal({
    star_database(mrs_cause_schema, ft_num) |>
      get_unique_attribute_values("where",
                                  attributes = c("REGION", "State"))
  }, {
    structure(
      list(REGION = c("1", "1"), State = c("CT", "MA")),
      row.names = c(NA,-2L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })
})


test_that("replace_attribute_values()", {
  expect_equal({
    db <- star_database(mrs_cause_schema, ft_num)
    db <- db |> replace_attribute_values(
      "where",
      old = c('1', 'CT', 'Bridgeport'),
      new = c('1', 'CT', 'Hartford')
    )
    db$dimensions$where$table
  }, {
    structure(
      list(
        where_key = 1:4,
        REGION = c("1", "1", "1", "1"),
        State = c("CT", "CT", "MA", "MA"),
        City = c("Hartford", "Hartford",
                 "Boston", "Cambridge")
      ),
      row.names = c(NA, -4L),
      class = c("tbl_df",
                "tbl", "data.frame")
    )
  })
})

test_that("replace_attribute_values() with role_playing_dimension()", {
  expect_equal({
    db <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
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
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_facts",
      "role_playing_dimension",
      "replace_attribute_values",
      when = c("when", "when_available", "when_received"),
      15L,
      15L,
      15L,
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
      "6",
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
      "6",
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
      "6",
      "9"
    )
  })
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
        row.names = c(NA, -2L),
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
        row.names = c(NA, -3L),
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
        row.names = c(NA, -6L)
      )
    )
  })
})

test_that("role_playing_dimension() define a rpd", {
  expect_equal({
    db <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    c(
      db$operations$mrs_cause$operation$operation,
      db$rpd,
      nrow(db$dimensions$when$table),
      nrow(db$dimensions$when_available$table),
      nrow(db$dimensions$when_received$table),
      names(db$dimensions$when$table),
      names(db$dimensions$when_available$table),
      names(db$dimensions$when_received$table)
    )
  }, {
    list(
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_facts",
      "role_playing_dimension",
      when = c("when", "when_available", "when_received"),
      15L,
      15L,
      15L,
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
      "Reception Date"
    )
  })
})

test_that("role_playing_dimension() define a rpd", {
  expect_equal({
    db <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
      role_playing_dimension(
        rpd = "When",
        roles = c("When Available", "When Received"),
        rpd_att_names = TRUE
      )

    c(
      db$operations$mrs_cause$operation$operation,
      db$rpd,
      nrow(db$dimensions$when$table),
      nrow(db$dimensions$when_available$table),
      nrow(db$dimensions$when_received$table),
      names(db$dimensions$when$table),
      names(db$dimensions$when_available$table),
      names(db$dimensions$when_received$table)
    )
  }, {
    list(
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_facts",
      "role_playing_dimension",
      when = c("when", "when_available", "when_received"),
      15L,
      15L,
      15L,
      "when_key",
      "Year",
      "WEEK",
      "Week Ending Date",
      "when_available_key",
      "Year",
      "WEEK",
      "Week Ending Date",
      "when_received_key",
      "Year",
      "WEEK",
      "Week Ending Date"
    )
  })
})

test_that("role_playing_dimension() define a rpd", {
  expect_equal({
    db <- star_database(mrs_cause_schema_rpd, ft_cause_rpd) |>
      role_playing_dimension(
        rpd = "When",
        roles = c("When Available", "When Received"),
        att_names = c("Year", "Week", "Date")
      )

    c(
      db$operations$mrs_cause$operation$operation,
      db$rpd,
      nrow(db$dimensions$when$table),
      nrow(db$dimensions$when_available$table),
      nrow(db$dimensions$when_received$table),
      names(db$dimensions$when$table),
      names(db$dimensions$when_available$table),
      names(db$dimensions$when_received$table)
    )
  }, {
    list(
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_dimension",
      "define_facts",
      "role_playing_dimension",
      when = c("when", "when_available", "when_received"),
      15L,
      15L,
      15L,
      "when_key",
      "Year",
      "Week",
      "Date",
      "when_available_key",
      "Year",
      "Week",
      "Date",
      "when_received_key",
      "Year",
      "Week",
      "Date"
    )
  })
})

test_that("as_single_tibble_list()", {
  expect_equal({
    ft1 <- ft_num  |>
      dplyr::filter(City != "Cambridge") |>
      dplyr::filter(Year <= "1963")

    db1 <- star_database(mrs_cause_schema, ft1) |>
      snake_case()

    db1 |>
      as_single_tibble_list()
  }, {
    list(mrs_cause = structure(
      list(
        year = c("1962", "1962", "1962",
                 "1963", "1963", "1963"),
        region = c("1", "1", "1", "1", "1",
                   "1"),
        state = c("CT", "CT", "MA", "CT", "CT", "MA"),
        city = c(
          "Bridgeport",
          "Hartford",
          "Boston",
          "Bridgeport",
          "Hartford",
          "Boston"
        ),
        pneumonia_and_influenza_deaths = c(9L,
                                           5L, 23L, 2L, 12L, 10L),
        all_deaths = c(131L, 104L, 555L, 46L,
                       192L, 276L),
        nrow_agg = c(3L, 2L, 2L, 1L, 3L, 1L)
      ),
      row.names = c(NA,-6L),
      class = c("tbl_df", "tbl", "data.frame")
    ))
  })
})

test_that("as_single_tibble_list()", {
  expect_equal({
    db1 <- star_database(mrs_cause_schema, ft_num) |>
      snake_case()
    db2 <- star_database(mrs_age_schema, ft_age) |>
      snake_case()
    ct <- constellation("MRS", list(db1, db2))
    tl <- ct |>
      as_single_tibble_list()
    c(names(tl$mrs_cause), names(tl$mrs_age))
  }, {
    c(
      "year",
      "region",
      "state",
      "city",
      "pneumonia_and_influenza_deaths",
      "all_deaths",
      "nrow_agg",
      "year",
      "region",
      "state",
      "city",
      "age",
      "all_deaths",
      "nrow_agg"
    )
  })
})


test_that("as_single_tibble_list()", {
  expect_equal({
    s <- star_schema() |>
      define_facts(fact_schema(
        name = "mrs_cause",
        measures = c("Pneumonia and Influenza Deaths",
                     "All Deaths")
      )) |>
      define_dimension(dimension_schema(
        name = "When",
        attributes = c("Year",
                       "WEEK",
                       "Week Ending Date")
      )) |>
      define_dimension(dimension_schema(
        name = "When Available",
        attributes = c(
          "Data Availability Year",
          "Data Availability Week",
          "Data Availability Date"
        )
      )) |>
      define_dimension(dimension_schema(
        name = "When Received",
        attributes = c("Reception Year",
                       "Reception Week",
                       "Reception Date")
      )) |>
      define_dimension(dimension_schema(
        name = "where",
        attributes = c("REGION",
                       "State",
                       "City")
      ))

    db <- star_database(s, ft_cause_rpd) |>
      role_playing_dimension(
        rpd = "When",
        roles = c("When Available", "When Received"),
        rpd_att_names = TRUE
      )
    r <- db |> as_single_tibble_list()
    names(r[[1]])
  }, {
    c(
      "Year",
      "WEEK",
      "Week Ending Date",
      "Year_when_available",
      "WEEK_when_available",
      "Week Ending Date_when_available",
      "Year_when_received",
      "WEEK_when_received",
      "Week Ending Date_when_received",
      "REGION",
      "State",
      "City",
      "Pneumonia and Influenza Deaths",
      "All Deaths",
      "nrow_agg"
    )
  })
})

test_that("as_single_tibble_list()", {
  expect_equal({
    db1 <- star_database(mrs_cause_schema, ft_num) |>
      snake_case()
    db2 <- star_database(mrs_age_schema, ft_age) |>
      snake_case()
    ct1 <- constellation("MRS", list(db1, db2))
    r <- ct1 |> as_single_tibble_list()
    c(names(r[[1]]), names(r[[2]]))
  }, {
    c(
      "year",
      "region",
      "state",
      "city",
      "pneumonia_and_influenza_deaths",
      "all_deaths",
      "nrow_agg",
      "year",
      "region",
      "state",
      "city",
      "age",
      "all_deaths",
      "nrow_agg"
    )
  })
})
