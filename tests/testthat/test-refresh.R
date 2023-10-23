


test_that("get_existing_fact_instances() update_according_to",
          {
            expect_equal({
              f1 <-
                flat_table('ft_num', ft_num[ft_num$City != 'Cambridge' &
                                              ft_num$Year != '1963', ]) |>
                as_star_database(mrs_cause_schema)

              r <- c(15L, 1L, 5L, 2L, 12L, 14L, 3L, 7L, 10L, 12L)
              f2 <- flat_table('ft_num2', ft_num[r, ])
              f2 <- f2 |>
                update_according_to(f1)
              f2 |> get_existing_fact_instances()
            }, {
              structure(
                list(
                  Year = c("1962", "1962", "1962", "1964", "1964"),
                  REGION = c("1", "1", "1", "1", "1"),
                  State = c("CT", "CT",
                            "MA", "CT", "MA"),
                  City = c("Bridgeport", "Hartford", "Boston",
                           "Bridgeport", "Boston"),
                  `Pneumonia and Influenza Deaths` = c(5L,
                                                       1L, 23L, 8L, 9L),
                  `All Deaths` = c(46L, 47L, 555L, 45L, 244L),
                  nrow_agg = c(1L, 1L, 2L, 1L, 1L)
                ),
                row.names = c(NA, -5L),
                class = c("tbl_df",
                          "tbl", "data.frame")
              )
            })
          })


test_that("get_new_dimension_instances() update_according_to",
          {
            expect_equal({
              f1 <-
                flat_table('ft_num', ft_num[ft_num$City != 'Cambridge' &
                                              ft_num$Year != '1963',]) |>
                as_star_database(mrs_cause_schema)

              r <- c(15L, 1L, 5L, 2L, 12L, 14L, 3L, 7L, 10L, 12L)
              f2 <- flat_table('ft_num2', ft_num[r,])
              f2 <- f2 |>
                update_according_to(f1)
              f2 |> get_new_dimension_instances()
            }, {
              list(
                when = structure(
                  list(Year = "1963"),
                  row.names = c(NA, -1L),
                  class = c("tbl_df", "tbl", "data.frame")
                ),
                where = structure(
                  list(
                    REGION = "1",
                    State = "MA",
                    City = "Cambridge"
                  ),
                  row.names = c(NA, -1L),
                  class = c("tbl_df", "tbl", "data.frame")
                )
              )
            })
          })


test_that("get_new_dimension_instances() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4', ]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    r <- c(5L, 1L, 5L, 2L, 2L, 4L, 3L, 7L, 10L, 2L)
    f2 <- flat_table('ft_num2', ft_cause_rpd[r, ])
    f2 <- f2 |>
      update_according_to(f1)
    f2 |> get_new_dimension_instances()
  }, {
    list(
      when = structure(
        list(
          Year = c("1962", "1962", "1962"),
          WEEK = c("4", "5", "6"),
          `Week Ending Date` = c("1962-01-27",
                                 "1962-02-01", "1962-02-07")
        ),
        row.names = c(NA, -3L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      ),
      where = structure(
        list(
          REGION = "1",
          State = "MA",
          City = "Cambridge"
        ),
        row.names = c(NA, -1L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      )
    )
  })
})


test_that("get_transformation_code() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4',]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    r <- c(5L, 1L, 5L, 2L, 2L, 4L, 3L, 7L, 10L, 2L)
    f2 <- flat_table('ft_num2', ft_cause_rpd[r,])
    f2 <- f2 |>
      update_according_to(f1)
    f2 |> get_transformation_code()
  }, {
    c(
      "transform_instance_table <- function(instance_df, star_sch) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    as_star_database(",
      "      schema = star_sch",
      "    ) |>",
      "    role_playing_dimension(",
      "      rpd = 'when',",
      "      roles = c('when_available', 'when_received'),",
      "      att_names = NULL",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
  })
})


test_that("get_transformation_code() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4',]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    r <- c(5L, 1L, 5L, 2L, 2L, 4L, 3L, 7L, 10L, 2L)
    f2 <- flat_table('ft_num2', ft_cause_rpd[r,])
    f2 <- f2 |>
      update_according_to(f1)
    f <- f2 |> get_transformation_file()
    readLines(f)
  }, {
    c(
      "transform_instance_table <- function(instance_df, star_sch) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    as_star_database(",
      "      schema = star_sch",
      "    ) |>",
      "    role_playing_dimension(",
      "      rpd = 'when',",
      "      roles = c('when_available', 'when_received'),",
      "      att_names = NULL",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
  })
})


test_that("incremental_refresh() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4', ]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    f2 <-
      flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
                                           ft_cause_rpd$WEEK != '2', ])
    f2 <- f2 |>
      update_according_to(f1)

    f1 <- f1 |>
      incremental_refresh(f2,
                          existing_instances = "ignore",
                          replace_transformations = FALSE, 'DONTDELETE')
    f1$refresh[[1]]
  }, {
    list(
      insert = list(
        when = structure(
          list(
            when_key = 11:14,
            Year = c("1962",
                     "1962", "1962", "1962"),
            WEEK = c("4", "5", "5", "6"),
            `Week Ending Date` = c("1962-01-27",
                                   "1962-01-29", "1962-02-01", "1962-02-07")
          ),
          row.names = c(NA, -4L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        when_available = structure(
          list(
            when_available_key = 11:14,
            `Data Availability Year` = c("1962",
                                         "1962", "1962", "1962"),
            `Data Availability Week` = c("4",
                                         "5", "5", "6"),
            `Data Availability Date` = c("1962-01-27",
                                         "1962-01-29", "1962-02-01", "1962-02-07")
          ),
          row.names = c(NA, -4L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        when_received = structure(
          list(
            when_received_key = 11:14,
            `Reception Year` = c("1962", "1962",
                                 "1962", "1962"),
            `Reception Week` = c("4", "5", "5", "6"),
            `Reception Date` = c("1962-01-27", "1962-01-29", "1962-02-01",
                                 "1962-02-07")
          ),
          row.names = c(NA, -4L),
          class = c("tbl_df",
                    "tbl", "data.frame")
        ),
        where = structure(
          list(
            where_key = 4L,
            REGION = "1",
            State = "MA",
            City = "Cambridge"
          ),
          row.names = c(NA, -1L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        mrs_cause = structure(
          list(
            when_key = c(11L, 11L, 11L),
            when_available_key = c(13L,
                                   13L, 14L),
            when_received_key = c(11L, 12L, 11L),
            where_key = c(4L,
                          2L, 3L),
            `Pneumonia and Influenza Deaths` = c(1L, 1L, 12L),
            `All Deaths` = c(40L, 47L, 285L),
            nrow_agg = c(1L, 1L,
                         1L)
          ),
          row.names = c(NA, -3L),
          class = c("tbl_df", "tbl",
                    "data.frame")
        )
      ),
      replace = NULL,
      delete = NULL
    )
  })
})


test_that("incremental_refresh() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4', ]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    f2 <-
      flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
                                           ft_cause_rpd$WEEK != '2', ])
    f2 <- f2 |>
      update_according_to(f1)

    f1 <- f1 |>
      incremental_refresh(f2)
    f1$facts
  }, {
    list(mrs_cause = structure(
      list(
        name = "mrs_cause",
        surrogate_keys = c(
          "when_key",
          "when_available_key",
          "when_received_key",
          "where_key"
        ),
        agg = c(
          `Pneumonia and Influenza Deaths` = "SUM",
          `All Deaths` = "SUM",
          nrow_agg = "SUM"
        ),
        dim_int_names = c("when",
                          "when_available", "when_received", "where"),
        table = structure(
          list(
            when_key = c(1L, 5L, 5L, 5L, 10L, 11L, 11L, 11L),
            when_available_key = c(6L,
                                   5L, 8L, 9L, 3L, 13L, 13L, 14L),
            when_received_key = c(4L,
                                  5L, 6L, 7L, 2L, 11L, 12L, 11L),
            where_key = c(1L, 2L, 1L,
                          3L, 2L, 4L, 2L, 3L),
            `Pneumonia and Influenza Deaths` = c(3L,
                                                 2L, 2L, 11L, 3L, 1L, 1L, 12L),
            `All Deaths` = c(46L, 54L,
                             43L, 270L, 63L, 40L, 47L, 285L),
            nrow_agg = c(1L, 1L, 1L,
                         1L, 1L, 1L, 1L, 1L)
          ),
          row.names = c(NA, -8L),
          class = c("tbl_df",
                    "tbl", "data.frame")
        )
      ),
      class = "fact_table"
    ))
  })
})


test_that("incremental_refresh() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4',]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    f2 <-
      flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
                                           ft_cause_rpd$WEEK != '2',])
    f2 <- f2 |>
      update_according_to(f1)

    f1 <- f1 |>
      incremental_refresh(f2)
    f1$dimensions
  }, {
    list(
      when = structure(
        list(
          name = "When",
          surrogate_key = "when_key",
          table = structure(
            list(
              when_key = 1:14,
              Year = c(
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962"
              ),
              WEEK = c(
                "1",
                "11",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "9",
                "4",
                "5",
                "5",
                "6"
              ),
              `Week Ending Date` = c(
                "1962-01-06",
                "1962-03-14",
                "1962-03-15",
                "1962-01-11",
                "1962-01-13",
                "1962-01-15",
                "1962-01-16",
                "1962-01-17",
                "1962-01-18",
                "1962-03-03",
                "1962-01-27",
                "1962-01-29",
                "1962-02-01",
                "1962-02-07"
              )
            ),
            row.names = c(NA, -14L),
            class = c("tbl_df",
                      "tbl", "data.frame")
          )
        ),
        class = "dimension_table"
      ),
      when_available = structure(
        list(
          name = "When Available",
          surrogate_key = "when_available_key",
          table = structure(
            list(
              when_available_key = 1:14,
              `Data Availability Year` = c(
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962"
              ),
              `Data Availability Week` = c(
                "1",
                "11",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "9",
                "4",
                "5",
                "5",
                "6"
              ),
              `Data Availability Date` = c(
                "1962-01-06",
                "1962-03-14",
                "1962-03-15",
                "1962-01-11",
                "1962-01-13",
                "1962-01-15",
                "1962-01-16",
                "1962-01-17",
                "1962-01-18",
                "1962-03-03",
                "1962-01-27",
                "1962-01-29",
                "1962-02-01",
                "1962-02-07"
              )
            ),
            row.names = c(NA, -14L),
            class = c("tbl_df",
                      "tbl", "data.frame")
          )
        ),
        class = "dimension_table"
      ),
      when_received = structure(
        list(
          name = "When Received",
          surrogate_key = "when_received_key",
          table = structure(
            list(
              when_received_key = 1:14,
              `Reception Year` = c(
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962",
                "1962"
              ),
              `Reception Week` = c(
                "1",
                "11",
                "11",
                "2",
                "2",
                "3",
                "3",
                "3",
                "3",
                "9",
                "4",
                "5",
                "5",
                "6"
              ),
              `Reception Date` = c(
                "1962-01-06",
                "1962-03-14",
                "1962-03-15",
                "1962-01-11",
                "1962-01-13",
                "1962-01-15",
                "1962-01-16",
                "1962-01-17",
                "1962-01-18",
                "1962-03-03",
                "1962-01-27",
                "1962-01-29",
                "1962-02-01",
                "1962-02-07"
              )
            ),
            row.names = c(NA, -14L),
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
              REGION = c("1", "1", "1", "1"),
              State = c("CT",
                        "CT", "MA", "MA"),
              City = c("Bridgeport", "Hartford",
                       "Boston", "Cambridge")
            ),
            row.names = c(NA, -4L),
            class = c("tbl_df",
                      "tbl", "data.frame")
          )
        ),
        class = "dimension_table"
      )
    )
  })
})


test_that("incremental_refresh() update_according_to",
          {
            f1 <-
              flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                                  ft_cause_rpd$WEEK != '4', ]) |>
              as_star_database(mrs_cause_schema_rpd) |>
              role_playing_dimension(rpd = "When",
                                     roles = c("When Available", "When Received"))
            f2 <-
              flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
                                                   ft_cause_rpd$WEEK != '2', ])
            f2 <- f2 |>
              update_according_to(f1)

            f1 <- f1 |>
              incremental_refresh(f2, existing_instances = "delete",
                                  replace_transformations = FALSE, 'DONTDELETE')

            expect_equal({
              f1$facts
            }, {
              list(mrs_cause = structure(
                list(
                  name = "mrs_cause",
                  surrogate_keys = c(
                    "when_key",
                    "when_available_key",
                    "when_received_key",
                    "where_key"
                  ),
                  agg = c(
                    `Pneumonia and Influenza Deaths` = "SUM",
                    `All Deaths` = "SUM",
                    nrow_agg = "SUM"
                  ),
                  dim_int_names = c("when",
                                    "when_available", "when_received", "where"),
                  table = structure(
                    list(
                      when_key = c(1L, 5L, 5L, 5L, 11L, 11L, 11L),
                      when_available_key = c(6L,
                                             5L, 8L, 9L, 13L, 13L, 14L),
                      when_received_key = c(4L, 5L,
                                            6L, 7L, 11L, 12L, 11L),
                      where_key = c(1L, 2L, 1L, 3L, 4L,
                                    2L, 3L),
                      `Pneumonia and Influenza Deaths` = c(3L, 2L, 2L,
                                                           11L, 1L, 1L, 12L),
                      `All Deaths` = c(46L, 54L, 43L, 270L,
                                       40L, 47L, 285L),
                      nrow_agg = c(1L, 1L, 1L, 1L, 1L, 1L, 1L)
                    ),
                    row.names = c(NA,-7L),
                    class = c("tbl_df", "tbl", "data.frame")
                  )
                ),
                class = "fact_table"
              ))
            })

            expect_equal({
              f1$refresh[[1]]
            }, {
              list(
                insert = list(
                  when = structure(
                    list(
                      when_key = 11:14,
                      Year = c("1962",
                               "1962", "1962", "1962"),
                      WEEK = c("4", "5", "5", "6"),
                      `Week Ending Date` = c("1962-01-27",
                                             "1962-01-29", "1962-02-01", "1962-02-07")
                    ),
                    row.names = c(NA,-4L),
                    class = c("tbl_df", "tbl", "data.frame")
                  ),
                  when_available = structure(
                    list(
                      when_available_key = 11:14,
                      `Data Availability Year` = c("1962",
                                                   "1962", "1962", "1962"),
                      `Data Availability Week` = c("4",
                                                   "5", "5", "6"),
                      `Data Availability Date` = c("1962-01-27",
                                                   "1962-01-29", "1962-02-01", "1962-02-07")
                    ),
                    row.names = c(NA,-4L),
                    class = c("tbl_df", "tbl", "data.frame")
                  ),
                  when_received = structure(
                    list(
                      when_received_key = 11:14,
                      `Reception Year` = c("1962", "1962",
                                           "1962", "1962"),
                      `Reception Week` = c("4", "5", "5", "6"),
                      `Reception Date` = c("1962-01-27", "1962-01-29", "1962-02-01",
                                           "1962-02-07")
                    ),
                    row.names = c(NA,-4L),
                    class = c("tbl_df",
                              "tbl", "data.frame")
                  ),
                  where = structure(
                    list(
                      where_key = 4L,
                      REGION = "1",
                      State = "MA",
                      City = "Cambridge"
                    ),
                    row.names = c(NA,-1L),
                    class = c("tbl_df", "tbl", "data.frame")
                  ),
                  mrs_cause = structure(
                    list(
                      when_key = c(11L, 11L, 11L),
                      when_available_key = c(13L,
                                             13L, 14L),
                      when_received_key = c(11L, 12L, 11L),
                      where_key = c(4L,
                                    2L, 3L),
                      `Pneumonia and Influenza Deaths` = c(1L, 1L, 12L),
                      `All Deaths` = c(40L, 47L, 285L),
                      nrow_agg = c(1L, 1L,
                                   1L)
                    ),
                    row.names = c(NA,-3L),
                    class = c("tbl_df", "tbl",
                              "data.frame")
                  )
                ),
                replace = NULL,
                delete = list(
                  mrs_cause = structure(
                    list(
                      when_key = 10L,
                      when_available_key = 3L,
                      when_received_key = 2L,
                      where_key = 2L
                    ),
                    row.names = c(NA,-1L),
                    class = c("tbl_df",
                              "tbl", "data.frame")
                  ),
                  when = structure(
                    list(
                      when_key = c(2L,
                                   3L, 10L)
                    ),
                    row.names = c(NA,-3L),
                    class = c("tbl_df", "tbl", "data.frame")
                  ),
                  when_available = structure(
                    list(
                      when_available_key = c(2L,
                                             3L, 10L)
                    ),
                    row.names = c(NA,-3L),
                    class = c("tbl_df",
                              "tbl", "data.frame")
                  ),
                  when_received = structure(
                    list(
                      when_received_key = c(2L,
                                            3L, 10L)
                    ),
                    row.names = c(NA,-3L),
                    class = c("tbl_df", "tbl",
                              "data.frame")
                  )
                )
              )
            })

          })


test_that("incremental_refresh() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4',]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    f2 <-
      flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
                                           ft_cause_rpd$WEEK != '2',])
    f2 <- f2 |>
      update_according_to(f1)

    f1 <- f1 |>
      incremental_refresh(f2, existing_instances = "replace")
    f1$facts
  }, {
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4',]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    f2 <-
      flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
                                           ft_cause_rpd$WEEK != '2',])
    f2 <- f2 |>
      update_according_to(f1)

    f1 <- f1 |>
      incremental_refresh(f2, existing_instances = "ignore")
    f1$facts
  })
})


test_that("incremental_refresh() update_according_to", {
  expect_equal({
    f1 <-
      flat_table('ft_num', ft_cause_rpd[ft_cause_rpd$City != 'Cambridge' &
                                          ft_cause_rpd$WEEK != '4', ]) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    f2 <-
      flat_table('ft_num2', ft_cause_rpd[ft_cause_rpd$City != 'Bridgeport' &
                                           ft_cause_rpd$WEEK != '2', ])
    f2 <- f2 |>
      update_according_to(f1)

    f1 <- f1 |>
      incremental_refresh(f2, existing_instances = "group")
    f1$facts
  }, {
    list(mrs_cause = structure(
      list(
        name = "mrs_cause",
        surrogate_keys = c(
          "when_key",
          "when_available_key",
          "when_received_key",
          "where_key"
        ),
        agg = c(
          `Pneumonia and Influenza Deaths` = "SUM",
          `All Deaths` = "SUM",
          nrow_agg = "SUM"
        ),
        dim_int_names = c("when",
                          "when_available", "when_received", "where"),
        table = structure(
          list(
            when_key = c(1L, 5L, 5L, 5L, 10L, 11L, 11L, 11L),
            when_available_key = c(6L,
                                   5L, 8L, 9L, 3L, 13L, 13L, 14L),
            when_received_key = c(4L,
                                  5L, 6L, 7L, 2L, 11L, 12L, 11L),
            where_key = c(1L, 2L, 1L,
                          3L, 2L, 4L, 2L, 3L),
            `Pneumonia and Influenza Deaths` = c(3L,
                                                 2L, 2L, 11L, 6L, 1L, 1L, 12L),
            `All Deaths` = c(46L, 54L,
                             43L, 270L, 126L, 40L, 47L, 285L),
            nrow_agg = c(1L, 1L, 1L,
                         1L, 2L, 1L, 1L, 1L)
          ),
          class = c("tbl_df", "tbl", "data.frame"),
          row.names = c(NA, -8L)
        )
      ),
      class = "fact_table"
    ))
  })
})
