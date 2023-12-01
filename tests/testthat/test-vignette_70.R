test_that("query", {
  #############################################################

  sq <- mrs_db |>
    star_query()

  sq_0 <- sq

  ## --------------------------------------------------------------------------------------------------------
  sq_1 <- sq |>
    select_fact(name = "mrs_age",
                measures = "all_deaths",
                agg_functions = "MAX")


  ## --------------------------------------------------------------------------------------------------------
  sq_2 <- sq |>
    select_fact(name = "mrs_age",
                measures = "all_deaths")


  ## --------------------------------------------------------------------------------------------------------
  sq_3 <- sq |>
    select_fact(name = "mrs_age")


  ## --------------------------------------------------------------------------------------------------------
  sq_4 <- sq |>
    select_fact(name = "mrs_age",
                measures = "all_deaths") |>
    select_fact(name = "mrs_cause")


  ## --------------------------------------------------------------------------------------------------------
  sq_5 <- sq |>
    select_dimension(name = "where",
                     attributes = c("city", "state"))


  ## --------------------------------------------------------------------------------------------------------
  sq_6 <- sq |>
    select_dimension(name = "where")


  ## --------------------------------------------------------------------------------------------------------
  sq_7 <- sq |>
    filter_dimension(name = "when", week <= " 3") |>
    filter_dimension(name = "where", city == "Bridgeport")

  ## --------------------------------------------------------------------------------------------------------

  sq <- star_query(mrs_db) |>
    select_dimension(name = "where",
                     attributes = c("region", "state")) |>
    select_dimension(name = "when",
                     attributes = "year") |>
    select_fact(name = "mrs_age",
                measures = "all_deaths") |>
    select_fact(name = "mrs_cause",
                measures = "all_deaths") |>
    filter_dimension(name = "when", week <= " 3" & year >= "2010")

  mrs_db_2 <- mrs_db |>
    run_query(sq)



  #############################################################
  expect_equal({
    sq_1$query
  },
  {
    list(fact = list(mrs_age = list(
      measure = c(all_deaths = "MAX",
                  nrow_agg_sq = "SUM"),
      new = NULL
    )), dimension = list())
  })

  expect_equal({
    sq_6$query
  },
  {
    list(fact = list(), dimension = list(where = list(
      attribute = c(
        "region",
        "state",
        "city",
        "city_state",
        "status",
        "pop",
        "lat",
        "long"
      )
    )))
  })

  expect_equal({
    sq
  },
  {
    structure(list(
      schema = list(
        fact = list(
          mrs_cause = list(
            fk = c("when_key",
                   "where_key"),
            nrow_agg = c(nrow_agg = "SUM"),
            measure = c(
              pneumonia_and_influenza_deaths = "SUM",
              all_deaths = "SUM"
            )
          ),
          mrs_age = list(
            fk = c("when_key", "where_key",
                   "who_key"),
            nrow_agg = c(nrow_agg = "SUM"),
            measure = c(all_deaths = "SUM")
          )
        ),
        dimension = list(
          when = list(
            pk = "when_key",
            attribute = c("year",
                          "week", "week_ending_date"),
            table = structure(
              list(
                when_key = integer(0),
                year = character(0),
                week = character(0),
                week_ending_date = character(0)
              ),
              row.names = integer(0),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          ),
          where = list(
            pk = "where_key",
            attribute = c(
              "region",
              "state",
              "city",
              "city_state",
              "status",
              "pop",
              "lat",
              "long"
            ),
            table = structure(
              list(
                where_key = integer(0),
                region = character(0),
                state = character(0),
                city = character(0),
                city_state = character(0),
                status = character(0),
                pop = character(0),
                lat = character(0),
                long = character(0)
              ),
              row.names = integer(0),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          ),
          who = list(
            pk = "who_key",
            attribute = "age",
            table = structure(
              list(who_key = integer(0), age = character(0)),
              row.names = integer(0),
              class = c("tbl_df",
                        "tbl", "data.frame")
            )
          )
        )
      ),
      query = list(
        fact = list(
          mrs_age = list(
            measure = c(all_deaths = "SUM", nrow_agg_sq = "SUM"),
            new = NULL
          ),
          mrs_cause = list(
            measure = c(all_deaths = "SUM", nrow_agg_sq = "SUM"),
            new = NULL
          )
        ),
        dimension = list(
          where = list(attribute = c("region",
                                     "state")),
          when = list(
            attribute = "year",
            filter = c("alist",
                       "week <= \" 3\" & year >= \"2010\"")
          )
        )
      )
    ), class = "star_query")
  })



  expect_equal({
    mrs_db_2
  },
  {
    structure(
      list(
        name = "mrs",
        operations = structure(list(
          operations = structure(
            list(
              operation = character(0),
              name = character(0),
              details = character(0),
              details2 = character(0),
              order = integer(0)
            ),
            class = "data.frame",
            row.names = integer(0)
          )
        ), class = "star_operation"),
        lookup_tables = list(),
        schemas = list(),
        refresh = list(),
        deploy = list(),
        facts = list(
          mrs_age = structure(
            list(
              name = "mrs_age",
              surrogate_keys = c("when_key", "where_key"),
              agg = c(all_deaths = "SUM",
                      nrow_agg_sq = "SUM"),
              dim_int_names = c("when", "where"),
              table = structure(
                list(
                  when_key = c(1L, 1L, 2L, 2L,
                               2L, 2L, 3L, 4L, 4L, 5L, 5L, 6L, 7L, 7L, 7L, 7L, 7L),
                  where_key = c(
                    2L,
                    8L,
                    4L,
                    6L,
                    9L,
                    14L,
                    13L,
                    3L,
                    15L,
                    5L,
                    11L,
                    7L,
                    1L,
                    3L,
                    10L,
                    12L,
                    14L
                  ),
                  all_deaths = c(
                    1L,
                    50L,
                    65L,
                    115L,
                    165L,
                    44L,
                    122L,
                    23L,
                    72L,
                    327L,
                    70L,
                    175L,
                    20L,
                    74L,
                    98L,
                    58L,
                    14L
                  ),
                  nrow_agg_sq = c(1L,
                                  5L, 4L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 5L, 10L,
                                  5L, 5L, 5L)
                ),
                class = c("tbl_df", "tbl", "data.frame"),
                row.names = c(NA,-17L)
              )
            ),
            class = "fact_table"
          ),
          mrs_cause = structure(
            list(
              name = "mrs_cause",
              surrogate_keys = c("when_key",
                                 "where_key"),
              agg = c(all_deaths = "SUM", nrow_agg_sq = "SUM"),
              dim_int_names = c("when", "where"),
              table = structure(
                list(
                  when_key = c(1L, 1L, 2L, 2L, 2L, 2L, 3L, 4L, 4L,
                               5L, 5L, 6L, 7L, 7L, 7L, 7L, 7L),
                  where_key = c(
                    2L,
                    8L,
                    4L,
                    6L,
                    9L,
                    14L,
                    13L,
                    3L,
                    15L,
                    5L,
                    11L,
                    7L,
                    1L,
                    3L,
                    10L,
                    12L,
                    14L
                  ),
                  all_deaths = c(
                    1L,
                    50L,
                    65L,
                    116L,
                    165L,
                    44L,
                    122L,
                    23L,
                    72L,
                    327L,
                    70L,
                    175L,
                    20L,
                    74L,
                    98L,
                    58L,
                    14L
                  ),
                  nrow_agg_sq = c(1L, 1L,
                                  1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 1L,
                                  1L, 1L)
                ),
                class = c("tbl_df", "tbl", "data.frame"),
                row.names = c(NA,-17L)
              )
            ),
            class = "fact_table"
          )
        ),
        dimensions = list(
          where = structure(
            list(
              name = "where",
              surrogate_key = "where_key",
              table = structure(
                list(
                  where_key = 1:15,
                  region = c(
                    "1",
                    "2",
                    "2",
                    "3",
                    "3",
                    "4",
                    "5",
                    "5",
                    "6",
                    "6",
                    "6",
                    "8",
                    "8",
                    "9",
                    "9"
                  ),
                  state = c(
                    "MA",
                    "NJ",
                    "NY",
                    "IN",
                    "OH",
                    "MO",
                    "FL",
                    "VA",
                    "AL",
                    "KY",
                    "TN",
                    "CO",
                    "NM",
                    "CA",
                    "WA"
                  )
                ),
                row.names = c(NA,-15L),
                class = c("tbl_df", "tbl", "data.frame")
              )
            ),
            class = "dimension_table"
          ),
          when = structure(
            list(
              name = "when",
              surrogate_key = "when_key",
              table = structure(
                list(
                  when_key = 1:7,
                  year = c("2010",
                           "2011", "2012", "2013", "2014", "2015", "2016")
                ),
                row.names = c(NA,-7L),
                class = c("tbl_df", "tbl", "data.frame")
              )
            ),
            class = "dimension_table"
          )
        ),
        rpd = list(),
        geo = list()
      ),
      class = "star_database"
    )
  })



  #############################################################
})
