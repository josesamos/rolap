test_that("rpd", {
  #############################################################

  ## ----------------------------------------------------------------------------------------------------------
  when <- dimension_schema(name = "When",
                           attributes = c("Year",
                                          "WEEK",
                                          "Week Ending Date"))
  when_available <- dimension_schema(
    name = "When Available",
    attributes = c(
      "Data Availability Year",
      "Data Availability Week",
      "Data Availability Date"
    )
  )
  where <- dimension_schema(name = "where",
                            attributes = c("REGION",
                                           "State",
                                           "City"))
  s <- star_schema() |>
    define_facts(fact_schema(
      name = "MRS Cause",
      measures = c("Pneumonia and Influenza Deaths",
                   "All Deaths")
    )) |>
    define_dimension(when) |>
    define_dimension(when_available) |>
    define_dimension(dimension_schema(
      name = "When Received",
      attributes = c("Reception Year",
                     "Reception Week",
                     "Reception Date")
    )) |>
    define_dimension(where)


  ## ----------------------------------------------------------------------------------------------------------
  db <- star_database(s, ft_cause_rpd) |>
    snake_case()


  ## ----------------------------------------------------------------------------------------------------------
  db_1 <- db |>
    role_playing_dimension(
      rpd = "when",
      roles = c("when_available", "when_received")
    )


  ## ----------------------------------------------------------------------------------------------------------
  db_2 <- db |>
    role_playing_dimension(
      rpd = "when",
      roles = c("when_available", "when_received"),
      rpd_att_names = TRUE
    )


  ## ----------------------------------------------------------------------------------------------------------
  s_2 <- star_schema() |>
    define_facts(fact_schema(
      name = "MRS Age",
      measures = c(
        "Deaths"
      )
    )) |>
    define_dimension(when) |>
    define_dimension(when_available) |>
    define_dimension(dimension_schema(
      name = "When Arrived",
      attributes = c(
        "Arrival Year",
        "Arrival Week",
        "Arrival Date"
      )
    )) |>
    define_dimension(dimension_schema(
      name = "Who",
      attributes = c(
        "Age Range"
      )
    )) |>
    define_dimension(where)


  ## ----------------------------------------------------------------------------------------------------------
  db_3 <- star_database(s_2, ft_age_rpd) |>
    role_playing_dimension(
      rpd = "When Arrived",
      roles = c("When Available"),
      att_names = c("Year", "Week", "Week Ending Date")
    ) |>
    snake_case()


  ## ----------------------------------------------------------------------------------------------------------
  ct <- constellation("MRS", db_2, db_3)


  ## ----------------------------------------------------------------------------------------------------------
  rpd_names <- ct  |>
    get_role_playing_dimension_names()

  names <- names(ct$dimensions$when$table)


  #############################################################
  expect_equal({
    rpd_names
  },
  {
    list(rpd_1 = c("when", "when_arrived", "when_available", "when_received"))
  })



  #############################################################
  expect_equal({
    ct$dimensions$when$table
  },
  {
    d2 <- ct$dimensions$when_arrived$table
    names(d2) <- names
    d2
  })



  #############################################################
  expect_equal({
    ct$dimensions$when$table
  },
  {
    d2 <- ct$dimensions$when_available$table
    names(d2) <- names
    d2
  })



  #############################################################
  expect_equal({
    ct$dimensions$when$table
  },
  {
    d2 <- ct$dimensions$when_received$table
    names(d2) <- names
    d2
  })


  #############################################################
})
