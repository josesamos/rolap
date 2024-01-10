test_that("multistar", {
  dm_mrs_age <- starschemar::dimensional_model() |>
    starschemar::define_fact(
      name = "mrs_age",
      measures = c(
        "Deaths"
      ),
      agg_functions = c(
        "SUM"
      ),
      nrow_agg = "nrow_agg"
    ) |>
    starschemar::define_dimension(
      name = "when",
      attributes = c(
        "Week Ending Date",
        "WEEK",
        "Year"
      )
    ) |>
    starschemar::define_dimension(
      name = "when_available",
      attributes = c(
        "Data Availability Date",
        "Data Availability Week",
        "Data Availability Year"
      )
    ) |>
    starschemar::define_dimension(
      name = "where",
      attributes = c(
        "REGION",
        "State",
        "City"
      )
    ) |>
    starschemar::define_dimension(
      name = "who",
      attributes = c(
        "Age Range"
      )
    )

  st_mrs_age <- starschemar::star_schema(starschemar::mrs_age, dm_mrs_age) |>
    starschemar::star_schema_as_multistar()

  dm_mrs_age_2 <- star_schema() |>
    define_facts(
      name = "mrs_age",
      measures = c(
        "Deaths"
      ),
      agg_functions = c(
        "SUM"
      ),
      nrow_agg = "nrow_agg"
    ) |>
    define_dimension(
      name = "when",
      attributes = c(
        "Week Ending Date",
        "WEEK",
        "Year"
      )
    ) |>
    define_dimension(
      name = "when_available",
      attributes = c(
        "Data Availability Date",
        "Data Availability Week",
        "Data Availability Year"
      )
    ) |>
    define_dimension(
      name = "where",
      attributes = c(
        "REGION",
        "State",
        "City"
      )
    ) |>
    define_dimension(
      name = "who",
      attributes = c(
        "Age Range"
      )
    )

  st_mrs_age_2 <- star_database(dm_mrs_age_2, starschemar::mrs_age) |>
    as_multistar()

  s1 <- st_mrs_age
  s2 <- st_mrs_age_2



  expect_equal(sort(names(s1$fact$mrs_age)),
               sort(names(s2$fact$mrs_age)))

  expect_equal(nrow(s1$fact$mrs_age),
               nrow(s2$fact$mrs_age))

  expect_equal(sort(names(s1$dimension)),
               sort(names(s2$dimension)))

  expect_equal(sort(names(s1$dimension$when_available)),
               sort(names(s2$dimension$when_available)))

  expect_equal(sort(names(s1$dimension$where)),
               sort(names(s2$dimension$where)))

  expect_equal(sort(names(s1$dimension$who)),
               sort(names(s2$dimension$who)))

  expect_equal(sort(names(s1$dimension$when)),
               sort(names(s2$dimension$when)))

  expect_equal(nrow(s1$dimension$when_available),
               nrow(s2$dimension$when_available))

  expect_equal(nrow(s1$dimension$where),
               nrow(s2$dimension$where))

  expect_equal(nrow(s1$dimension$who),
               nrow(s2$dimension$who))

  expect_equal(nrow(s1$dimension$when),
               nrow(s2$dimension$when))
})
