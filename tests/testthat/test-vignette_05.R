test_that("flat table operations", {
  ## ---------------------------------------------------------------------------------------
  usc_ft <-
    flat_table(name = 'us_cities', instances = maps::us.cities)

  ## ---------------------------------------------------------------------------------------
  capital_status <- data.frame(
    code = c('0', '1', '2'),
    status = c('non-capital', 'capital', 'state capital')
  )

  cs_ft <-
    flat_table(name = 'capital_status', instances = capital_status)

  ## ---------------------------------------------------------------------------------------
  mrs_ft <- mrs_ft |>
    transform_to_measure(
      attributes = c(
        'Pneumonia and Influenza Deaths',
        'All Deaths',
        '<1 year (all cause deaths)',
        '1-24 years (all cause deaths)',
        '25-44 years',
        '45-64 years (all cause deaths)',
        '65+ years (all cause deaths)'
      )
    )

  ## ---------------------------------------------------------------------------------------
  mrs_ft <- mrs_ft |>
    transform_attribute_format(attributes = c('WEEK'),
                               width = 2)

  ## ---------------------------------------------------------------------------------------
  usc_ft <- usc_ft |>
    transform_to_attribute(measures = 'capital') |>
    transform_to_attribute(measures = 'pop',
                           width = 5) |>
    transform_to_attribute(measures = c('lat', 'long'),
                           width = 2,
                           decimal_places = 1)

  ## ---------------------------------------------------------------------------------------
  cs_ft <- cs_ft |>
    lookup_table(pk_attributes = 'code')

  ## ---------------------------------------------------------------------------------------
  usc_ft <- usc_ft |>
    join_lookup_table(fk_attributes = 'capital', lookup = cs_ft)

  ## ---------------------------------------------------------------------------------------
  usc_ft <- usc_ft |>
    lookup_table(pk_attributes = 'name')

  ## ---------------------------------------------------------------------------------------
  # function to define a derived column
  city_state <- function(table) {
    paste0(table$City, ' ', table$State)
  }

  mrs_ft_TMP <- mrs_ft |>
    add_custom_column(name = 'city_state', definition = city_state)

  ## ---------------------------------------------------------------------------------------
  result_lookup <- mrs_ft_TMP |>
    check_lookup_table(fk_attributes = 'city_state', lookup = usc_ft)

  ## ---------------------------------------------------------------------------------------
  mrs_ft <- mrs_ft |>
    replace_empty_values()

  ## ---------------------------------------------------------------------------------------
  mrs_ft <- mrs_ft |>
    add_custom_column(name = 'city_state', definition = city_state)

  ## ---------------------------------------------------------------------------------------
  usc_ft <- usc_ft |>
    replace_attribute_values(
      attributes = 'name',
      old = c('WASHINGTON DC'),
      new = c('Washington DC')
    )

  mrs_ft <- mrs_ft |>
    replace_attribute_values(
      attributes = c('City', 'city_state'),
      old = c('Wilimington', 'Wilimington DE'),
      new = c('Wilmington', 'Wilmington DE')
    )

  ## ---------------------------------------------------------------------------------------
  check_res <- mrs_ft |>
    check_lookup_table(fk_attributes = 'city_state', lookup = usc_ft)

  ## ---------------------------------------------------------------------------------------
  mrs_ft <- mrs_ft |>
    join_lookup_table(fk_attributes = 'city_state', lookup = usc_ft)

  ## ---------------------------------------------------------------------------------------
  mrs_ft <- mrs_ft |>
    select_attributes(
      attributes = c(
        'Year',
        'WEEK',
        'Week Ending Date',
        'REGION',
        'State',
        'City',
        'city_state',
        'status',
        'pop',
        'lat',
        'long'
      )
    )

  ## ---------------------------------------------------------------------------------------
  l_mrs_ft <- mrs_ft |>
    separate_measures(measures = list(
      c('Pneumonia and Influenza Deaths',
        'All Deaths'),
      c(
        '<1 year (all cause deaths)',
        '1-24 years (all cause deaths)',
        '25-44 years',
        '45-64 years (all cause deaths)',
        '65+ years (all cause deaths)'
      )
    ),
    names = c('mrs_cause', 'mrs_age'))

  mrs_cause_ft <- l_mrs_ft[['mrs_cause']]
  mrs_age_ft <- l_mrs_ft[['mrs_age']]

  ## ---------------------------------------------------------------------------------------
  mrs_cause_ft <- mrs_cause_ft |>
    snake_case()

  ## ---------------------------------------------------------------------------------------
  mrs_age_ft <- mrs_age_ft |>
    transform_to_values(attribute = 'age',
                        measure = 'all_deaths')

  ## ---------------------------------------------------------------------------------------
  mrs_age_ft <- mrs_age_ft |>
    snake_case()

  ## ---------------------------------------------------------------------------------------
  mrs_age_ft <- mrs_age_ft |>
    replace_string(
      attributes = 'age',
      string = ' (all cause deaths)',
      replacement = ''
    )

  ## ---------------------------------------------------------------------------------------
  mrs_age_ft_TMP <- mrs_age_ft |>
    transform_from_values(
      attribute = 'age'
    )

  ## ---------------------------------------------------------------------------------------
  when <- dimension_schema(
    name = 'when',
    attributes = c(
      'year',
      'week',
      'week_ending_date'
    )
  )
  where <- dimension_schema(
    name = "where",
    attributes = c(
      'region',
      'state',
      'city',
      'city_state',
      'status',
      'pop',
      'lat',
      'long'
    )
  )
  s_cause <- star_schema() |>
    define_facts(fact_schema(
      name = 'mrs_cause',
      measures = c('pneumonia_and_influenza_deaths', 'all_deaths')
    )) |>
    define_dimension(when) |>
    define_dimension(where)

  ## ---------------------------------------------------------------------------------------
  mrs_cause_db <- mrs_cause_ft |>
    as_star_database(s_cause)

  ## ---------------------------------------------------------------------------------------
  who <- dimension_schema(
    name = 'who',
    attributes = c(
      'age'
    )
  )
  s_age <- star_schema() |>
    define_facts(fact_schema(
      name = 'mrs_age',
      measures = c('all_deaths')
    )) |>
    define_dimension(when) |>
    define_dimension(where) |>
    define_dimension(who)

  ## ---------------------------------------------------------------------------------------
  mrs_age_db <- mrs_age_ft |>
    as_star_database(s_age)

  ## ----example5---------------------------------------------------------------------------
  mrs_db_2 <- constellation("mrs", mrs_cause_db, mrs_age_db)

  #############################################################
  expect_equal(cs_ft,
               structure(
                 list(
                   name = "capital_status",
                   table = structure(
                     list(
                       code = c("0", "1", "2"),
                       status = c("non-capital", "capital",
                                  "state capital")
                     ),
                     row.names = c(NA, -3L),
                     class = c("tbl_df",
                               "tbl", "data.frame")
                   ),
                   unknown_value = "___UNKNOWN___",
                   operations = structure(list(
                     operations = structure(
                       list(
                         operation = c("flat_table", "lookup_table"),
                         name = c("capital_status<|>___UNKNOWN___", "code"),
                         details = c("code<|>status",
                                     "|"),
                         details2 = c("", "|"),
                         order = c(1, 2)
                       ),
                       row.names = c(NA, -2L),
                       class = "data.frame"
                     )
                   ), class = "star_operation"),
                   pk_attributes = "code",
                   lookup_tables = list(),
                   attributes = c("code",
                                  "status"),
                   measures = NULL
                 ),
                 class = "flat_table"
               ))

  #############################################################
  expect_equal({
    head(unique(sort(mrs_db_2$dimensions$when$table$week)), 12)
  },
  {
    c(" 1", " 2", " 3", " 4", " 5", " 6", " 7", " 8", " 9", "10",
      "11", "12")
  })

  #############################################################
  expect_equal({
    mrs_db_2 |>
      get_star_database("mrs_age")
  },
  {
    mrs_age_db
  })

  #############################################################
  expect_equal({
    mrs_db_2 |>
      get_star_database("mrs_cause")
  },
  {
    mrs_cause_db
  })

  #############################################################
  expect_equal({
    facts <- names(mrs_db_2$facts)
    names <- NULL
    for (f in facts) {
      names <- c(names, names(mrs_db_2$facts[[f]]$table))
    }
    names
  },
  {
    c("when_key", "where_key", "pneumonia_and_influenza_deaths",
      "all_deaths", "nrow_agg", "when_key", "where_key", "who_key",
      "all_deaths", "nrow_agg")
  }
  )

  #############################################################
  expect_equal({
    mrs_db_2
  },
  {
    mrs_db
  }
  )
  #############################################################
})
