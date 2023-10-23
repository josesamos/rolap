test_that("refresh", {
  #############################################################

  mrs_db_original <- mrs_db

  ## ---------------------------------------------------------------------------------------
  mrs_db_age_refresh <- mrs_ft_new |>
    update_according_to(mrs_db, star = "mrs_age")

  ## ---------------------------------------------------------------------------------------
  mrs_db_cause_refresh <- mrs_ft_new |>
    update_according_to(mrs_db, star = "mrs_cause")


  ## ---------------------------------------------------------------------------------------
  new_dimension_instances <- mrs_db_age_refresh |>
    get_new_dimension_instances()


  ## ---------------------------------------------------------------------------------------
  existing_fact_instances_age <- mrs_db_age_refresh |>
    get_existing_fact_instances()

  existing_fact_instances_cause <- mrs_db_cause_refresh |>
    get_existing_fact_instances()


  ## ---------------------------------------------------------------------------------------
  mrs_db_seg <- mrs_db
  mrs_db2 <- mrs_db

  mrs_db <- mrs_db |>
    incremental_refresh(mrs_db_age_refresh) |>
    incremental_refresh(mrs_db_cause_refresh, existing_instances = "group")

  mrs_db2 <- mrs_db2 |>
    incremental_refresh(mrs_db_age_refresh, existing_instances = "delete",
                        replace_transformations = FALSE, 'DONTDELETE') |>
    incremental_refresh(mrs_db_cause_refresh, existing_instances = "delete",
                        replace_transformations = FALSE, 'DONTDELETE')

  ## ---------------------------------------------------------------------------------------
  transform_instance_table <-
    function(instance_df,
             lookup_ft,
             definition_fun,
             star_sch) {
      ft <-
        flat_table(name = 'mrs',
                   instances = instance_df,
                   unknown_value = 'Not available') |>
        transform_to_measure(
          attributes = c(
            'Pneumonia and Influenza Deaths',
            'All Deaths',
            '<1 year (all cause deaths)',
            '1-24 years (all cause deaths)',
            '25-44 years',
            '45-64 years (all cause deaths)',
            '65+ years (all cause deaths)'
          ),
          k_sep = NULL,
          decimal_sep = NULL
        ) |>
        transform_attribute_format(
          attributes = 'WEEK',
          width = 2,
          decimal_places = 0,
          k_sep = ',',
          decimal_sep = '.'
        ) |>
        replace_empty_values(
          attributes = c('Year', 'WEEK', 'Week Ending Date', 'REGION', 'State', 'City'),
          empty_values = NULL
        ) |>
        add_custom_column(name = 'city_state',
                          definition = definition_fun) |>
        replace_attribute_values(
          attributes = c('City', 'city_state'),
          old = c('Wilimington', 'Wilimington DE'),
          new = c('Wilmington', 'Wilmington DE')
        ) |>
        join_lookup_table(fk_attributes = 'city_state',
                          lookup = lookup_ft) |>
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
        ) |>
        separate_measures(
          measures = list(
            c('Pneumonia and Influenza Deaths', 'All Deaths'),
            c(
              '<1 year (all cause deaths)',
              '1-24 years (all cause deaths)',
              '25-44 years',
              '45-64 years (all cause deaths)',
              '65+ years (all cause deaths)'
            )
          ),
          names = c('mrs_cause', 'mrs_age'),
          na_rm = TRUE
        ) |>
        magrittr::extract2('mrs_cause') |>
        snake_case() |>
        as_star_database(schema = star_sch)

      ft
    }


  ## ---------------------------------------------------------------------------------------
  instance_df <- mrs_ft_new |>
    get_table()

  lookup_list <- mrs_db_cause_refresh |>
    get_lookup_tables()

  star_sch <- mrs_db_cause_refresh |>
    get_star_schema()

  # function to define a derived column
  city_state <- function(table) {
    paste0(table$City, ' ', table$State)
  }

  mrs_db_cause_transf <-
    transform_instance_table(
      instance_df = instance_df,
      lookup_ft = lookup_list[['us_cities']],
      definition_fun = city_state,
      star_sch = star_sch
    )


  ## ---------------------------------------------------------------------------------------
  transform_instance_table_2 <-
    function(instance_df,
             lookup_ft,
             definition_fun,
             star_sch) {
      ft <-
        flat_table(name = 'mrs',
                   instances = instance_df,
                   unknown_value = 'Not available') |>
        transform_to_measure(
          attributes = c(
            'Pneumonia and Influenza Deaths',
            'All Deaths',
            '<1 year (all cause deaths)',
            '1-24 years (all cause deaths)',
            '25-44 years',
            '45-64 years (all cause deaths)',
            '65+ years (all cause deaths)'
          ),
          k_sep = NULL,
          decimal_sep = NULL
        ) |>
        transform_attribute_format(
          attributes = 'WEEK',
          width = 2,
          decimal_places = 0,
          k_sep = ',',
          decimal_sep = '.'
        ) |>
        replace_empty_values(
          attributes = c('Year', 'WEEK', 'Week Ending Date', 'REGION', 'State', 'City'),
          empty_values = NULL
        ) |>
        add_custom_column(name = 'city_state',
                          definition = definition_fun) |>
        replace_attribute_values(
          attributes = c('City', 'city_state'),
          old = c('Wilimington', 'Wilimington DE'),
          new = c('Wilmington', 'Wilmington DE')
        ) |>
        join_lookup_table(fk_attributes = 'city_state',
                          lookup = lookup_ft) |>
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
        ) |>
        separate_measures(
          measures = list(
            c('Pneumonia and Influenza Deaths', 'All Deaths'),
            c(
              '<1 year (all cause deaths)',
              '1-24 years (all cause deaths)',
              '25-44 years',
              '45-64 years (all cause deaths)',
              '65+ years (all cause deaths)'
            )
          ),
          names = c('mrs_cause', 'mrs_age'),
          na_rm = TRUE
        ) |>
        magrittr::extract2('mrs_age') |>
        transform_to_values(
          attribute = 'age',
          measure = 'all_deaths',
          id_reverse = NULL,
          na_rm = TRUE
        ) |>
        snake_case() |>
        replace_string(attributes = 'age',
                       string = ' (all cause deaths)',
                       replacement = NULL) |>
        as_star_database(schema = star_sch)

      ft
    }

  star_sch <- mrs_db_age_refresh |>
    get_star_schema()

  mrs_db_age_transf <-
    transform_instance_table_2(
      instance_df = instance_df,
      lookup_ft = lookup_list[['us_cities']],
      definition_fun = city_state,
      star_sch = star_sch
    )


  ## ---------------------------------------------------------------------------------------
  mrs_db_cause_transf_refresh <- mrs_ft_new |>
    update_according_to(mrs_db_seg, star = "mrs_cause", sdb_operations = mrs_db_cause_transf)

  mrs_db_age_transf_refresh <- mrs_ft_new |>
    update_according_to(mrs_db_seg, star = "mrs_age", sdb_operations = mrs_db_age_transf)


  ## ---------------------------------------------------------------------------------------
  mrs_db_seg <- mrs_db_seg |>
    incremental_refresh(mrs_db_age_transf_refresh, replace_transformations = TRUE) |>
    incremental_refresh(
      mrs_db_cause_transf_refresh,
      existing_instances = "group",
      replace_transformations = TRUE
    )


  #############################################################
  expect_equal({
    names(mrs_db_seg$refresh) <- names(mrs_db$refresh)
    mrs_db_seg
  },
  {
    mrs_db
  })


  #############################################################
  expect_equal({
    new_dimension_instances[[2]]
  },
  {
    structure(
      list(
        region = c("1", "5"),
        state = c("MA", "MD"),
        city = c("Boston",
                 "Baltimore"),
        city_state = c("Boston MA", "Baltimore MD"),
        status = c("state capital",
                   "non-capital"),
        pop = c("  567,759", "  602,658"),
        lat = c("42.3",
                "39.3"),
        long = c(" -71.0", " -76.6")
      ),
      row.names = c(NA,-2L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })

  #############################################################
  expect_equal({
    mrs_db_age_refresh_all <- mrs_ft |>
      update_according_to(mrs_db, star = "mrs_cause") |>
      get_star_database()
  },
  {
    mrs_db_original |>
      get_star_database("mrs_cause")

  })


  #############################################################
  expect_equal({
    mrs_db_age_refresh_all <- mrs_ft |>
      update_according_to(mrs_db, star = "mrs_age") |>
      get_star_database()
  },
  {
    mrs_db_original |>
      get_star_database("mrs_age")

  })


  #############################################################
  expect_equal({
    mrs_db2$refresh[[2]]$delete$when
  },
  {
    structure(
      list(
        when_key = c(
          45L,
          175L,
          352L,
          551L,
          731L,
          796L,
          1076L,
          1230L,
          1332L,
          1851L,
          1901L
        )
      ),
      row.names = c(NA,-11L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })

  #############################################################
})
