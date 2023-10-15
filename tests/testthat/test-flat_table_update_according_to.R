test_that("flat_table() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris))
  }, {
    f2 <- flat_table('iris2', head(iris))
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("flat_table() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', head(ft_num))
  }, {
    f2 <- flat_table('ft_num2', head(ft_num))
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("join_lookup_table() update_according_to", {
  expect_equal({
    lookup <- flat_table('iris', iris) |>
      lookup_table(
        measures = c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Petal.Width"
        ),
        measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
      )
    f1 <- lookup |>
      join_lookup_table(lookup = lookup)
  }, {
    f2 <- flat_table('iris', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("add_custom_column() update_according_to", {
  expect_equal({
    f <- function(table) {
      paste0(table$City, '-', table$State)
    }
    f1 <- flat_table('ft_num', ft_num) |>
      add_custom_column(name = 'city_state', definition = f)
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("replace_attribute_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_attribute_values(
        attributes = 'Species',
        old = c('setosa'),
        new = c('versicolor')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("replace_attribute_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_attribute_values(
        attributes = 'Species',
        old = c('setosa', 'virginica', 'versicolor'),
        new = c('flor')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("replace_attribute_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      replace_attribute_values(
        attributes = c('Year', 'WEEK'),
        old = c('1962', '2'),
        new = c('1932', '12')
      )
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("replace_empty_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris)
    f1$table[1, 1] <- NA
    f1$table[2, 1] <- ""
    f1 <- f1 |>
      replace_empty_values()
  }, {
    f2 <- flat_table('iris2', iris)
    f2$table[1, 1] <- NA
    f2$table[2, 1] <- ""
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("replace_string() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_string(string = c('set'),
                     replacement = c('Set'))
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_attributes() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_attributes(attributes = c('Year', 'WEEK'))
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_instances() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances(
        attributes = c('Species'),
        values = c('versicolor', 'virginica')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_instances() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances(
        not = TRUE,
        attributes = c('Species'),
        values = c('versicolor', 'virginica')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_instances() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_instances(attributes = c('Year', 'WEEK'),
                       values = list(c('1962', '2'), c('1964', '2')))
  }, {
    f2 <- flat_table('ft_num', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("transform_attribute_format() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_attribute(measures = "Sepal.Length", decimal_places = 1) |>
      transform_attribute_format(attributes = "Sepal.Length",
                                 width = 5,
                                 decimal_places = 2)
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("transform_to_attribute() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris)
    f1$table[1, 2] <- 4000
    f1 <- f1 |>
      transform_to_attribute(measures = "Sepal.Length",
                             width = 3,
                             decimal_places = 2)
  }, {
    f2 <- flat_table('iris2', iris)
    f2$table[1, 2] <- 4000
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("transform_to_measure() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris)
    f1$table[1, 2] <- 4000
    f1 <- f1 |>
      transform_to_attribute(measures = "Sepal.Length",
                             width = 3,
                             decimal_places = 2) |>
      transform_to_measure(attributes = "Sepal.Length",
                           k_sep = ",",
                           decimal_sep = ".")
  }, {
    f2 <- flat_table('iris2', iris)
    f2$table[1, 2] <- 4000
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_instances_by_comparison() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances_by_comparison(attributes = 'Species',
                                     comparisons = '>=',
                                     values = 'v')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_instances_by_comparison() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_instances_by_comparison(
        not = FALSE,
        attributes = c('Year', 'Year', 'WEEK'),
        comparisons = c('>=', '<=', '=='),
        values = c('1962', '1964', '2')
      )
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_instances_by_comparison() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_instances_by_comparison(
        not = FALSE,
        attributes = c('Year', 'Year', 'WEEK'),
        comparisons = c('>=', '<=', '=='),
        values = list(c('1962', '1964', '2'),
                      c('1962', '1964', '4'))
      )
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("select_measures() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_measures(measures = c('Sepal.Length', 'Sepal.Width'))
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("separate_measures() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris, 2)) |>
      separate_measures(measures = list(
        c('Petal.Length'),
        c('Petal.Width'),
        c('Sepal.Length'),
        c('Sepal.Width')
      ),
      names = c('PL', 'PW', 'SL', 'SW'))
    f1[[1]]
  }, {
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[1]])
    f2$star_database
  })
})

test_that("separate_measures() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris, 2)) |>
      separate_measures(measures = list(
        c('Petal.Length'),
        c('Petal.Width'),
        c('Sepal.Length'),
        c('Sepal.Width')
      ),
      names = c('PL', 'PW', 'SL', 'SW'))
    f1[[3]]
  }, {
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[3]])
    f2$star_database
  })
})

test_that("separate_measures() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris, 2)) |>
      separate_measures(measures = list(
        c('Petal.Length', 'Petal.Width'),
        c('Sepal.Length', 'Sepal.Width')
      ),
      names = c('PL-PW', 'SL-SW'))
    f1[[1]]
  }, {
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[1]])
    f2$star_database
  })
})

test_that("separate_measures() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris, 2)) |>
      separate_measures(measures = list(
        c('Petal.Length', 'Petal.Width'),
        c('Sepal.Length', 'Sepal.Width')
      ),
      names = c('PL-PW', 'SL-SW'))
    f1[[2]]
  }, {
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[2]])
    f2$star_database
  })
})


test_that("set_attribute_names() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      set_attribute_names(old = c('Species'),
                          new = c('species'))
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("set_measure_names() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      set_measure_names(
        old = c(
          'Petal.Length',
          'Petal.Width',
          'Sepal.Length',
          'Sepal.Width'
        ),
        new = c('pl', 'pw', 'ls', 'sw')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("snake_case() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris)) |>
      snake_case()
  }, {
    f2 <- flat_table('iris2', head(iris))
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("transform_from_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id') |>
      transform_from_values(attribute = 'Characteristic')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("transform_to_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("transform_to_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("lookup_table() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      lookup_table(
        measures = c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Petal.Width"
        ),
        measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("replace_unknown_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris)
    f1$table[1, 1] <- NA
    f1$table[2, 1] <- ""
    f1 <- f1 |>
      replace_empty_values() |>
      replace_unknown_values(value = "Not available")
  }, {
    f2 <- flat_table('iris2', iris)
    f2$table[1, 1] <- NA
    f2$table[2, 1] <- ""
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("remove_instances_without_measures() update_according_to", {
  expect_equal({
    iris2 <- iris
    iris2[10, 1:4] <- NA
    f1 <- flat_table('iris', iris2) |>
      remove_instances_without_measures()
  }, {
    f2 <- flat_table('iris2', iris2)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})



test_that("star_database() update_according_to", {
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
    f1 <- flat_table('ft_num', ft_num) |>
      as_star_database(s)
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  }
  )
})

test_that("star_database() update_according_to", {
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
    f1 <- flat_table('ft_num', ft_num) |>
      as_star_database(s)
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  }
  )
})

test_that("snake_case() update_according_to", {
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
    f1 <- flat_table('ft_num', ft_num) |>
      as_star_database(s) |>
      snake_case()
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  }
)
})

test_that("set_attribute_names() update_according_to",
          {
            expect_equal({
              f1 <- flat_table('ft_num', ft_num) |>
                as_star_database(mrs_cause_schema) |>
                set_attribute_names(name = "where",
                                    new = c("Region",
                                            "State",
                                            "City"))
            }, {
              f2 <- flat_table('ft_num2', ft_num)
              f2 <- f2 |>
                update_according_to(f1)
              f2$star_database
            })
          })


test_that("set_attribute_names() update_according_to",
          {
            expect_equal({
              f1 <- flat_table('ft_num', ft_num) |>
                as_star_database(mrs_cause_schema) |>
                set_attribute_names(
                  name = "where",
                  old = c("REGION"),
                  new = c("Region")
                )
            }, {
              f2 <- flat_table('ft_num2', ft_num)
              f2 <- f2 |>
                update_according_to(f1)
              f2$star_database
            })
          })


test_that("set_measure_names() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      as_star_database(mrs_cause_schema) |>
      set_measure_names(new = c("Pneumonia and Influenza",
                                "All",
                                "Rows Aggregated"))
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("replace_attribute_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      as_star_database(mrs_cause_schema) |>
      replace_attribute_values(
        "where",
        old = c('1', 'CT', 'Bridgeport'),
        new = c('1', 'CT', 'Hartford')
      ) |>
      group_dimension_instances("where")
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("replace_attribute_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_cause_rpd) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received")) |>
      replace_attribute_values(
        name = "When Available",
        old = c('1962', '11', '1962-03-14'),
        new = c('1962', '3', '1962-01-15')
      ) |>
      group_dimension_instances("When Available")
  }, {
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("role_playing_dimension() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_cause_rpd) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
  }, {
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("role_playing_dimension() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_cause_rpd) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(
        rpd = "When",
        roles = c("When Available", "When Received"),
        rpd_att_names = TRUE
      )
  }, {
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})

test_that("role_playing_dimension() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_cause_rpd) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(
        rpd = "When",
        roles = c("When Available", "When Received"),
        att_names = c("Year", "Week", "Date")
      )
  }, {
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$star_database
  })
})


test_that("group_dimension_instances() update_according_to",
          {
            expect_equal({
              f1 <- flat_table('ft_num', ft_cause_rpd) |>
                as_star_database(mrs_cause_schema_rpd) |>
                replace_attribute_values(
                  name = "When Available",
                  old = c('1962', '11', '1962-03-14'),
                  new = c('1962', '3', '1962-01-15')
                ) |>
                group_dimension_instances(name = "When")
            }, {
              f2 <- flat_table('ft_num2', ft_cause_rpd)
              f2 <- f2 |>
                update_according_to(f1)
              f2$star_database
            })
          })
