test_that("flat_table() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', head(iris))
  }, {
    f2 <- flat_table('iris2', head(iris))
    f2 |>
      refresh_new(f1)
  })
})

test_that("flat_table() refresh_new", {
  expect_equal({
    f1 <- flat_table('ft_num', head(ft_num))
  }, {
    f2 <- flat_table('ft_num2', head(ft_num))
    f2 |>
      refresh_new(f1)
  })
})


test_that("join_lookup_table() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})


test_that("add_custom_column() refresh_new", {
  expect_equal({
    f <- function(table) {
      paste0(table$City, '-', table$State)
    }
    f1 <- flat_table('ft_num', ft_num) |>
      add_custom_column(name = 'city_state', definition = f)
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 |>
      refresh_new(f1)
  })
})


test_that("replace_attribute_values() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_attribute_values(
        attributes = 'Species',
        old = c('setosa'),
        new = c('versicolor')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("replace_attribute_values() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_attribute_values(
        attributes = 'Species',
        old = c('setosa', 'virginica', 'versicolor'),
        new = c('flor')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("replace_attribute_values() refresh_new", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      replace_attribute_values(
        attributes = c('Year', 'WEEK'),
        old = c('1962', '2'),
        new = c('1932', '12')
      )
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 |>
      refresh_new(f1)
  })
})

test_that("replace_empty_values() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})


test_that("replace_string() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_string(
        string = c('set'),
        replacement = c('Set')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_attributes() refresh_new", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_attributes(attributes = c('Year', 'WEEK'))
  }, {
    f2 <- flat_table('ft_num2', ft_num)
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_instances() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances(
        attributes = c('Species'),
        values = c('versicolor', 'virginica')
      )
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_instances() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances(not = TRUE,
                       attributes = c('Species'),
                       values = c('versicolor', 'virginica'))
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_instances() refresh_new", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_instances(attributes = c('Year', 'WEEK'),
                       values = list(c('1962', '2'), c('1964', '2')))
  }, {
    f2 <- flat_table('ft_num', ft_num)
    f2 |>
      refresh_new(f1)
  })
})


test_that("transform_attribute_format() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_attribute(measures = "Sepal.Length", decimal_places = 1) |>
      transform_attribute_format(attributes = "Sepal.Length",
                                 width = 5,
                                 decimal_places = 2)
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("transform_to_attribute() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})


test_that("transform_to_measure() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris)
    f1$table[1, 2] <- 4000
    f1 <- f1 |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2
      ) |>
      transform_to_measure(attributes = "Sepal.Length", k_sep = ",", decimal_sep = ".")
  }, {
    f2 <- flat_table('iris2', iris)
    f2$table[1, 2] <- 4000
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_instances_by_comparison() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances_by_comparison(attributes = 'Species',
                                     comparisons = '>=',
                                     values = 'v')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_instances_by_comparison() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_instances_by_comparison() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})


test_that("select_measures() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_measures(measures = c('Sepal.Length', 'Sepal.Width'))
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})

test_that("separate_measures() refresh_new", {
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
    f2 |>
      refresh_new(f1[[1]])
  })
})

test_that("separate_measures() refresh_new", {
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
    f2 |>
      refresh_new(f1[[3]], sel_measure_group = 3)
  })
})

test_that("separate_measures() refresh_new", {
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
    f2 |>
      refresh_new(f1[[1]])
  })
})


test_that("set_attribute_names() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      set_attribute_names(old = c('Species'),
                          new = c('species'))
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("set_measure_names() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})

test_that("snake_case() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', head(iris)) |>
      snake_case()
  }, {
    f2 <- flat_table('iris2', head(iris))
    f2 |>
      refresh_new(f1)
  })
})

test_that("transform_from_values() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id') |>
      transform_from_values(attribute = 'Characteristic')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})

test_that("transform_to_values() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})

test_that("transform_to_values() refresh_new", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id')
  }, {
    f2 <- flat_table('iris2', iris)
    f2 |>
      refresh_new(f1)
  })
})


test_that("lookup_table() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})


test_that("replace_unknown_values() refresh_new", {
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
    f2 |>
      refresh_new(f1)
  })
})


test_that("remove_instances_without_measures() refresh_new", {
  expect_equal({
    iris2 <- iris
    iris2[10, 1:4] <- NA
    f1 <- flat_table('iris', iris2) |>
      remove_instances_without_measures()
  }, {
    f2 <- flat_table('iris2', iris2)
    f2 |>
      refresh_new(f1)
  })
})




