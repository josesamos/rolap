
test_that("flat_table() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris))
    f2 <- flat_table('iris2', head(iris))
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df, lookup_ft) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    lookup_table(",
      "      pk_attributes = 'Species',",
      "      attributes = NULL,",
      "      attribute_agg = NULL,",
      "      measures = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),",
      "      measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')",
      "    ) |>",
      "    join_lookup_table(",
      "      fk_attributes = 'Species',",
      "      lookup = lookup_ft",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, lookup_ft)"
    )
  })
})

test_that("add_custom_column() update_according_to", {
  expect_equal({
    f <- function(table) {
      paste0(table$City, '-', table$State)
    }
    f1 <- flat_table('ft_num', ft_num) |>
      add_custom_column(name = 'city_state', definition = f)
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df, definition_fun) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    add_custom_column(",
      "      name = 'city_state',",
      "      definition = definition_fun",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, definition_fun)"
    )
  })
})

test_that("replace_attribute_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_attribute_values(
        attributes = 'Species',
        old = 'setosa',
        new = 'versicolor'
      )
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    replace_attribute_values(",
      "      attributes = 'Species',",
      "      old = 'setosa',",
      "      new = 'versicolor'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("replace_attribute_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_attribute_values(
        attributes = 'Species',
        old = c('setosa', 'virginica', 'versicolor'),
        new = 'flor'
      )
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    replace_attribute_values(",
      "      attributes = 'Species',",
      "      old = c('setosa', 'virginica', 'versicolor'),",
      "      new = 'flor'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    replace_attribute_values(",
      "      attributes = c('Year', 'WEEK'),",
      "      old = c('1962', '2'),",
      "      new = c('1932', '12')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})

test_that("replace_empty_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris)
    f1$table[1, 1] <- NA
    f1$table[2, 1] <- ""
    f1 <- f1 |>
      replace_empty_values()
    f2 <- flat_table('iris2', iris)
    f2$table[1, 1] <- NA
    f2$table[2, 1] <- ""
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    replace_empty_values(",
      "      attributes = 'Species',",
      "      empty_values = NULL",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("replace_string() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      replace_string(string = c('set'),
                     replacement = c('Set'))
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    replace_string(",
      "      attributes = 'Species',",
      "      string = 'set',",
      "      replacement = 'Set'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("select_attributes() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_attributes(attributes = c('Year', 'WEEK'))
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_attributes(",
      "      attributes = c('Year', 'WEEK')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("select_instances() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances(
        attributes = 'Species',
        values = c('versicolor', 'virginica')
      )
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_instances(",
      "      not = FALSE,",
      "      attributes = 'Species',",
      "      values = c('versicolor', 'virginica')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("select_instances() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances(
        not = TRUE,
        attributes = 'Species',
        values = c('versicolor', 'virginica')
      )
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_instances(",
      "      not = TRUE,",
      "      attributes = 'Species',",
      "      values = c('versicolor', 'virginica')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("select_instances() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      select_instances(attributes = c('Year', 'WEEK'),
                       values = list(c('1962', '2'), c('1964', '2')))
    f2 <- flat_table('ft_num', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_instances(",
      "      not = FALSE,",
      "      attributes = c('Year', 'WEEK'),",
      "      values = list('1' = c('1962', '2'), '2' = c('1964', '2'))",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("transform_attribute_format() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_attribute(measures = "Sepal.Length", decimal_places = 1) |>
      transform_attribute_format(attributes = "Sepal.Length",
                                 width = 5,
                                 decimal_places = 2)
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    transform_to_attribute(",
      "      measures = 'Sepal.Length',",
      "      width = 1,",
      "      decimal_places = 1,",
      "      k_sep = ',',",
      "      decimal_sep = '.'",
      "    ) |>",
      "    transform_attribute_format(",
      "      attributes = 'Sepal.Length',",
      "      width = 5,",
      "      decimal_places = 2,",
      "      k_sep = ',',",
      "      decimal_sep = '.',",
      "      space_filling = TRUE",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', iris)
    f2$table[1, 2] <- 4000
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    transform_to_attribute(",
      "      measures = 'Sepal.Length',",
      "      width = 3,",
      "      decimal_places = 2,",
      "      k_sep = ',',",
      "      decimal_sep = '.'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', iris)
    f2$table[1, 2] <- 4000
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    transform_to_attribute(",
      "      measures = 'Sepal.Length',",
      "      width = 3,",
      "      decimal_places = 2,",
      "      k_sep = ',',",
      "      decimal_sep = '.'",
      "    ) |>",
      "    transform_to_measure(",
      "      attributes = 'Sepal.Length',",
      "      k_sep = ',',",
      "      decimal_sep = '.'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("select_instances_by_comparison() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_instances_by_comparison(attributes = 'Species',
                                     comparisons = '>=',
                                     values = 'v')
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_instances_by_comparison(",
      "      not = FALSE,",
      "      attributes = 'Species',",
      "      comparisons = '>=',",
      "      values = 'v'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_instances_by_comparison(",
      "      not = FALSE,",
      "      attributes = c('Year', 'Year', 'WEEK'),",
      "      comparisons = c('>=', '<=', '=='),",
      "      values = c('1962', '1964', '2')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'ft_num',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_instances_by_comparison(",
      "      not = FALSE,",
      "      attributes = list(c('Year', 'Year', 'WEEK'), c('Year', 'Year', 'WEEK')),",
      "      comparisons = list(c('>=', '<=', '=='), c('>=', '<=', '==')),",
      "      values = list(c('1962', '1964', '2'), c('1962', '1964', '4'))",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})

test_that("select_measures() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      select_measures(measures = c('Sepal.Length', 'Sepal.Width'))
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    select_measures(",
      "      measures = c('Sepal.Length', 'Sepal.Width'),",
      "      na_rm = TRUE",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[1]])
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    separate_measures(",
      "      measures = list('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'),",
      "      names = c('PL', 'PW', 'SL', 'SW'),",
      "      na_rm = TRUE",
      "    ) |>",
      "    magrittr::extract2('PL')",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[3]])
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    separate_measures(",
      "      measures = list('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'),",
      "      names = c('PL', 'PW', 'SL', 'SW'),",
      "      na_rm = TRUE",
      "    ) |>",
      "    magrittr::extract2('SL')",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[1]])
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    separate_measures(",
      "      measures = list(c('Petal.Length', 'Petal.Width'), c('Sepal.Length', 'Sepal.Width')),",
      "      names = c('PL-PW', 'SL-SW'),",
      "      na_rm = TRUE",
      "    ) |>",
      "    magrittr::extract2('PL-PW')",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', head(iris, 2))
    f2 <- f2 |>
      update_according_to(f1[[2]])
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    separate_measures(",
      "      measures = list(c('Petal.Length', 'Petal.Width'), c('Sepal.Length', 'Sepal.Width')),",
      "      names = c('PL-PW', 'SL-SW'),",
      "      na_rm = TRUE",
      "    ) |>",
      "    magrittr::extract2('SL-SW')",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("set_attribute_names() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      set_attribute_names(old = 'Species',
                          new = c('species'))
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    set_attribute_names(",
      "      old = 'Species',",
      "      new = 'species'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    set_measure_names(",
      "      old = c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width'),",
      "      new = c('pl', 'pw', 'ls', 'sw')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})

test_that("snake_case() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', head(iris)) |>
      snake_case()
    f2 <- flat_table('iris2', head(iris))
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    snake_case(",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})

test_that("transform_from_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id') |>
      transform_from_values(attribute = 'Characteristic')
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    transform_to_values(",
      "      attribute = 'Characteristic',",
      "      measure = 'Value',",
      "      id_reverse = 'id',",
      "      na_rm = TRUE",
      "    ) |>",
      "    transform_from_values(",
      "      attribute = 'Characteristic'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})

test_that("transform_to_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value')
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    transform_to_values(",
      "      attribute = 'Characteristic',",
      "      measure = 'Value',",
      "      id_reverse = NULL,",
      "      na_rm = TRUE",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})

test_that("transform_to_values() update_according_to", {
  expect_equal({
    f1 <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id')
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    transform_to_values(",
      "      attribute = 'Characteristic',",
      "      measure = 'Value',",
      "      id_reverse = 'id',",
      "      na_rm = TRUE",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', iris)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    lookup_table(",
      "      pk_attributes = 'Species',",
      "      attributes = NULL,",
      "      attribute_agg = NULL,",
      "      measures = c('Sepal.Length', 'Sepal.Width', 'Petal.Length', 'Petal.Width'),",
      "      measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('iris2', iris)
    f2$table[1, 1] <- NA
    f2$table[2, 1] <- ""
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    replace_empty_values(",
      "      attributes = 'Species',",
      "      empty_values = NULL",
      "    ) |>",
      "    replace_unknown_values(",
      "      attributes = 'Species',",
      "      value = 'Not available'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
  })
})


test_that("remove_instances_without_measures() update_according_to", {
  expect_equal({
    iris2 <- iris
    iris2[10, 1:4] <- NA
    f1 <- flat_table('iris', iris2) |>
      remove_instances_without_measures()
    f2 <- flat_table('iris2', iris2)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
  }, {
    c(
      "transform_instance_table <- function(instance_df) {",
      "  ft <- ",
      "    flat_table(",
      "      name = 'iris',",
      "      instances = instance_df,",
      "      unknown_value = '___UNKNOWN___'",
      "    ) |>",
      "    remove_instances_without_measures(",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df)"
    )
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
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
  })
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
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
  })
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
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "    snake_case(",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
  })
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
              f2 <- flat_table('ft_num2', ft_num)
              f2 <- f2 |>
                update_according_to(f1)
              f2$code
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
                "    set_attribute_names(",
                "      name = 'where',",
                "      old = c('REGION', 'State', 'City'),",
                "      new = c('Region', 'State', 'City')",
                "    )",
                "",
                "  ft",
                "}",
                "",
                "ft <- transform_instance_table(instance_df, star_sch)"
              )
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
              f2 <- flat_table('ft_num2', ft_num)
              f2 <- f2 |>
                update_according_to(f1)
              f2$code
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
                "    set_attribute_names(",
                "      name = 'where',",
                "      old = 'REGION',",
                "      new = 'Region'",
                "    )",
                "",
                "  ft",
                "}",
                "",
                "ft <- transform_instance_table(instance_df, star_sch)"
              )
            })
          })


test_that("set_measure_names() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_num) |>
      as_star_database(mrs_cause_schema) |>
      set_measure_names(new = c("Pneumonia and Influenza",
                                "All",
                                "Rows Aggregated"))
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "    set_measure_names(",
      "      name = 'mrs_cause',",
      "      old = c('Pneumonia and Influenza Deaths', 'All Deaths', 'nrow_agg'),",
      "      new = c('Pneumonia and Influenza', 'All', 'Rows Aggregated')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
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
    f2 <- flat_table('ft_num2', ft_num)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "    replace_attribute_values(",
      "      name = 'where',",
      "      attributes = c('REGION', 'State', 'City'),",
      "      old = c('1', 'CT', 'Bridgeport'),",
      "      new = c('1', 'CT', 'Hartford')",
      "    ) |>",
      "    group_dimension_instances(",
      "      name = 'where'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
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
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "    ) |>",
      "    replace_attribute_values(",
      "      name = 'when',",
      "      attributes = c('Year', 'WEEK', 'Week Ending Date'),",
      "      old = c('1962', '11', '1962-03-14'),",
      "      new = c('1962', '3', '1962-01-15')",
      "    ) |>",
      "    group_dimension_instances(",
      "      name = 'when'",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
  })
})


test_that("role_playing_dimension() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_cause_rpd) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(rpd = "When",
                             roles = c("When Available", "When Received"))
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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

test_that("role_playing_dimension() update_according_to", {
  expect_equal({
    f1 <- flat_table('ft_num', ft_cause_rpd) |>
      as_star_database(mrs_cause_schema_rpd) |>
      role_playing_dimension(
        rpd = "When",
        roles = c("When Available", "When Received"),
        rpd_att_names = TRUE
      )
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "      att_names = c('Year', 'WEEK', 'Week Ending Date')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
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
    f2 <- flat_table('ft_num2', ft_cause_rpd)
    f2 <- f2 |>
      update_according_to(f1)
    f2$code
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
      "      att_names = c('Year', 'Week', 'Date')",
      "    )",
      "",
      "  ft",
      "}",
      "",
      "ft <- transform_instance_table(instance_df, star_sch)"
    )
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
              f2 <- flat_table('ft_num2', ft_cause_rpd)
              f2 <- f2 |>
                update_according_to(f1)
              f2$code
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
                "    replace_attribute_values(",
                "      name = 'when_available',",
                "      attributes = c('Data Availability Year', 'Data Availability Week', 'Data Availability Date'),",
                "      old = c('1962', '11', '1962-03-14'),",
                "      new = c('1962', '3', '1962-01-15')",
                "    ) |>",
                "    group_dimension_instances(",
                "      name = 'when'",
                "    )",
                "",
                "  ft",
                "}",
                "",
                "ft <- transform_instance_table(instance_df, star_sch)"
              )
            })
          })
