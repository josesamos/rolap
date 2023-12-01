
test_that("select_attributes() ", {
  expect_equal({
    ft <- flat_table('ft_num', ft_num) |>
      select_attributes(attributes = c('Year', 'WEEK'))
    c(ft$operations$operation$operation,
      ft |>
        get_attribute_names())
  }, c("flat_table", "select_attributes", "Year", "WEEK"))
})


test_that("select_measures() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      select_measures(measures = c('Sepal.Length', 'Sepal.Width'))
    c(ft$operations$operation$operation,
      ft |>
        get_measure_names())
  }, c(
    "flat_table",
    "select_measures",
    "Sepal.Length",
    "Sepal.Width"
  ))
})


test_that("select_measures() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      select_measures()

    names(ft$table)
  }, "Species")
})


test_that("select_instances() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      select_instances(
        attributes = c('Species'),
        values = c('versicolor', 'virginica')
      )
    unique(ft$table$Species)
  }, c("versicolor", "virginica"))
})


test_that("select_instances() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      select_instances(not = TRUE,
                       attributes = c('Species'),
                       values = c('versicolor', 'virginica'))
    unique(ft$table$Species)
  }, "setosa")
})


test_that("select_instances() ", {
  expect_equal({
    ft <- flat_table('ft_num', ft_num) |>
      select_instances(attributes = c('Year', 'WEEK'),
                       values = list(c('1962', '2'), c('1964', '2')))
    unique(ft$table[, c('Year', 'WEEK')])
  }, structure(
    list(Year = c("1962", "1964"), WEEK = c("2", "2")),
    row.names = c(NA,-2L),
    class = c("tbl_df", "tbl", "data.frame")
  ))
})


test_that("select_instances_by_comparison() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      select_instances_by_comparison(attributes = 'Species',
                                     comparisons = '>=',
                                     values = 'v')
    unique(ft$table$Species)
  }, c("versicolor", "virginica"))
})


test_that("select_instances_by_comparison() ", {
  expect_equal({
    ft <- flat_table('ft_num', ft_num) |>
      select_instances_by_comparison(
        not = FALSE,
        attributes = c('Year', 'Year', 'WEEK'),
        comparisons = c('>=', '<=', '=='),
        values = c('1962', '1964', '2')
      )
    c(ft$table$Year, ft$table$WEEK)
  }, c("1962", "1964", "1964", "2", "2", "2"))
})


test_that("select_instances_by_comparison() ", {
  expect_equal({
    ft <- flat_table('ft_num', ft_num) |>
      select_instances_by_comparison(
        not = FALSE,
        attributes = c('Year', 'Year', 'WEEK'),
        comparisons = c('>=', '<=', '=='),
        values = list(c('1962', '1964', '2'),
                      c('1962', '1964', '4'))
      )
    c(ft$table$Year, ft$table$WEEK)
  }, c(
    "1962",
    "1962",
    "1963",
    "1963",
    "1964",
    "1962",
    "1964",
    "2",
    "4",
    "4",
    "4",
    "2",
    "4",
    "2"
  ))
})


test_that("select_instances_by_comparison() ", {
  expect_equal({
    res <- tryCatch(
      ft <- flat_table('ft_num', ft_num) |>
        select_instances_by_comparison(
          not = FALSE,
          attributes = c('Year', 'Year', 'WEEK'),
          comparisons = c('>=', '<=', '=='),
          values = list(c('1962', '1964', '2'),
                        c('1962', '1964'))
        )
      ,
      error = function(e)
        1
    )
    res
  },
  1)
})


test_that("select_instances_by_comparison() ", {
  expect_equal({
    ft <- flat_table('ft_num', ft_num) |>
      select_instances_by_comparison(
        not = TRUE,
        attributes = c('Year', 'Year', 'WEEK'),
        comparisons = c('>=', '<=', '>'),
        values = c('1962', '1964', '2')
      )
    c(ft$table$Year, ft$table$WEEK)
    },
    c("1962", "1964", "1964", "2", "2", "2"))
})


test_that("transform_to_attribute() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(measures = "Sepal.Length",
                             width = 3,
                             decimal_places = 2)
    c(ft$table[1, 2][[1]], ft$attributes, ft$measures)
  }, c(
    "4,000.00",
    "Species",
    "Sepal.Length",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width"
  ))
})


test_that("transform_to_measure() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2
      ) |>
      transform_to_measure(attributes = "Sepal.Length", k_sep = ",", decimal_sep = ".")
    c(ft$table[1, 5][[1]], ft$attributes, ft$measures)
  }, c(
    "4000",
    "Species",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width",
    "Sepal.Length"
  ))
})


test_that("transform_to_measure() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = ".",
        decimal_sep = ","
      ) |>
      transform_to_measure(attributes = "Sepal.Length", k_sep = ".", decimal_sep = ".")
    c(ft$table[1, 5][[1]], ft$table[2, 5][[1]], ft$attributes, ft$measures)
  }, c(
    "4000",
    "4.9",
    "Species",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width",
    "Sepal.Length"
  ))
})


test_that("transform_to_measure() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = ".",
        decimal_sep = ","
      ) |>
      transform_to_measure(attributes = "Sepal.Length", k_sep = ".", decimal_sep = ",")
    c(ft$table[1, 5][[1]], ft$table[2, 5][[1]], ft$attributes, ft$measures)
  }, c(
    NA,
    NA,
    "Species",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width",
    "Sepal.Length"
  ))
})


test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      transform_to_attribute(measures = "Sepal.Length", decimal_places = 1) |>
      transform_attribute_format(attributes = "Sepal.Length",
                                 width = 5,
                                 decimal_places = 2)
    unique(ft$table$Sepal.Length)
  }, c(
    " 5.10",
    " 4.90",
    " 4.70",
    " 4.60",
    " 5.00",
    " 5.40",
    " 4.40",
    " 4.80",
    " 4.30",
    " 5.80",
    " 5.70",
    " 5.20",
    " 5.50",
    " 4.50",
    " 5.30",
    " 7.00",
    " 6.40",
    " 6.90",
    " 6.50",
    " 6.30",
    " 6.60",
    " 5.90",
    " 6.00",
    " 6.10",
    " 5.60",
    " 6.70",
    " 6.20",
    " 6.80",
    " 7.10",
    " 7.60",
    " 7.30",
    " 7.20",
    " 7.70",
    " 7.40",
    " 7.90"
  ))
})


test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = ".",
        decimal_sep = ","
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        decimal_places = 1,
        k_sep = ".",
        decimal_sep = ","
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4.000,0", "    4,9"))
})



test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = ".",
        decimal_sep = ","
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        decimal_places = 1,
        k_sep = ".",
        decimal_sep = ",",
        space_filling = FALSE
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4.000,0", "00004,9"))
})


test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = ",",
        decimal_sep = "."
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        decimal_places = 1,
        k_sep = ",",
        decimal_sep = "."
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4,000.0", "    4.9"))
})



test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = ",",
        decimal_sep = "."
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        decimal_places = 1,
        k_sep = ",",
        decimal_sep = ".",
        space_filling = FALSE
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4,000.0", "00004.9"))
})



test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = NULL,
        decimal_sep = "."
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        decimal_places = 1,
        k_sep = NULL,
        decimal_sep = ".",
        space_filling = FALSE
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4,000.0", "00004.9"))
})

test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        decimal_places = 2,
        k_sep = NULL,
        decimal_sep = "."
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        decimal_places = 1,
        k_sep = NULL,
        decimal_sep = "."
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4,000.0", "    4.9"))
})


test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft$table$Sepal.Length <- as.integer(ft$table$Sepal.Length)

    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        k_sep = ","
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4,000", "    4"))
})


test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft$table$Sepal.Length <- as.integer(ft$table$Sepal.Length)

    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        k_sep = NULL
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        k_sep = NULL
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4,000", "    4"))
})



test_that("transform_attribute_format() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 2] <- 4000
    ft$table$Sepal.Length <- as.integer(ft$table$Sepal.Length)

    ft <- ft |>
      transform_to_attribute(
        measures = "Sepal.Length",
        width = 3,
        k_sep = NULL
      )

    ft$table$Sepal.Length <- trimws(ft$table$Sepal.Length)

    ft <- ft |>
      transform_attribute_format(
        attributes = "Sepal.Length",
        width = 3,
        k_sep = NULL,
        space_filling = FALSE
      )

    c(ft$table$Sepal.Length[[1]], ft$table$Sepal.Length[[2]])
  }, c("4,000", "00004"))
})



test_that("transform_to_values() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value')
    c(names(ft$table), ft$attributes, ft$measures)
  }, c(
    "Species",
    "Characteristic",
    "Value",
    "Species",
    "Characteristic",
    "Value"
  ))
})

test_that("transform_to_values() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id')
    c(names(ft$table), ft$attributes, ft$measures)
  }, c(
    "id",
    "Species",
    "Characteristic",
    "Value",
    "id",
    "Species",
    "Characteristic",
    "Value"
  ))
})


test_that("transform_to_values() ", {
  expect_equal({
    res <- tryCatch(
      ft <- flat_table('iris', iris) |>
        transform_to_values(attribute = 'Species',
                            measure = 'Value')
      ,
      error = function(e)
        1
    )
    res
  },
  1)
})


test_that("transform_to_values() ", {
  expect_equal({
    res <- tryCatch(
      ft <- flat_table('iris', iris) |>
        transform_to_values(attribute = 'Characteristic',
                            measure = 'Value',
                            id_reverse = 'Species')
      ,
      error = function(e)
        1
    )
    res
  },
  1)
})


test_that("transform_from_values() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      transform_to_values(attribute = 'Characteristic',
                          measure = 'Value',
                          id_reverse = 'id')
    ft <- ft |>
      transform_from_values(attribute = 'Characteristic')
    c(names(ft$table), ft$attributes, ft$measures)
  }, c(
    "id",
    "Species",
    "Petal.Length",
    "Petal.Width",
    "Sepal.Length",
    "Sepal.Width",
    "id",
    "Species",
    "Petal.Length",
    "Petal.Width",
    "Sepal.Length",
    "Sepal.Width"
  ))
})


test_that("transform_from_values() ", {
  expect_equal({
    res <- tryCatch(
      ft <- flat_table('iris', iris) |>
        transform_to_values(attribute = 'Characteristic',
                            measure = 'Value',
                            id_reverse = 'id') |>
        transform_from_values(attribute = 'haracteristic')
      ,
      error = function(e)
        1
    )
    res
  },
  1)
})



test_that("separate_measures() ", {
  expect_equal({
    flat_table('iris', head(iris, 2)) |>
      separate_measures(measures = list(
        c('Petal.Length'),
        c('Petal.Width'),
        c('Sepal.Length'),
        c('Sepal.Width')
      ),
      names = c('PL', 'PW', 'SL', 'SW'))
  }, list(
    PL = structure(
      list(
        name = "PL",
        table = structure(
          list(
            Species = c("setosa", "setosa"),
            Petal.Length = c(1.4, 1.4)
          ),
          row.names = c(NA,-2L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        unknown_value = "___UNKNOWN___",
        operations = structure(list(
          operations = structure(
            list(
              operation = c("flat_table", "separate_measures"),
              name = c(
                "iris<|>___UNKNOWN___",
                "Petal.Length<|>Petal.Width<|>Sepal.Length<|>Sepal.Width"
              ),
              details = c("Species", "PL<|>PL<|>PW<|>SL<|>SW"),
              details2 = c(
                "Sepal.Length<|>Sepal.Width<|>Petal.Length<|>Petal.Width",
                "TRUE"
              ),
              order = c(1, 2)
            ),
            row.names = c(NA,-2L),
            class = "data.frame"
          )
        ), class = "star_operation"),
        lookup_tables = list(),
        attributes = "Species",
        measures = "Petal.Length"
      ),
      class = "flat_table"
    ),
    PW = structure(
      list(
        name = "PW",
        table = structure(
          list(
            Species = c("setosa",
                        "setosa"),
            Petal.Width = c(0.2, 0.2)
          ),
          row.names = c(NA,-2L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        unknown_value = "___UNKNOWN___",
        operations = structure(list(
          operations = structure(
            list(
              operation = c("flat_table", "separate_measures"),
              name = c(
                "iris<|>___UNKNOWN___",
                "Petal.Length<|>Petal.Width<|>Sepal.Length<|>Sepal.Width"
              ),
              details = c("Species", "PW<|>PL<|>PW<|>SL<|>SW"),
              details2 = c(
                "Sepal.Length<|>Sepal.Width<|>Petal.Length<|>Petal.Width",
                "TRUE"
              ),
              order = c(1, 2)
            ),
            row.names = c(NA,-2L),
            class = "data.frame"
          )
        ), class = "star_operation"),
        lookup_tables = list(),
        attributes = "Species",
        measures = "Petal.Width"
      ),
      class = "flat_table"
    ),
    SL = structure(
      list(
        name = "SL",
        table = structure(
          list(
            Species = c("setosa",
                        "setosa"),
            Sepal.Length = c(5.1, 4.9)
          ),
          row.names = c(NA,-2L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        unknown_value = "___UNKNOWN___",
        operations = structure(list(
          operations = structure(
            list(
              operation = c("flat_table", "separate_measures"),
              name = c(
                "iris<|>___UNKNOWN___",
                "Petal.Length<|>Petal.Width<|>Sepal.Length<|>Sepal.Width"
              ),
              details = c("Species", "SL<|>PL<|>PW<|>SL<|>SW"),
              details2 = c(
                "Sepal.Length<|>Sepal.Width<|>Petal.Length<|>Petal.Width",
                "TRUE"
              ),
              order = c(1, 2)
            ),
            row.names = c(NA,-2L),
            class = "data.frame"
          )
        ), class = "star_operation"),
        lookup_tables = list(),
        attributes = "Species",
        measures = "Sepal.Length"
      ),
      class = "flat_table"
    ),
    SW = structure(
      list(
        name = "SW",
        table = structure(
          list(
            Species = c("setosa",
                        "setosa"),
            Sepal.Width = c(3.5, 3)
          ),
          row.names = c(NA,-2L),
          class = c("tbl_df", "tbl", "data.frame")
        ),
        unknown_value = "___UNKNOWN___",
        operations = structure(list(
          operations = structure(
            list(
              operation = c("flat_table", "separate_measures"),
              name = c(
                "iris<|>___UNKNOWN___",
                "Petal.Length<|>Petal.Width<|>Sepal.Length<|>Sepal.Width"
              ),
              details = c("Species", "SW<|>PL<|>PW<|>SL<|>SW"),
              details2 = c(
                "Sepal.Length<|>Sepal.Width<|>Petal.Length<|>Petal.Width",
                "TRUE"
              ),
              order = c(1, 2)
            ),
            row.names = c(NA,-2L),
            class = "data.frame"
          )
        ), class = "star_operation"),
        lookup_tables = list(),
        attributes = "Species",
        measures = "Sepal.Width"
      ),
      class = "flat_table"
    )
  ))
})


test_that("separate_measures() ", {
  expect_equal({
    r <- flat_table('iris', head(iris, 2)) |>
      separate_measures(
        measures = list(
          c('Petal.Length'),
          c('Petal.Width'),
          c('Sepal.Length'),
          c('Sepal.Width')
        ),
        names = c('PL', 'PW', 'SL', 'SW', 'rest')
      )
    r[['rest']]
  },
  structure(
    list(
      name = "rest",
      table = structure(
        list(Species = c("setosa",
                         "setosa")),
        row.names = c(NA,-2L),
        class = c("tbl_df", "tbl",
                  "data.frame")
      ),
      unknown_value = "___UNKNOWN___",
      operations = structure(list(
        operations = structure(
          list(
            operation = c("flat_table", "separate_measures"),
            name = c(
              "iris<|>___UNKNOWN___",
              "Petal.Length<|>Petal.Width<|>Sepal.Length<|>Sepal.Width<|>NULL"
            ),
            details = c("Species", "rest<|>PL<|>PW<|>SL<|>SW<|>rest"),
            details2 = c(
              "Sepal.Length<|>Sepal.Width<|>Petal.Length<|>Petal.Width",
              "TRUE"
            ),
            order = c(1, 2)
          ),
          row.names = c(NA,-2L),
          class = "data.frame"
        )
      ), class = "star_operation"),
      lookup_tables = list(),
      attributes = "Species",
      measures = NULL
    ),
    class = "flat_table"
  ))
})


test_that("replace_empty_values() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 1] <- NA
    ft$table[2, 1] <- ""
    ft <- ft |>
      replace_empty_values()
    c(ft$table[1, 1], ft$table[2, 1])
  }, list(Species = "___UNKNOWN___", Species = "___UNKNOWN___"))
})


test_that("replace_unknown_values() ", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table[1, 1] <- NA
    ft$table[2, 1] <- ""
    ft <- ft |>
      replace_empty_values() |>
      replace_unknown_values(value = "Not available")
    c(ft$table[1, 1], ft$table[2, 1])
  }, list(Species = "Not available", Species = "Not available"))
})


test_that("replace_string() ", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      replace_string(
        string = c('set'),
        replacement = c('Set')
      )
    unique(ft$table$Species)
  }, c("Setosa", "versicolor", "virginica"))
})


test_that("remove_instances_without_measures() ", {
  expect_equal({
    iris2 <- iris
    iris2[10, 1:4] <- NA
    ft <- flat_table('iris', iris2) |>
      remove_instances_without_measures()
    c(nrow(iris2), nrow(ft$table))
  }, 150:149)
})

test_that("add_custom_column() ", {
  expect_equal({
    f <- function(table) {
      paste0(table$City, '-', table$State)
    }
    ft <- flat_table('ft_num', ft_num) |>
      add_custom_column(name = 'city_state', definition = f)
    unique(ft$table[, c('city_state')])
  }, structure(
    list(
      city_state = c("Boston-MA", "Bridgeport-CT", "Cambridge-MA",
                     "Hartford-CT")
    ),
    row.names = c(NA,-4L),
    class = c("tbl_df",
              "tbl", "data.frame")
  ))
})


test_that("add_custom_column() ", {
  expect_equal({
    f <- function(table) {
      paste0(table$City, '-', table$State)
    }
    ft <- flat_table('ft_num', ft_num) |>
      add_custom_column(name = 'city_state', definition = f)
    unique(ft$table[, c('city_state')])
  }, structure(
    list(
      city_state = c("Boston-MA", "Bridgeport-CT", "Cambridge-MA",
                     "Hartford-CT")
    ),
    row.names = c(NA,-4L),
    class = c("tbl_df",
              "tbl", "data.frame")
  ))
})


test_that("transform_to_values() ", {
  f <- function(table) {
    paste0(table$City, '-', table$State)
  }
  expect_equal({
    res <- tryCatch(
      ft <- flat_table('ft_num', ft_num) |>
        add_custom_column(name = 'region', definition = f)
      ,
      error = function(e)
        1
    )
    res
  },
  1)
})

