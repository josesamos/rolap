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
    ft <- flat_table('iris', iris)
    ft$table[1, 1] <- NA
    ft$table[2, 1] <- ""
    ft <- ft |>
      replace_empty_values()
  }, {
    f2 <- flat_table('iris2', iris)
    f2$table[1, 1] <- NA
    f2$table[2, 1] <- ""
    f2 |>
      refresh_new(ft)
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











test_that("snake_case()", {
  expect_equal({
    ft <- flat_table('iris', head(iris)) |>
      snake_case()
    c(
      ft$name,
      names(ft$table),
      ft$pk_attributes,
      ft$attributes,
      ft$measures,
      ft$operations[[1]][[1]]
    )
  }, c(
    "iris",
    "species",
    "sepal_length",
    "sepal_width",
    "petal_length",
    "petal_width",
    "species",
    "sepal_length",
    "sepal_width",
    "petal_length",
    "petal_width",
    "flat_table",
    "snake_case"
  ))
})


test_that("get_attribute_names()", {
  expect_equal({
    flat_table('ft_num', ft_num) |>
      get_attribute_names()
  }, c(
    "Year",
    "WEEK",
    "Week Ending Date",
    "REGION",
    "State",
    "City",
    "<1 year (all cause deaths)",
    "1-24 years (all cause deaths)",
    "25-44 years",
    "45-64 years (all cause deaths)",
    "65+ years (all cause deaths)"
  ))
})


test_that("get_attribute_names()", {
  expect_equal({
    flat_table('ft_num', ft_num) |>
      get_attribute_names(as_definition = TRUE)
  }, "c('Year', 'WEEK', 'Week Ending Date', 'REGION', 'State', 'City', '<1 year (all cause deaths)', '1-24 years (all cause deaths)', '25-44 years', '45-64 years (all cause deaths)', '65+ years (all cause deaths)')")
})


test_that("get_measure_names()", {
  expect_equal({
    flat_table('iris', iris) |>
      get_measure_names()
  }, c(
    "Sepal.Length",
    "Sepal.Width",
    "Petal.Length",
    "Petal.Width"
  ))
})


test_that("get_measure_names()", {
  expect_equal({
    flat_table('iris', iris) |>
      get_measure_names(ordered = TRUE, as_definition = TRUE)
  }, "c('Petal.Length', 'Petal.Width', 'Sepal.Length', 'Sepal.Width')")
})


test_that("set_attribute_names() and get_attribute_names()", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      set_attribute_names(old = c('Species'),
                          new = c('species'))
    c(ft$operations$operation$operation,
      ft |>
        get_attribute_names())
  }, c("flat_table", "set_attribute_names", "species"))
})


test_that("set_measure_names() and get_measure_names()", {
  expect_equal({
    ft <- flat_table('iris', iris) |>
      set_measure_names(
        old = c(
          'Petal.Length',
          'Petal.Width',
          'Sepal.Length',
          'Sepal.Width'
        ),
        new = c('pl', 'pw', 'ls', 'sw')
      )
    c(ft$operations$operation$operation,
      ft |>
        get_measure_names())
  }, c("flat_table", "set_measure_names", "ls", "sw", "pl", "pw"))
})

test_that("get_similar_attribute_values()", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table$Species[2] <- "se Tosa"
    ft$table$Species[20] <- "se.Tosa."
    ft$table$Species[60] <- "Versicolor"
    ft |>
      get_similar_attribute_values()
  }, {
    list(
      structure(
        list(Species = c("se Tosa", "se.Tosa.", "setosa")),
        row.names = c(NA,-3L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      structure(
        list(Species = c("Versicolor", "versicolor")),
        row.names = c(NA,-2L),
        class = c("tbl_df", "tbl", "data.frame")
      )
    )
  })
})


test_that("get_similar_attribute_values()", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table$Species[2] <- "se Tosa"
    ft$table$Species[20] <- "se.Tosa."
    ft$table$Species[60] <- "Versicolor"
    ft |>
      get_similar_attribute_values(col_as_vector = 'col')
  }, {
    list(
      structure(
        list(
          Species = c("se Tosa", "se.Tosa.", "setosa"),
          col = c("c('se Tosa')", "c('se.Tosa.')", "c('setosa')")
        ),
        row.names = c(NA, -3L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      structure(
        list(
          Species = c("Versicolor", "versicolor"),
          col = c("c('Versicolor')",
                  "c('versicolor')")
        ),
        row.names = c(NA, -2L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      )
    )
  })
})

test_that("get_similar_attribute_values_individually()", {
  expect_equal({
    ft <- flat_table('iris', iris)
    ft$table$Species[2] <- "se Tosa"
    ft$table$Species[20] <- "se.Tosa."
    ft$table$Species[60] <- "Versicolor"
    ft |>
      get_similar_attribute_values_individually()
  }, {
    list(
      structure(
        list(Species = c("se Tosa", "se.Tosa.", "setosa")),
        row.names = c(NA, -3L),
        class = c("tbl_df", "tbl", "data.frame")
      ),
      structure(
        list(Species = c("Versicolor", "versicolor")),
        row.names = c(NA, -2L),
        class = c("tbl_df", "tbl", "data.frame")
      )
    )
  })
})

test_that("get_similar_attribute_values_individually()", {
  expect_equal({
    ft <- flat_table('ft_num', ft_num)
    ft$table$Year[3] <- "1.963"
    ft |>
      get_similar_attribute_values_individually()
  }, {
    list(structure(
      list(Year = c("1.963", "1963")),
      row.names = c(NA, -2L),
      class = c("tbl_df", "tbl", "data.frame")
    ))
  })
})


test_that("get_unique_attribute_values()", {
  expect_equal({
    flat_table('iris', iris) |>
      get_unique_attribute_values()
  }, {
    structure(
      list(Species = c("setosa", "versicolor", "virginica")),
      row.names = c(NA,-3L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })
})


test_that("get_unique_attribute_values()", {
  expect_equal({
    flat_table('ft_num', ft_num) |>
      get_unique_attribute_values(attributes = c("REGION", "State"))
  }, {
    structure(
      list(REGION = c("1", "1"), State = c("CT", "MA")),
      row.names = c(NA,-2L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })
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
              name = c("iris<|>___UNKNOWN___", "Petal.Length"),
              details = c("Species",
                          "PL"),
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
              name = c("iris<|>___UNKNOWN___", "Petal.Width"),
              details = c("Species", "PW"),
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
              name = c("iris<|>___UNKNOWN___", "Sepal.Length"),
              details = c("Species", "SL"),
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
              name = c("iris<|>___UNKNOWN___", "Sepal.Width"),
              details = c("Species", "SW"),
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



test_that("remove_instances_without_measures() ", {
  expect_equal({
    iris2 <- iris
    iris2[10, 1:4] <- NA
    ft <- flat_table('iris', iris2) |>
      remove_instances_without_measures()
    c(nrow(iris2), nrow(ft$table))
  }, 150:149)
})


test_that("lookup_table() ", {
  expect_equal({
    flat_table('iris', iris) |>
      lookup_table(
        measures = c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Petal.Width"
        ),
        measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
      )
  }, structure(
    list(
      name = "iris",
      table = structure(
        list(
          Species = c("setosa",
                      "versicolor", "virginica"),
          Sepal.Length = c(5.8, 7, 7.9),
          Sepal.Width = c(2.3,
                          2, 2.2),
          Petal.Length = c(73.1, 213, 277.6),
          Petal.Width = c(0.246,
                          1.326, 2.026)
        ),
        row.names = c(NA,-3L),
        class = c("tbl_df", "tbl",
                  "data.frame")
      ),
      unknown_value = "___UNKNOWN___",
      operations = structure(list(
        operations = structure(
          list(
            operation = c("flat_table", "lookup_table"),
            name = c("iris<|>___UNKNOWN___", "Species"),
            details = c("Species",
                        "|"),
            details2 = c(
              "Sepal.Length<|>Sepal.Width<|>Petal.Length<|>Petal.Width",
              "Sepal.Length<|>Sepal.Width<|>Petal.Length<|>Petal.Width<|>|<|>MAX<|>MIN<|>SUM<|>MEAN"
            ),
            order = c(1, 2)
          ),
          row.names = c(NA,-2L),
          class = "data.frame"
        )
      ), class = "star_operation"),
      pk_attributes = "Species",
      lookup_tables = list(),
      attributes = "Species",
      measures = c(
        "Sepal.Length",
        "Sepal.Width",
        "Petal.Length",
        "Petal.Width"
      )
    ),
    class = "flat_table"
  ))
})


test_that("get_pk_attribute_names() ", {
  expect_equal({
    flat_table('iris', iris) |>
      lookup_table(
        measures = c(
          "Sepal.Length",
          "Sepal.Width",
          "Petal.Length",
          "Petal.Width"
        ),
        measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
      ) |>
      get_pk_attribute_names()
  }, "Species")
})



test_that("check_lookup_table() ", {
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
    flat_table('iris', iris) |>
      replace_attribute_values(
        attributes = 'Species',
        old = c('setosa'),
        new = c('flor')
      ) |>
      check_lookup_table(lookup = lookup)
  }, structure(
    list(Species = "flor"),
    row.names = c(NA,-1L),
    class = c("tbl_df",
              "tbl", "data.frame")
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



test_that("get_unknown_values() ", {
  expect_equal({
    iris2 <- iris
    iris2[10, 'Species'] <- NA
    flat_table('iris', iris2) |>
      get_unknown_values()
  }, structure(list(Species = NA_character_), row.names = c(NA, -1L
  ), class = c("tbl_df", "tbl", "data.frame")))
})

