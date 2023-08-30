test_that("flat_table() creates a flat_table object", {
  expect_equal({
    flat_table('iris', head(iris))
  }, structure(
    list(
      name = "iris",
      table = structure(
        list(
          Species = c("setosa",
                      "setosa", "setosa", "setosa", "setosa", "setosa"),
          Sepal.Length = c(5.1,
                           4.9, 4.7, 4.6, 5, 5.4),
          Sepal.Width = c(3.5, 3, 3.2, 3.1, 3.6,
                          3.9),
          Petal.Length = c(1.4, 1.4, 1.3, 1.5, 1.4, 1.7),
          Petal.Width = c(0.2,
                          0.2, 0.2, 0.2, 0.2, 0.4)
        ),
        row.names = c(NA,-6L),
        class = c("tbl_df",
                  "tbl", "data.frame")
      ),
      unknown_value = "___UNKNOWN___",
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
      pk_attributes = NULL,
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

test_that("flat_table() creates a flat_table object", {
  expect_equal({
    flat_table('ft_num', head(ft_num))
  }, structure(
    list(
      name = "ft_num",
      table = structure(
        list(
          Year = c("1962",
                   "1962", "1963", "1964", "1964", "1962"),
          WEEK = c("2", "4", "4",
                   "3", "6", "3"),
          `Week Ending Date` = c(
            "01/13/1962",
            "01/27/1962",
            "01/26/1963",
            "01/18/1964",
            "02/08/1964",
            "01/20/1962"
          ),
          REGION = c("1",
                     "1", "1", "1", "1", "1"),
          State = c("MA", "MA", "MA", "MA", "MA",
                    "CT"),
          City = c(
            "Boston",
            "Boston",
            "Boston",
            "Boston",
            "Boston",
            "Bridgeport"
          ),
          `<1 year (all cause deaths)` = c("14", "22", "11",
                                           "17", "13", "5"),
          `1-24 years (all cause deaths)` = c("8", "7",
                                              "14", "7", "9", "1"),
          `25-44 years` = c("11", "8", "17", "24",
                            "14", "3"),
          `45-64 years (all cause deaths)` = c("70", "73",
                                               "67", "90", "61", "10"),
          `65+ years (all cause deaths)` = c("167",
                                             "175", "167", "187", "147", "21"),
          `Pneumonia and Influenza Deaths` = c(11L,
                                               12L, 10L, 13L, 9L, 2L),
          `All Deaths` = c(270L, 285L, 276L, 325L,
                           244L, 40L)
        ),
        row.names = c(NA,-6L),
        class = c("tbl_df", "tbl",
                  "data.frame")
      ),
      unknown_value = "___UNKNOWN___",
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
      pk_attributes = NULL,
      lookup_tables = list(),
      attributes = c(
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
      ),
      measures = c("Pneumonia and Influenza Deaths",
                   "All Deaths")
    ),
    class = "flat_table"
  ))
})

test_that("snake_case() creates a flat_table object", {
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
  }, c("set_attribute_names", "species"))
})
