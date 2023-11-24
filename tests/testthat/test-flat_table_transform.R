
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

