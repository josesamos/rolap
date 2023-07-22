test_that("dimension_schema() creates a fact schema", {
  expect_equal(
    d <- dimension_schema(
      name = "a",
      attributes = c(
        "b",
        "c",
        "d"
      )
    ),
    structure(list(name = "a", attributes = c("b", "c", "d")), class = "dimension_schema")
  )
})

