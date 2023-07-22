test_that("fact_schema() creates a fact schema", {
  expect_equal(
    fact_schema(
      name = "a",
      measures = c(
        "b",
        "c"
      )
    ),
    structure(list(
      name = "a", measures = c("b", "c")
    ), class = "fact_schema")
  )
})

test_that("fact_schema() creates a fact schema", {
  expect_equal(
    fact_schema(name = "a"),
    structure(list(name = "a", measures = NULL), class = "fact_schema")
  )
})

