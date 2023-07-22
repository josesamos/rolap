test_that("star_schema() creates a star schema", {
  expect_equal(star_schema(), structure(list(
    facts = NULL, dimensions = NULL
  ), class = "star_schema"))
})

test_that("define_facts() define facts in a star schema", {
  expect_equal(
    define_facts(star_schema(), fact_schema(name = "a")),
    structure(list(
      facts = structure(list(name = "a", measures = NULL), class = "fact_schema"),
      dimensions = NULL
    ), class = "star_schema")
  )
})
