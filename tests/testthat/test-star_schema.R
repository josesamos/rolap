test_that("star_schema() creates a star schema", {
  expect_equal(star_schema(), structure(list(fact = NULL, dimensions = NULL), class = "star_schema"))
})
