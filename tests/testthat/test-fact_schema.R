test_that("fact_schema() creates a fact schema", {
  expect_equal(
    fact_schema(name = "a"),
    structure(
      list(
        name = "a",
        measures = NULL,
        agg_functions = NULL,
        nrow_agg = NULL
      ),
      class = "fact_schema"
    )
  )
})

test_that("fact_schema() creates a fact schema", {
  expect_equal(
    fact_schema(
      name = "a",
      measures = c(
        "b",
        "c"
      )
    ),
    structure(
      list(
        name = "a",
        measures = c("b", "c"),
        agg_functions = NULL,
        nrow_agg = NULL
      ),
      class = "fact_schema"
    )
  )
})

test_that("fact_schema() creates a fact schema", {
  expect_equal(
    fact_schema(
      name = "a",
      measures = c(
        "b",
        "c"
      ),
      agg_functions = c(
        "MIN",
        "MAX"
      )
    ),
    structure(
      list(
        name = "a",
        measures = c("b", "c"),
        agg_functions = c(
          "MIN",
          "MAX"
        ),
        nrow_agg = NULL
      ),
      class = "fact_schema"
    )
  )
})

test_that("fact_schema() creates a fact schema", {
  expect_equal(
    fact_schema(name = "a", nrow_agg = "nrow"),
    structure(
      list(
        name = "a",
        measures = NULL,
        agg_functions = NULL,
        nrow_agg = "nrow"
      ),
      class = "fact_schema"
    )
  )
})
