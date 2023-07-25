test_that("star_schema() creates a star schema", {
  expect_equal(star_schema(), structure(list(
    facts = NULL, dimensions = NULL
  ), class = "star_schema"))
})

test_that("define_facts() define facts in a star schema", {
  expect_equal(
    define_facts(star_schema(), fact_schema(name = "a")),
    structure(list(
      facts = list(a = structure(
        list(
          name = "a",
          measures = NULL,
          agg_functions = NULL,
          nrow_agg = NULL
        ),
        class = "fact_schema"
      )),
      dimensions = NULL
    ), class = "star_schema")
  )
})

test_that("define_facts() define facts in a star schema", {
  expect_equal(
    define_facts(star_schema(), name = "a"),
    structure(list(
      facts = list(a = structure(
        list(
          name = "a",
          measures = NULL,
          agg_functions = NULL,
          nrow_agg = NULL
        ),
        class = "fact_schema"
      )),
      dimensions = NULL
    ), class = "star_schema")
  )
})

test_that("define_facts() define facts in a star schema", {
  expect_equal(
    define_facts(star_schema(), name = "a", measures = "b"),
    structure(list(
      facts = list(a = structure(
        list(
          name = "a",
          measures = "b",
          agg_functions = NULL,
          nrow_agg = NULL
        ),
        class = "fact_schema"
      )),
      dimensions = NULL
    ), class = "star_schema")
  )
})

test_that("define_facts() define facts in a star schema", {
  expect_equal(
    define_facts(
      star_schema(),
      name = "a",
      measures = c("b",
                   "c"),
      agg_functions = c("MIN",
                        "MAX")
    ),
    structure(list(
      facts = list(a = structure(
        list(
          name = "a",
          measures = c("b",
                       "c"),
          agg_functions = c("MIN", "MAX"),
          nrow_agg = NULL
        ),
        class = "fact_schema"
      )),
      dimensions = NULL
    ), class = "star_schema")
  )
})

test_that("define_facts() define facts in a star schema", {
  expect_equal(
    define_facts(star_schema(), name = "a", nrow_agg = "nrow"),
    structure(list(
      facts = list(a = structure(
        list(
          name = "a",
          measures = NULL,
          agg_functions = NULL,
          nrow_agg = "nrow"
        ),
        class = "fact_schema"
      )),
      dimensions = NULL
    ), class = "star_schema")
  )
})


test_that("define_dimension() define dimension in a star schema", {
  expect_equal(
    {
      s <- star_schema()
      d <- dimension_schema(
        name = "a",
        attributes = "b"
      )
      define_dimension(s, d)
    },
    structure(list(facts = NULL, dimensions = list(a = structure(list(
      name = "a", attributes = "b"
    ), class = "dimension_schema"))), class = "star_schema")
  )
})

test_that("define_dimension() define dimension in a star schema", {
  expect_equal(
    {
      s <- star_schema()
      d <- dimension_schema(
        name = "a",
        attributes = "b"
      )
      s2 <- define_dimension(s, d)
      d2 <- dimension_schema(
        name = "c",
        attributes = "d"
      )
      define_dimension(s2, d2)
    },
    structure(list(facts = NULL, dimensions = list(
      a = structure(list(
        name = "a", attributes = "b"
      ), class = "dimension_schema"),
      c = structure(list(name = "c", attributes = "d"), class = "dimension_schema")
    )), class = "star_schema")
  )
})

test_that("define_dimension() define dimension in a star schema", {
  expect_equal(
    {
      define_dimension(star_schema(),
        name = "a",
        attributes = "b"
      )
    },
    structure(list(facts = NULL, dimensions = list(a = structure(list(
      name = "a", attributes = "b"
    ), class = "dimension_schema"))), class = "star_schema")
  )
})

test_that("define_dimension() define dimension in a star schema", {
  expect_equal(
    {
      s <- star_schema()
      d <- dimension_schema(
        name = "a",
        attributes = "b"
      )
      s2 <- define_dimension(s, d)
      define_dimension(s2,
        name = "c",
        attributes = "d"
      )
    },
    structure(list(facts = NULL, dimensions = list(
      a = structure(list(
        name = "a", attributes = "b"
      ), class = "dimension_schema"),
      c = structure(list(name = "c", attributes = "d"), class = "dimension_schema")
    )), class = "star_schema")
  )
})
