


test_that("operations", {
  so <- star_operation()


  expect_equal(so,
               structure(list(
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
               ), class = "star_operation"))
})
