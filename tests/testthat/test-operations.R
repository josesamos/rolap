


test_that("operations", {
  so0 <- star_operation()

  so1 <-
    add_operation(
      so0,
      op_name = "replace_attribute_values",
      name = c("where", "|", "region", "state", "city"),
      details = c("1", "CT", "Bridgeport"),
      details2 = c("1", "CT", "Hartford")
    )

  so2 <-
    add_operation(
      so1,
      op_name = "replace_attribute_values",
      name = c("where", "|", "region", "state", "city"),
      details = c("1", "CT", "Bridgeport"),
      details2 = c("1", "CT", "Hartfordd")
    )

  res <-
    is_new_operation(
      so1,
      op_name = "replace_attribute_values",
      name = c("where", "|", "region", "state", "city"),
      details = c("1", "CT", "Bridgeport"),
      details2 = c("1", "CT", "Hartford")
    )

  res2 <-
    is_new_operation(
      so1,
      op_name = "replace_attribute_values",
      name = c("where", "|", "region", "state", "city"),
      details = c("1", "CT", "Bridgeport"),
      details2 = c("1", "CT", "Hartfordd")
    )

  res3 <- get_next_operation(so1, op_name = "replace_attribute_values",
                             name = "where")

  res4 <- get_next_operation(so2, op_name = "replace_attribute_values",
                             name = "where", actual = res3)

  res5 <- get_next_operation(so2, op_name = "replace_attribute_values",
                             name = "where", actual = res4)



  expect_equal(res5,
               NULL)

  expect_equal(res4,
               structure(
                 list(
                   operation = "replace_attribute_values",
                   name = "where<|>|<|>region<|>state<|>city",
                   details = "1<|>CT<|>Bridgeport",
                   details2 = "1<|>CT<|>Hartfordd",
                   order = 2
                 ),
                 row.names = 2L,
                 class = "data.frame"
               ))

  expect_equal(res3,
               structure(
                 list(
                   operation = "replace_attribute_values",
                   name = "where<|>|<|>region<|>state<|>city",
                   details = "1<|>CT<|>Bridgeport",
                   details2 = "1<|>CT<|>Hartford",
                   order = 1
                 ),
                 row.names = 1L,
                 class = "data.frame"
               ))

  expect_equal(res,
               FALSE)

  expect_equal(res2,
               TRUE)

  expect_equal(so0,
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
