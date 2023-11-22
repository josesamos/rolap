test_that("operations in constellation 1", {
  db1 <- star_database(mrs_cause_schema, ft_num) |>
    snake_case()

  db1 <- db1 |>
    replace_attribute_values(name = "where",
                             old = c('1', 'CT', 'Bridgeport'),
                             new = c('1', 'CT', 'Hartford'))

  db2 <- star_database(mrs_age_schema, ft_age) |>
    snake_case()

  ct1 <- constellation("MRS", db1, db2)

  expect_equal(ct1$operations$mrs_cause$operations[2:3, ],
               ct1$operations$mrs_age$operations[2:3, ])

  expect_equal(ct1$dimensions$where$table,
               structure(
                 list(
                   where_key = 1:3,
                   region = c("1", "1", "1"),
                   state = c("CT",
                             "MA", "MA"),
                   city = c("Hartford", "Boston", "Cambridge")
                 ),
                 row.names = c(NA,-3L),
                 class = c("tbl_df", "tbl", "data.frame")
               ))
})


test_that("operations in constellation 2", {
  db1 <- star_database(mrs_cause_schema, ft_num) |>
    snake_case()

  db1 <- db1 |>
    replace_attribute_values(name = "where",
                             old = c('1', 'CT', 'Bridgeport'),
                             new = c('1', 'CT', 'Hartford'))

  db2 <- star_database(mrs_age_schema, ft_age) |>
    snake_case()
  db2 <- db2 |>
    replace_attribute_values(name = "where",
                             old = c('1', 'CT', 'Bridgeport'),
                             new = c('1', 'CT', 'Hartford'))

  ct1 <- constellation("MRS", db1, db2)

  expect_equal(ct1$operations$mrs_cause$operations[2:3, ],
               ct1$operations$mrs_age$operations[2:3, ])

  expect_equal(ct1$dimensions$where$table,
               structure(
                 list(
                   where_key = 1:3,
                   region = c("1", "1", "1"),
                   state = c("CT",
                             "MA", "MA"),
                   city = c("Hartford", "Boston", "Cambridge")
                 ),
                 row.names = c(NA,-3L),
                 class = c("tbl_df", "tbl", "data.frame")
               ))
})




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

  res6 <-
    delete_operation(
      so2,
      op_name = "replace_attribute_values",
      name = c("where", "|", "region", "state", "city"),
      details = c("1", "CT", "Bridgeport"),
      details2 = c("1", "CT", "Hartfordd")
    )

  res7 <- delete_operation_set(so2, so1)

  res8 <- delete_operation_set(so2, so2)



  expect_equal(res7,
               structure(list(
                 operations = structure(
                   list(
                     operation = "replace_attribute_values",
                     name = "where<|>|<|>region<|>state<|>city",
                     details = "1<|>CT<|>Bridgeport",
                     details2 = "1<|>CT<|>Hartfordd",
                     order = 2
                   ),
                   row.names = 2L,
                   class = "data.frame"
                 )
               ), class = "star_operation"))

  expect_equal(res8,
               structure(list(
                 operations = structure(
                   list(
                     operation = character(0),
                     name = character(0),
                     details = character(0),
                     details2 = character(0),
                     order = numeric(0)
                   ),
                   row.names = integer(0),
                   class = "data.frame"
                 )
               ), class = "star_operation"))

  expect_equal(res6,
               so1)

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



