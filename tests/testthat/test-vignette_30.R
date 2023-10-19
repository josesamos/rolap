test_that("dim instances", {
  #############################################################

  names <- db_summary |>
    get_table_names()


  ## ----------------------------------------------------------------------------------------------------------
  db_test <- db_summary
  (value_seg1 <- db_test$dimensions$where_chain$table$chain[3])
  (value_seg2 <- db_test$dimensions$where_chain$table$chain[4])
  db_test$dimensions$where_chain$table$chain[3] <- ' ,1-.% $1)='
  db_test$dimensions$where_chain$table$chain[4] <- ' ,1-.% $1)='


  ## ----------------------------------------------------------------------------------------------------------
  db_summary2 <- db_test |>
    replace_attribute_values(
      name = 'where_chain',
      attributes = c('chain', 'country'),
      old = c(' ,1-.% $1)=', 'CZE'),
      new = c(value_seg1, 'CZE')
    ) |>
    replace_attribute_values(
      name = 'where_chain',
      attributes = c('chain', 'country'),
      old = c(' ,1-.% $1)=', 'SVK'),
      new = c(value_seg2, 'SVK')
    )

  ## ----------------------------------------------------------------------------------------------------------
  db_summary3 <- db_summary2 |>
    replace_attribute_values(
      name = 'what',
      attributes = 'product',
      old = c('Autokosmet.'),
      new = c('Autokozmetik')
    ) |>
    replace_attribute_values(
      name = 'what',
      attributes = 'product',
      old = c('Diesel aditiv'),
      new = c('Diesel +')
    ) |>
    replace_attribute_values(
      name = 'what',
      attributes = 'product',
      old = c('Nafta Plus'),
      new = c('Nafta +')
    ) |>
    replace_attribute_values(
      name = 'what',
      attributes = 'product',
      old = c('Nat.Super', 'Natural Plus', 'Natural Spec'),
      new = c('Natural +')
    ) |>
    replace_attribute_values(
      name = 'what',
      attributes = 'product',
      old = c('Autoprísluš.', 'Dalnic.popl.', 'Knihy,nov.', 'LPG', 'Mytí vozidel',
              'Nemrz.kapal.', 'Obcerstvení', 'Oleje,tuky', 'Potraviny', 'Prev.náplne',
              'Provoz.nápl.', 'Umývanie voz', 'Zboží nesp.', 'Zpr.nakupu'),
      new = c('Other')
    ) |>
    group_dimension_instances(name = 'what')

  unique_attribute_values_db_summary3 <- db_summary3 |>
    get_unique_attribute_values('what')

  ## ----------------------------------------------------------------------------------------------------------
  db_tl <- db_summary3 |>
    as_tibble_list()


  names <- names(db_tl)
  res <- NULL
  for (i in seq_along(db_tl)){
    res <- c(res, sprintf("name: %s, %d rows\n", names[i], nrow(db_tl[[i]])))
  }

  res_db_summary3 <- c(res, sum(db_tl[['transaction_summary']]$transactions))

  ## ----------------------------------------------------------------------------------------------------------
  db_summary4 <- db_summary3 |>
    replace_attribute_values(
      name = 'when',
      attributes = 'hour',
      old = c('05', '06', '07', '08', '09', '10', '11'),
      new = c('Morning')
    ) |>
    replace_attribute_values(
      name = 'when',
      attributes = 'hour',
      old = c('12', '13', '14', '15', '16'),
      new = c('Afternoon')
    ) |>
    replace_attribute_values(
      name = 'when',
      attributes = 'hour',
      old = c('17', '18', '19', '20'),
      new = c('Evening')
    ) |>
    replace_attribute_values(
      name = 'when',
      attributes = 'hour',
      old = c('21', '22', '23', '00', '01', '02', '03', '04'),
      new = c('Night')
    ) |>
    group_dimension_instances(name = 'when')


  ## ----------------------------------------------------------------------------------------------------------
  db_summary4 |>
    get_unique_attribute_values('when')

  db_summary4 |>
    get_unique_attribute_values('when_paid')


  ## ----------------------------------------------------------------------------------------------------------
  db_tl <- db_summary4 |>
    as_tibble_list()

  names <- names(db_tl)
  res <- NULL
  for (i in seq_along(db_tl)){
    res <- c(res, sprintf("name: %s, %d rows\n", names[i], nrow(db_tl[[i]])))
  }

  res_db_summary4 <- c(res, sum(db_tl[['transaction_summary']]$transactions))


  ## ----------------------------------------------------------------------------------------------------------
  db_finest2 <- db_finest |>
    replace_attribute_values(
      name = 'what',
      attributes = 'product',
      old = c('Additivum', 'Autokozmetik'),
      new = c('Other')
    ) |>
    group_dimension_instances(name = 'what')


  ## ----------------------------------------------------------------------------------------------------------
  ct <- constellation("CSS", db_summary4, db_finest2)

  names <- ct |>
    get_table_names()

  db_tl <- ct |>
    as_tibble_list()

  res <- NULL
  for (n in names) {
    res <- c(res, sprintf("name: %s, %d rows\n", n, nrow(db_tl[[n]])))
  }

  res_ct <- res


  ## ----------------------------------------------------------------------------------------------------------
  ct2 <- ct |>
    replace_attribute_values(
      name = 'what',
      attributes = 'product',
      old = c('Autokozmetik'),
      new = c('Other')
    ) |>
    group_dimension_instances(name = 'what')

  unique <- ct2 |>
    get_unique_attribute_values('what')



  #############################################################
  expect_equal({
    db_summary4 |>
      get_unique_attribute_values('when')
  },
  {
    db_summary4 |>
      get_unique_attribute_values('when_paid')
  })


  #############################################################
  expect_equal({
    unique
  },
  {
    structure(
      list(
        product = c(
          "Additivum",
          "Autokosmet.",
          "Autoprísluš.",
          "Dalnic.popl.",
          "Diesel",
          "Diesel +",
          "Diesel aditiv",
          "Knihy,nov.",
          "LPG",
          "Mytí vozidel",
          "Nafta",
          "Nafta +",
          "Nafta Plus",
          "Nat.Super",
          "Natural",
          "Natural +",
          "Natural Plus",
          "Natural Spec",
          "Nemrz.kapal.",
          "Obcerstvení",
          "Oleje,tuky",
          "Other",
          "Potraviny",
          "Prev.náplne",
          "Provoz.nápl.",
          "Umývanie voz",
          "Zboží nesp.",
          "Zpr.nakupu"
        )
      ),
      row.names = c(NA,-28L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })



  #############################################################
  expect_equal({
    unique_attribute_values_db_summary3
  },
  {
    structure(
      list(
        product = c(
          "Additivum",
          "Autokozmetik",
          "Diesel",
          "Diesel +",
          "Nafta",
          "Nafta +",
          "Natural",
          "Natural +",
          "Other"
        )
      ),
      row.names = c(NA,-9L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })



  #############################################################
  expect_equal({
    unique_attribute_values_db_summary3
  },
  {
    structure(
      list(
        product = c(
          "Additivum",
          "Autokozmetik",
          "Diesel",
          "Diesel +",
          "Nafta",
          "Nafta +",
          "Natural",
          "Natural +",
          "Other"
        )
      ),
      row.names = c(NA,-9L),
      class = c("tbl_df", "tbl", "data.frame")
    )
  })



  #############################################################
  expect_equal({
    res_db_summary3
  },
  {
    c(
      "name: who_segment, 6 rows\n",
      "name: where_chain, 33 rows\n",
      "name: when, 84 rows\n",
      "name: when_paid, 84 rows\n",
      "name: what, 9 rows\n",
      "name: transaction_summary, 759 rows\n",
      "1000"
    )
  })



  #############################################################
  expect_equal({
    res_db_summary4
  },
  {
    c(
      "name: who_segment, 6 rows\n",
      "name: where_chain, 33 rows\n",
      "name: when, 18 rows\n",
      "name: when_paid, 18 rows\n",
      "name: what, 9 rows\n",
      "name: transaction_summary, 486 rows\n",
      "1000"
    )
  })


  #############################################################
  expect_equal({
    res_ct
  },
  {
    c(
      "name: transaction, 921 rows\n",
      "name: transaction_line, 1000 rows\n",
      "name: transaction_summary, 486 rows\n",
      "name: what, 29 rows\n",
      "name: when, 124 rows\n",
      "name: when_moment, 599 rows\n",
      "name: when_paid, 124 rows\n",
      "name: when_processed, 124 rows\n",
      "name: where, 437 rows\n",
      "name: where_chain, 33 rows\n",
      "name: who, 902 rows\n",
      "name: who_segment, 6 rows\n"
    )
  })

  #############################################################
})
