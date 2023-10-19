test_that("rdbms", {
  #############################################################

  ccs_sel_dm <- transactions_db

  ## ----message=FALSE-----------------------------------------------------------------------------------------
  transactions_ft <- ccs_sel_dm |>
    dm::dm_flatten_to_tbl(transactions_1k, .recursive = TRUE) |>
    dm::collect()


  ## ----------------------------------------------------------------------------------------------------------
  transactions_ft <- transactions_ft |>
    dplyr::mutate(Hour = format(Time, format = "%H"))  |>
    dplyr::mutate(`Processing Date` = format(as.POSIXct(Date) +
                                               lubridate::days(2), format = "%Y-%m-%d"))


  ## ----setup-------------------------------------------------------------------------------------------------

  s_finest <- rolap::star_schema() |>
    rolap::define_facts(name = "Transaction Line",
                        measures = c("Amount", "Price")) |>
    rolap::define_dimension(name = "Transaction",
                            attributes = c("Date", "Time", "CardID")) |>
    rolap::define_dimension(name = "Who",
                            attributes = c("CardID", "CustomerID", "Segment.customers",
                                           "Currency")) |>
    rolap::define_dimension(name = "Where",
                            attributes = c("GasStationID", "ChainID", "Country",
                                           "Segment.gasstations")) |>
    rolap::define_dimension(name = "When",
                            attributes = c("Date", "Hour")) |>
    rolap::define_dimension(name = "When Moment",
                            attributes = c("Time", "Hour")) |>
    rolap::define_dimension(name = "When Processed",
                            attributes = c("Processing Date", "Hour")) |>
    rolap::define_dimension(name = "What",
                            attributes = c("Description"))


  ## ----------------------------------------------------------------------------------------------------------
  db_finest <- rolap::star_database(s_finest, transactions_ft)


  ## ----------------------------------------------------------------------------------------------------------
  db_finest <- db_finest |>
    rolap::snake_case() |>
    rolap::set_attribute_names(
      name = "who",
      new = c(
        "card",
        "customer",
        "segment",
        "currency"
      )
    ) |>
    rolap::set_attribute_names(
      name = "where",
      new = c(
        "gas_station",
        "chain",
        "country",
        "segment"
      )
    ) |>
    rolap::set_attribute_names(
      name = "what",
      new = c(
        "product"
      )
    )


  ## ----------------------------------------------------------------------------------------------------------
  db_finest <- db_finest |>
    rolap::role_playing_dimension(
      rpd = "when",
      roles = c("when_processed")
    )


  ## ----------------------------------------------------------------------------------------------------------
  transactions_ft <- transactions_ft |>
    dplyr::mutate(`Payment Date` = format(as.POSIXct(Date) +
                                            lubridate::days(1), format = "%Y-%m-%d"))


  ## ----------------------------------------------------------------------------------------------------------
  s_summary <- rolap::star_schema() |>
    rolap::define_facts(name = "Transaction Summary",
                        measures = c("Amount", "Price"),
                        nrow_agg = "Transactions") |>
    rolap::define_dimension(name = "Who Segment",
                            attributes = c("Segment.customers",
                                           "Currency")) |>
    rolap::define_dimension(name = "Where Chain",
                            attributes = c("ChainID", "Country",
                                           "Segment.gasstations")) |>
    rolap::define_dimension(name = "When",
                            attributes = c("Date", "Hour")) |>
    rolap::define_dimension(name = "When Paid",
                            attributes = c("Payment Date", "Hour")) |>
    rolap::define_dimension(name = "What",
                            attributes = c("Description"))


  ## ----------------------------------------------------------------------------------------------------------
  db_summary <- rolap::star_database(s_summary, transactions_ft) |>
    rolap::snake_case() |>
    rolap::set_attribute_names(
      name = "who_segment",
      new = c(
        "segment",
        "currency"
      )
    ) |>
    rolap::set_attribute_names(
      name = "where_chain",
      new = c(
        "chain",
        "country",
        "segment"
      )
    ) |>
    rolap::set_attribute_names(
      name = "what",
      new = c(
        "product"
      )
    ) |>
    rolap::role_playing_dimension(
      rpd = "when",
      roles = c("when_paid")
    )


  ## ----------------------------------------------------------------------------------------------------------
  ct <- rolap::constellation("CSS", db_finest, db_summary)

  db_tl <- ct |>
    rolap::as_tibble_list()

  tables <- ct |>
    rolap::get_table_names()

  dimension_names <- ct |>
    get_dimension_names()

  ## ----------------------------------------------------------------------------------------------------------
  rpd_names <- ct |>
    rolap::get_role_playing_dimension_names()


  names <- names(db_tl)
  res <- NULL
  for (i in seq_along(db_tl)){
    res <- c(res, sprintf("name: %s, %d rows\n", names[i], nrow(db_tl[[i]])))
  }



  #############################################################
  expect_equal({
    rpd_names
  },
  {
    list(rpd_1 = c("when", "when_paid", "when_processed"))
  })



  #############################################################
  expect_equal({
    tables
  },
  {
    c(
      "transaction",
      "transaction_line",
      "transaction_summary",
      "what",
      "when",
      "when_moment",
      "when_paid",
      "when_processed",
      "where",
      "where_chain",
      "who",
      "who_segment"
    )
  })



  #############################################################
  expect_equal({
    dimension_names
  },
  {
    c("transaction", "what", "when", "when_moment", "when_paid",
      "when_processed", "where", "where_chain", "who", "who_segment"
    )
  })



  #############################################################
  expect_equal({
    res
  },
  {
    c(
      "name: transaction, 921 rows\n",
      "name: what, 27 rows\n",
      "name: when, 1965 rows\n",
      "name: when_moment, 599 rows\n",
      "name: when_paid, 1965 rows\n",
      "name: when_processed, 1965 rows\n",
      "name: where, 437 rows\n",
      "name: where_chain, 33 rows\n",
      "name: who, 902 rows\n",
      "name: who_segment, 6 rows\n",
      "name: transaction_line, 1000 rows\n",
      "name: transaction_summary, 995 rows\n"
    )
  })



  #############################################################
})
