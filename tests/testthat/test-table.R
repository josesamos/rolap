test_that("table",
          {
            table <- mrs_db$dimensions$where$table

            table[2, 4] <- "Bridgeport 2"


            r1 <-
              get_similar_values_table(
                table,
                attributes = "city",
                exclude_numbers = TRUE,
                col_as_vector = "a"
              )

            r2 <-
              get_unique_values_table(table,
                                      attributes = "city",
                                      col_as_vector = "a")

            r3 <-
              remove_all_measures_na(table, measures = c("lat", "long"))

            r5 <-
              replace_empty_values_table(
                table,
                attributes = c("lat", "long"),
                empty_values = c("41.2", " -71.1"),
                unknown_value = "00.00"
              )

            table[2, 9] <- NA
            r4 <-
              remove_all_measures_na(table, measures = c("long"))

            table[3, 9] <- NA
            r6 <- prepare_to_join(table, unknown_value = "0000")

            expect_equal(r1,
                         list(structure(
                           list(
                             city = c("Bridgeport", "Bridgeport 2"),
                             a = c("c('Bridgeport')",
                                   "c('Bridgeport 2')")
                           ),
                           row.names = c(NA, -2L),
                           class = c("tbl_df",
                                     "tbl", "data.frame")
                         )))

            table2 <- table[3, ]

            r7 <- prepare_to_join(table2, unknown_value = "0000")

            table3 <- table[, c("state", "region")]
            table3$region <- as.integer(table3$region)

            r8 <-
              group_by_keys(
                table3,
                keys = "state",
                measures = "region",
                agg_functions = "MIN",
                nrow_agg = "n"
              )



            expect_equal(nrow(r2),
                         119)

            expect_equal(nrow(r3),
                         120)

            expect_equal(nrow(r4),
                         119)

            expect_equal(r5[1, 8][[1]],
                         "00.00")

            expect_equal(r5[5, 9][[1]],
                         "00.00")

            expect_equal(r6[2, 9],
                         r6[3, 9])

            expect_equal(r6[2, 9],
                         r7[1, 9])

            expect_equal(r8,
                         structure(
                           list(
                             state = c(
                               "AL",
                               "AR",
                               "AZ",
                               "CA",
                               "CO",
                               "CT",
                               "DC",
                               "DE",
                               "FL",
                               "GA",
                               "HI",
                               "IA",
                               "ID",
                               "IL",
                               "IN",
                               "KS",
                               "KY",
                               "LA",
                               "MA",
                               "MI",
                               "MN",
                               "MO",
                               "NC",
                               "NE",
                               "NJ",
                               "NM",
                               "NV",
                               "NY",
                               "OH",
                               "OK",
                               "OR",
                               "PA",
                               "RI",
                               "TN",
                               "TX",
                               "UT",
                               "VA",
                               "WA",
                               "WI"
                             ),
                             region = c(
                               6L,
                               7L,
                               8L,
                               9L,
                               8L,
                               1L,
                               5L,
                               5L,
                               5L,
                               5L,
                               9L,
                               4L,
                               8L,
                               3L,
                               3L,
                               4L,
                               6L,
                               7L,
                               1L,
                               3L,
                               4L,
                               4L,
                               5L,
                               4L,
                               2L,
                               8L,
                               8L,
                               2L,
                               3L,
                               7L,
                               9L,
                               2L,
                               1L,
                               6L,
                               7L,
                               8L,
                               5L,
                               9L,
                               3L
                             ),
                             n = c(
                               3L,
                               1L,
                               2L,
                               11L,
                               3L,
                               4L,
                               1L,
                               1L,
                               4L,
                               2L,
                               1L,
                               1L,
                               1L,
                               3L,
                               5L,
                               2L,
                               1L,
                               3L,
                               8L,
                               3L,
                               3L,
                               2L,
                               1L,
                               2L,
                               6L,
                               1L,
                               1L,
                               8L,
                               8L,
                               1L,
                               1L,
                               6L,
                               1L,
                               4L,
                               7L,
                               2L,
                               2L,
                               3L,
                               1L
                             )
                           ),
                           class = c("tbl_df", "tbl", "data.frame"),
                           row.names = c(NA,-39L)
                         ))

          })
