test_that("geoattribute", {
  us_layer_state_incomplet <-
    us_layer_state[!(us_layer_state$STUSPS %in% c("IL", "MN", "OR")),]

  db_2 <- mrs_db |>
    define_geoattribute(
      dimension = "where",
      attribute = "state",
      from_layer = us_layer_state,
      by = "STUSPS"
    )

  db_3 <- db_2 |>
    define_geoattribute(
      dimension = "where",
      attribute = "region",
      from_attribute = "state"
    )

  state_point <- db_2$geo$where$state$point

  db_4 <- mrs_db |>
    define_geoattribute(
      dimension = "where",
      attribute = "state",
      from_layer = us_layer_state_incomplet,
      by = "STUSPS"
    )

  instances <- get_unrelated_instances (db_4,
                                        dimension = "where",
                                        attribute = "state")

  db_5 <- db_2 |>
    define_geoattribute(
      dimension = "where",
      attribute = "state",
      from_layer = us_layer_state_incomplet,
      by = "STUSPS"
    )

  instances_2 <- get_unrelated_instances (db_5,
                                        dimension = "where",
                                        attribute = "state")

  db_6 <- mrs_db |>
    define_geoattribute(
      dimension = "where",
      attribute = "state",
      from_layer = state_point,
      by = "state"
    )

  db_7 <- db_6 |>
    define_geoattribute(
      dimension = "where",
      attribute = "region",
      from_attribute = "state"
    )

  us_state_point <-
    coordinates_to_point(us_layer_state,
                         lon_lat = c("INTPTLON", "INTPTLAT"))


  expect_equal({
    c(get_geometry(us_state_point),
      nrow(us_state_point))
  },
  c(
    "point",
    "52"
  ))

  expect_equal({
    c(names(db_7$geo$where$region$point),
      nrow(db_7$geo$where$region$point))
  },
  c("region", "geom", "9"))

  expect_equal({
    instances_2
  },
  structure(
    list(state = character(0)),
    row.names = integer(0),
    class = c("tbl_df",
              "tbl", "data.frame")
  ))

  expect_equal({
    instances
  },
  structure(
    list(state = c("IL", "MN", "OR")),
    row.names = c(NA,-3L),
    class = c("tbl_df", "tbl", "data.frame")
  ))

  expect_equal({
    get_geometry(us_layer_state)
  },
  "polygon"
  )

  expect_equal({
    get_geoattribute_name(c('a'))
  },
  "a"
  )

  expect_equal({
    get_geoattribute_name(c('a', 'b'))
  },
  "a<|>b"
  )
})

