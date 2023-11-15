test_that("multiple_value_key()", {
  file <-
    system.file(
      "extdata",
      "layer_us_level.gpkg",
      package = "rolap"
    )
  layer_us_state <- sf::st_read(file, 'layer_us_state',
                                quiet = TRUE)

  layer_us_state_incomplet <-
    layer_us_state[!(layer_us_state$STUSPS %in% c("IL", "MN", "OR")),]

  db_2 <- mrs_db |>
    define_geoattribute(
      dimension = "where",
      attribute = "state",
      from_layer = layer_us_state,
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
      from_layer = layer_us_state_incomplet,
      by = "STUSPS"
    )

  instances <- get_unrelated_instances (db_4,
                                        dimension = "where",
                                        attribute = "state")

  db_5 <- db_2 |>
    define_geoattribute(
      dimension = "where",
      attribute = "state",
      from_layer = layer_us_state_incomplet,
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
    get_geometry(layer_us_state)
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

