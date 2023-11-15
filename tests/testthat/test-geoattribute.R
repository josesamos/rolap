test_that("multiple_value_key()", {
  file <-
    system.file(
      "extdata",
      "layer_us_level.gpkg",
      package = "rolap"
    )
  layer_us_state <- sf::st_read(file, 'layer_us_state',
                                quiet = TRUE)

  db_2 <- db |>
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

