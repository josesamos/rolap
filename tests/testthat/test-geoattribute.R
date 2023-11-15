test_that("multiple_value_key()", {
  file <-
    system.file(
      "extdata",
      "layer_us_level.gpkg",
      package = "rolap"
    )
  layer_us_state <- sf::st_read(file, 'layer_us_state',
                                quiet = TRUE)

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

