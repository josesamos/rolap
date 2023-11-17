test_that("geoattribute", {
  t <- sf::st_drop_geometry(us_layer_state)
  t <- t |>
    dplyr::filter(!(STUSPS %in% c("IL", "MN", "OR")))
  t <- dplyr::inner_join(t, us_layer_state, by = "STUSPS")
  us_layer_state_incomplet <- sf::st_as_sf(t)


  # us_layer_state_incomplet <- us_layer_state |>
  #   dplyr::filter(!(STUSPS %in% c("IL", "MN", "OR")))

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

  fgd <- filter_geo_dimensions(db_2, c("where", "when"))

  gd1 = db_3$geo$where
  gd2 = db_4$geo$where
  igd1 <- integrate_geo_dimensions(gd1, gd2)

  gd2 = db_3$geo$where
  gd1 = db_4$geo$where
  igd2 <- integrate_geo_dimensions(gd1, gd2)

  db_8 <- db_3
  db_8$dimensions$where$table <- db_8$dimensions$where$table[, -2]
  db_9 <- db_3
  db_9$dimensions$where$table <- db_9$dimensions$where$table[, -3]
  db_10 <- db_3
  db_10$dimensions$where$table <- db_10$dimensions$where$table[, -c(2, 3)]
  r1 <- filter_geo_attributes(db_3)
  r1 <- names(r1$where)
  r2 <- filter_geo_attributes(db_8)
  r2 <- names(r2$where)
  r3 <- filter_geo_attributes(db_9)
  r3 <- names(r3$where)
  r4 <- filter_geo_attributes(db_10)
  r4 <- names(r4$where)

  ga <- get_geoattributes(db_3)

  geometries <- db_3 |>
    get_geoattribute_geometries(
      dimension = "where",
      attribute = "state"
    )

  geometries2 <- db_3 |>
    get_geoattribute_geometries(
      attribute = "state"
    )

  geometries3 <- db_2 |>
    get_geoattribute_geometries()


  db_11 <- db_4 |>
    define_geoattribute(
      dimension = "where",
      attribute = "state",
      from_layer = us_layer_state,
      by = "STUSPS"
    )


  expect_equal({
    db_11$geo$where$state$point$state
  },
  c(
    "AK",
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
    "IN",
    "KS",
    "KY",
    "LA",
    "MA",
    "MD",
    "ME",
    "MI",
    "MO",
    "MS",
    "MT",
    "NC",
    "ND",
    "NE",
    "NH",
    "NJ",
    "NM",
    "NV",
    "NY",
    "OH",
    "OK",
    "PA",
    "PR",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VA",
    "VT",
    "WA",
    "WI",
    "WV",
    "WY",
    "IL",
    "MN",
    "OR"
  ))


  expect_equal({
    geometries3
  },
  c("polygon", "point"))

  expect_equal({
    geometries2
  },
  c("polygon", "point"))

  expect_equal({
    geometries
  },
  c("polygon", "point"))

  expect_equal({
    ga
  },
  list(where = list("state", "region")))

  expect_equal({
    r1
  },
  c("state", "region"))

  expect_equal({
    r2
  },
  "state")

  expect_equal({
    r3
  },
  "region")

  expect_equal({
    r4
  },
  NULL)

  expect_equal({
    igd2$state$polygon$state
  },
  c(
    "AK",
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
    "IN",
    "KS",
    "KY",
    "LA",
    "MA",
    "MD",
    "ME",
    "MI",
    "MO",
    "MS",
    "MT",
    "NC",
    "ND",
    "NE",
    "NH",
    "NJ",
    "NM",
    "NV",
    "NY",
    "OH",
    "OK",
    "PA",
    "PR",
    "RI",
    "SC",
    "SD",
    "TN",
    "TX",
    "UT",
    "VA",
    "VT",
    "WA",
    "WI",
    "WV",
    "WY",
    "IL",
    "MN",
    "OR"
  ))


  expect_equal({
    igd2$state$polygon$state
  },
  igd2$state$point$state)


  expect_equal({
    names(igd2)
  },
  c("state", "region"))

  expect_equal({
    igd1
  },
  db_3$geo$where)

  expect_equal({
    fgd
  },
  db_2$geo)

  expect_equal({
    instances
  },
  structure(
    list(state = c("IL", "MN", "OR")),
    row.names = c(NA,-3L),
    class = c("tbl_df", "tbl", "data.frame")
  ))

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
    c(get_geometry(us_state_point),
      nrow(us_state_point))
  },
  c("point",
    "52"))

  expect_equal({
    c(names(db_7$geo$where$region$point),
      nrow(db_7$geo$where$region$point))
  },
  c("region", "geom", "9"))

  expect_equal({
    get_geometry(us_layer_state)
  },
  "polygon")

  expect_equal({
    get_geoattribute_name(c('a'))
  },
  "a")

  expect_equal({
    get_geoattribute_name(c('a', 'b'))
  },
  "a<|>b")
})
