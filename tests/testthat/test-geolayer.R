test_that("geolayer", {

  gl_polygon <- mrs_db_geo |>
    as_geolayer()

  gl_point <- mrs_db_geo |>
    as_geolayer(geometry = "point")

  gl <- gl_polygon
  l <- gl |>
    get_layer()

  v <- gl |>
    get_variables()

  v2 <- v |>
    dplyr::filter(year == '1966' | year == '2016')

  gl_sel <- gl |>
    set_variables(v2)

  f <- gl |>
    as_GeoPackage(dir = tempdir())

  vd1 <- gl |>
    get_variable_description(name = c("var_009", "var_109"))

  vd2 <- gl_sel |>
    get_variable_description()

  vd3 <- gl_sel |>
    get_variable_description(only_values = TRUE)


  expect_equal({
    vd1
  },
  c(var_009 = "year = 1966, facts = mrs_age, measure = all_deaths",
    var_109 = "year = 2016, facts = mrs_age, measure = all_deaths"))

  expect_equal({
    vd2
  },
  c(
    var_009 = "year = 1966, facts = mrs_age, measure = all_deaths",
    var_010 = "year = 1966, facts = mrs_cause, measure = pneumonia_and_influenza_deaths",
    var_109 = "year = 2016, facts = mrs_age, measure = all_deaths",
    var_110 = "year = 2016, facts = mrs_cause, measure = pneumonia_and_influenza_deaths"
  ))

  expect_equal({
    vd3
  },
  c(
    var_009 = "1966, mrs_age, all_deaths",
    var_010 = "1966, mrs_cause, pneumonia_and_influenza_deaths",
    var_109 = "2016, mrs_age, all_deaths",
    var_110 = "2016, mrs_cause, pneumonia_and_influenza_deaths"
  ))


  expect_equal({
    basename(f)
  },
  "state.gpkg")


  expect_equal({
    names(gl_sel$geolayer)
  },
  c("state", "var_009", "var_010", "var_109", "var_110", "geom"))


  expect_equal({
    nrow(gl_sel$geolayer)
  },
  32)


  expect_equal({
    gl_sel$variables
  },
  structure(
    list(
      variable = c("var_009", "var_010", "var_109",
                   "var_110"),
      year = c("1966", "1966", "2016", "2016"),
      facts = c("mrs_age",
                "mrs_cause", "mrs_age", "mrs_cause"),
      measure = c(
        "all_deaths",
        "pneumonia_and_influenza_deaths",
        "all_deaths",
        "pneumonia_and_influenza_deaths"
      )
    ),
    row.names = c(NA,-4L),
    class = c("tbl_df", "tbl", "data.frame")
  ))

  expect_equal({
    nrow(v)
  },
  110)

  expect_equal({
    nrow(l)
  },
  39)

  expect_equal({
    names(v)
  },
  c("variable", "year", "facts", "measure"))

  expect_equal({
    names(gl_point$variables)
  },
  c("variable", "year", "facts", "measure"))

  expect_equal({
    get_geometry(gl_polygon$geolayer$geom)
  },
  "polygon")

  expect_equal({
    get_geometry(gl_point$geolayer$geom)
  },
  "point")

  expect_equal({
    names(l)
  },
  c(
    "state",
    "var_001",
    "var_002",
    "var_003",
    "var_004",
    "var_005",
    "var_006",
    "var_007",
    "var_008",
    "var_009",
    "var_010",
    "var_011",
    "var_012",
    "var_013",
    "var_014",
    "var_015",
    "var_016",
    "var_017",
    "var_018",
    "var_019",
    "var_020",
    "var_021",
    "var_022",
    "var_023",
    "var_024",
    "var_025",
    "var_026",
    "var_027",
    "var_028",
    "var_029",
    "var_030",
    "var_031",
    "var_032",
    "var_033",
    "var_034",
    "var_035",
    "var_036",
    "var_037",
    "var_038",
    "var_039",
    "var_040",
    "var_041",
    "var_042",
    "var_043",
    "var_044",
    "var_045",
    "var_046",
    "var_047",
    "var_048",
    "var_049",
    "var_050",
    "var_051",
    "var_052",
    "var_053",
    "var_054",
    "var_055",
    "var_056",
    "var_057",
    "var_058",
    "var_059",
    "var_060",
    "var_061",
    "var_062",
    "var_063",
    "var_064",
    "var_065",
    "var_066",
    "var_067",
    "var_068",
    "var_069",
    "var_070",
    "var_071",
    "var_072",
    "var_073",
    "var_074",
    "var_075",
    "var_076",
    "var_077",
    "var_078",
    "var_079",
    "var_080",
    "var_081",
    "var_082",
    "var_083",
    "var_084",
    "var_085",
    "var_086",
    "var_087",
    "var_088",
    "var_089",
    "var_090",
    "var_091",
    "var_092",
    "var_093",
    "var_094",
    "var_095",
    "var_096",
    "var_097",
    "var_098",
    "var_099",
    "var_100",
    "var_101",
    "var_102",
    "var_103",
    "var_104",
    "var_105",
    "var_106",
    "var_107",
    "var_108",
    "var_109",
    "var_110",
    "geom"
  ))


})
