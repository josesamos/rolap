#' Mortality Reporting System
#'
#' Selection of 20 rows from the 122 Cities Mortality Reporting System.
#'
#' The original dataset covers from 1962 to 2016. For each week, in 122 US cities,
#' mortality figures by age group and cause, considered separately, are included.
#' In the cause, only a distinction is made between pneumonia or influenza and
#' others.
#'
#' @family mrs example data
#' @seealso \code{\link{mrs_cause_schema}}
#'
#' @format A `tibble`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"ft"

#' Mortality Reporting System with numerical measures
#'
#' Selection of 20 rows from the 122 Cities Mortality Reporting System. Measures
#' have been defined as integer values.
#'
#' The original dataset covers from 1962 to 2016. For each week, in 122 US cities,
#' mortality figures by age group and cause, considered separately, are included.
#' In the cause, only a distinction is made between pneumonia or influenza and
#' others.
#'
#' @family mrs example data
#' @seealso \code{\link{mrs_cause_schema}}
#' @examples
#' # The operations to obtain it from the `ft` data set are:
#'
#' ft_num <- ft |>
#'   dplyr::mutate(`Pneumonia and Influenza Deaths` = as.integer(`Pneumonia and Influenza Deaths`)) |>
#'   dplyr::mutate(`All Deaths` = as.integer(`All Deaths`))
#'
#' @format A `tibble`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"ft_num"

#' Mortality Reporting System by Age Group
#'
#' Selection data from the 122 Cities Mortality Reporting System by age group.
#'
#' The original dataset covers from 1962 to 2016. For each week, in 122 US cities,
#' mortality figures by age group and cause, considered separately, are included.
#'
#' @family mrs example data
#' @seealso \code{\link{mrs_age_schema}}
#' @examples
#' # The operations to obtain it from the `ft` data set are:
#'
#' if (rlang::is_installed("stringr")) {
#'   ft_age <- ft |>
#'     dplyr::select(-`Pneumonia and Influenza Deaths`, -`All Deaths`) |>
#'     tidyr::gather("Age", "All Deaths", 7:11) |>
#'     dplyr::mutate(`All Deaths` = as.integer(`All Deaths`)) |>
#'     dplyr::mutate(Age = stringr::str_replace(Age, " \\(all cause deaths\\)", ""))
#' }
#'
#' @format A `tibble`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"ft_age"

#' Star schema for Mortality Reporting System by Cause
#'
#' Definition of schemas for facts and dimensions for the Mortality Reporting System
#' considering the cause classification.
#'
#' Dimension schemes can be defined using variables so that you do not have to repeat
#' the definition in several multidimensional designs.
#'
#' @family mrs example schema
#' @seealso \code{\link{ft_num}}
#' @examples
#' # Defined by:
#'
#' when <- dimension_schema(name = "When",
#'                          attributes = c("Year"))
#' where <- dimension_schema(name = "Where",
#'                           attributes = c("REGION",
#'                                          "State",
#'                                          "City"))
#' mrs_cause_schema <- star_schema() |>
#'   define_facts(name = "MRS Cause",
#'                measures = c("Pneumonia and Influenza Deaths",
#'                             "All Deaths")) |>
#'   define_dimension(when) |>
#'   define_dimension(where)
#'
#' @format A `star_schema` object.
"mrs_cause_schema"

#' Star schema for Mortality Reporting System by Age
#'
#' Definition of schemas for facts and dimensions for the Mortality Reporting System
#' considering the age classification.
#'
#' Dimension schemes can be defined using variables so that you do not have to repeat
#' the definition in several multidimensional designs.
#'
#' @family mrs example schema
#' @seealso \code{\link{ft_age}}
#' @examples
#' # Defined by:
#'
#' when <- dimension_schema(name = "When",
#'                          attributes = c("Year"))
#' where <- dimension_schema(name = "Where",
#'                           attributes = c("REGION",
#'                                          "State",
#'                                          "City"))
#' mrs_age_schema <- star_schema() |>
#'   define_facts(name = "MRS Age",
#'                measures = c("All Deaths")) |>
#'   define_dimension(when) |>
#'   define_dimension(where) |>
#'   define_dimension(name = "Who",
#'                    attributes = c("Age"))
#'
#' @format A `star_schema` object.
"mrs_age_schema"

#' Mortality Reporting System by Age
#'
#' Selection of data from the 122 Cities Mortality Reporting System by age
#' group, for the first 9 weeks of 1962 and 4 cities.
#'
#' The original dataset begins in 1962. For each week, in 122 US cities,
#' mortality figures by age group and cause, considered separately, are included
#' (i.e., the combination of age group and cause is not included). In the cause,
#' only a distinction is made between pneumonia or influenza and others.
#'
#' Two additional dates have been generated, which were not present in the
#' original dataset.
#'
#' @family mrs example data
#' @seealso \code{\link{mrs_age_schema}}
#'
#' @format A `tibble`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"ft_age_rpd"

#' Mortality Reporting System by Cause
#'
#' Selection of data from the 122 Cities Mortality Reporting System by cause,
#' for the first 9 weeks of 1962 and 4 cities.
#'
#' The original dataset begins in 1962. For each week, in 122 US cities,
#' mortality figures by age group and cause, considered separately, are included
#' (i.e., the combination of age group and cause is not included). In the cause,
#' only a distinction is made between pneumonia or influenza and others.
#'
#' Two additional dates have been generated, which were not present in the
#' original dataset.
#'
#' @family mrs example data
#' @seealso \code{\link{mrs_cause_schema}}
#'
#' @format A `tibble`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"ft_cause_rpd"

#' Star schema for Mortality Reporting System by Cause with additional dates
#'
#' Definition of schemas for facts and dimensions for the Mortality Reporting System
#' considering the cause classification with additional dates to be used as role
#' playing dimensions..
#'
#' @family mrs example schema
#' @seealso \code{\link{ft_cause_rpd}}
#' @examples
#' # Defined by:
#'
#' mrs_cause_schema_rpd <- star_schema() |>
#'   define_facts(fact_schema(
#'     name = "mrs_cause",
#'     measures = c(
#'       "Pneumonia and Influenza Deaths",
#'       "All Deaths"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When",
#'     attributes = c(
#'       "Year",
#'       "WEEK",
#'       "Week Ending Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Available",
#'     attributes = c(
#'       "Data Availability Year",
#'       "Data Availability Week",
#'       "Data Availability Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Received",
#'     attributes = c(
#'       "Reception Year",
#'       "Reception Week",
#'       "Reception Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "where",
#'     attributes = c(
#'       "REGION",
#'       "State",
#'       "City"
#'     )
#'   ))
#'
#' @format A `star_schema` object.
"mrs_cause_schema_rpd"


#' Star schema for Mortality Reporting System by Age with additional dates
#'
#' Definition of schemas for facts and dimensions for the Mortality Reporting System
#' considering the cause classification with additional dates to be used as role
#' playing dimensions..
#'
#' @family mrs example schema
#' @seealso \code{\link{ft_age_rpd}}
#' @examples
#' # Defined by:
#'
#' mrs_age_schema_rpd <- star_schema() |>
#'   define_facts(fact_schema(
#'     name = "mrs_age",
#'     measures = c(
#'       "Deaths"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When",
#'     attributes = c(
#'       "Year",
#'       "WEEK",
#'       "Week Ending Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Available",
#'     attributes = c(
#'       "Data Availability Year",
#'       "Data Availability Week",
#'       "Data Availability Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "When Arrived",
#'     attributes = c(
#'       "Arrival Year",
#'       "Arrival Week",
#'       "Arrival Date"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "Who",
#'     attributes = c(
#'       "Age Range"
#'     )
#'   )) |>
#'   define_dimension(dimension_schema(
#'     name = "where",
#'     attributes = c(
#'       "REGION",
#'       "State",
#'       "City"
#'     )
#'   ))
#'
#' @format A `star_schema` object.
"mrs_age_schema_rpd"


#' Flat table generated from MRS file
#'
#' The original dataset covers from 1962 to 2016. For each week, in 122 US cities,
#' from the original file, we have stored in the package a file with the same
#' format as the original file but that includes only 1% of its data, selected at
#' random.
#'
#' @family mrs example data
#'
#' @format A `flat_table`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"mrs_ft"


#' Flat table generated from MRS file
#'
#' The original dataset covers from 1962 to 2016. For each week, in 122 US cities,
#' from the original file, we have stored in the package a file with the same
#' format as the original file but that includes only 0,1% of its data, selected at
#' random to test the incremental refresh.
#'
#' @family mrs example data
#'
#' @format A `flat_table`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"mrs_ft_new"


#' Constellation generated from MRS file
#'
#' The original dataset covers from 1962 to 2016. For each week, in 122 US cities,
#' from the original file, we have stored in the package a file with the same
#' format as the original file but that includes only 1% of its data, selected at
#' random.
#'
#' From these data the constellation in the vignette titled 'Obtaining and
#' transforming flat tables' has been generated. This variable contains the defined
#' constellation.
#'
#' @family mrs example data
#'
#' @format A `star_database`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"mrs_db"


#' Czech debit card company specialising on payments at gas stations (finest detail)
#'
#' Multidimensional design with finest detail from the data available at the source.
#'
#' @family debit card example data
#'
#' @format A `star_database`.
#' @source \url{https://fit.cvut.cz/cs}
"db_finest"


#' Czech debit card company specialising on payments at gas stations (summary)
#'
#' Multidimensional design with a summary from the data available at the source.
#'
#' @family debit card example data
#'
#' @format A `star_database`.
#' @source \url{https://fit.cvut.cz/cs}
"db_summary"


#' Census of US States, by sex and age
#'
#' Census of US States, by sex and age, obtained from the United States Census
#' Bureau (USCB), American Community Survey (ACS). Obtained from the variables
#' defined in reports, classifying the concepts according to the defined subjects.
#'
#' U.S. Census Bureau. “Government Units: US and State: Census Years 1942 - 2022.”
#' Public Sector, PUB Public Sector Annual Surveys and Census of Governments,
#' Table CG00ORG01, 2022,
#' https://data.census.gov/table/GOVSTIMESERIES.CG00ORG01?q=census+state+year.
#' Accessed on October 25, 2023.
#'
#' @format A `tibble`.
#' @source
#'   \url{https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-data.2021.html}
"us_census_state"


