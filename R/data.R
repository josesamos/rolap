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
