#' Mortality Reporting System
#'
#' Selection of 20 rows from the 122 Cities Mortality Reporting System.
#'
#' The original dataset covers from 1962 to 2016. For each week, in 122 US cities,
#' mortality figures by age group and cause, considered separately, are included.
#' In the cause, only a distinction is made between pneumonia or influenza and
#' others.
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
#' The operations to obtain it from ft data set are:
#'
#' ft |>
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
#' The operations to obtain it from ft data set are:
#'
#' ft_age <- ft |>
#'   dplyr::select(-`Pneumonia and Influenza Deaths`, -`All Deaths`) |>
#'   tidyr::gather("Age", "All Deaths", 7:11) |>
#'   dplyr::mutate(`All Deaths` = as.integer(`All Deaths`)) |>
#'   dplyr::mutate(Age = stringr::str_replace(Age, " \\(all cause deaths\\)", ""))
#'
#' @format A `tibble`.
#' @source \url{https://catalog.data.gov/dataset/deaths-in-122-u-s-cities-1962-2016-122-cities-mortality-reporting-system}
"ft_age"
