
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rolap

<!-- badges: start -->
<!-- badges: end -->

The goal of rolap is to …

## Installation

You can install the development version of rolap from
[GitHub](https://github.com/) with:

``` r
# install.packages("rolap")
devtools::install_github("josesamos/rolap")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rolap)

s <- star_schema() |>
  define_facts(
    name = "mrs_cause",
    measures = c(
      "Pneumonia and Influenza Deaths",
      "All Deaths"
    )
  ) |>
  define_dimension(
    name = "when",
    attributes = c(
      "Week Ending Date",
      "WEEK",
      "Year"
    )
  ) |>
  define_dimension(
    name = "where",
    attributes = c(
      "REGION",
      "State",
      "City"
    )
  )

ft$`Pneumonia and Influenza Deaths` <-
  as.integer(ft$`Pneumonia and Influenza Deaths`)
ft$`All Deaths` <- as.integer(ft$`All Deaths`)

db <- star_database(s, ft)
```
