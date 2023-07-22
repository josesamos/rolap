
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rolap

<!-- badges: start -->
<!-- badges: end -->

The goal of rolap is to …

## Installation

You can install the development version of rolap from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("josesamos/rolap")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(rolap)

s <- star_schema() |>
  define_facts(fact_schema(
    name = "mrs_cause",
    measures = c(
      "Pneumonia and Influenza Deaths",
      "Other Deaths"
    )
  )) |>
  define_dimension(dimension_schema(
    name = "when",
    attributes = c(
      "Week Ending Date",
      "WEEK",
      "Year"
    )
  )) |>
  define_dimension(dimension_schema(
    name = "where",
    attributes = c(
      "REGION",
      "State",
      "City"
    )
  ))
```
