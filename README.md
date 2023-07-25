
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

Original data as a starting point, stored in `ft`.

| Year | WEEK | Week Ending Date | REGION | State |    City    | Pneumonia and Influenza Deaths | All Deaths | \<1 year (all cause deaths) | 1-24 years (all cause deaths) | 25-44 years | 45-64 years (all cause deaths) | 65+ years (all cause deaths) |
|:----:|:----:|:----------------:|:------:|:-----:|:----------:|:------------------------------:|:----------:|:---------------------------:|:-----------------------------:|:-----------:|:------------------------------:|:----------------------------:|
| 1962 |  2   |    01/13/1962    |   1    |  MA   |   Boston   |               11               |    270     |             14              |               8               |     11      |               70               |             167              |
| 1962 |  4   |    01/27/1962    |   1    |  MA   |   Boston   |               12               |    285     |             22              |               7               |      8      |               73               |             175              |
| 1963 |  4   |    01/26/1963    |   1    |  MA   |   Boston   |               10               |    276     |             11              |              14               |     17      |               67               |             167              |
| 1964 |  3   |    01/18/1964    |   1    |  MA   |   Boston   |               13               |    325     |             17              |               7               |     24      |               90               |             187              |
| 1964 |  6   |    02/08/1964    |   1    |  MA   |   Boston   |               9                |    244     |             13              |               9               |     14      |               61               |             147              |
| 1962 |  3   |    01/20/1962    |   1    |  CT   | Bridgeport |               2                |     40     |              5              |               1               |      3      |               10               |              21              |
| 1962 |  5   |    02/03/1962    |   1    |  CT   | Bridgeport |               5                |     46     |              6              |               0               |      3      |               15               |              22              |
| 1962 |  8   |    02/24/1962    |   1    |  CT   | Bridgeport |               2                |     45     |              2              |               0               |      2      |               16               |              25              |
| 1963 |  4   |    01/26/1963    |   1    |  CT   | Bridgeport |               2                |     46     |              4              |               0               |      3      |               10               |              29              |
| 1964 |  5   |    02/01/1964    |   1    |  CT   | Bridgeport |               8                |     45     |              3              |               1               |      2      |               11               |              28              |
| 1962 |  9   |    03/03/1962    |   1    |  MA   | Cambridge  |               4                |     39     |              1              |               0               |      2      |               7                |              29              |
| 1964 |  2   |    01/11/1964    |   1    |  MA   | Cambridge  |               7                |     31     |              1              |               0               |      2      |               9                |              19              |
| 1964 |  5   |    02/01/1964    |   1    |  MA   | Cambridge  |               6                |     27     |              2              |               0               |      0      |               8                |              17              |
| 1964 |  9   |    02/29/1964    |   1    |  MA   | Cambridge  |               0                |     26     |              0              |               0               |      2      |               8                |              16              |
| 1962 |  4   |    01/27/1962    |   1    |  CT   |  Hartford  |               1                |     47     |              7              |               1               |      0      |               14               |              25              |
| 1962 |  7   |    02/17/1962    |   1    |  CT   |  Hartford  |               4                |     57     |              3              |               1               |      3      |               21               |              29              |
| 1963 |  3   |    01/19/1963    |   1    |  CT   |  Hartford  |               2                |     66     |              7              |               2               |      3      |               18               |              36              |
| 1963 |  7   |    02/16/1963    |   1    |  CT   |  Hartford  |               4                |     77     |              6              |               1               |      7      |               19               |              44              |
| 1963 |  8   |    02/23/1963    |   1    |  CT   |  Hartford  |               6                |     49     |              3              |               2               |      3      |               14               |              27              |
| 1964 |  2   |    01/11/1964    |   1    |  CT   |  Hartford  |               3                |     53     |              7              |               0               |      2      |               16               |              28              |

This is a basic example which shows you how to solve a common problem:

``` r
library(rolap)

s <- star_schema() |>
  define_facts(
    name = "MRS Cause",
    measures = c(
      "Pneumonia and Influenza Deaths",
      "All Deaths"
    )
  ) |>
  define_dimension(
    name = "When",
    attributes = c(
      "Year"
    )
  ) |>
  define_dimension(
    name = "Where",
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

The tables of dimensions and facts of the obtained star database are
shown below.

| when_key | Year |
|:--------:|:----:|
|    1     | 1962 |
|    2     | 1963 |
|    3     | 1964 |

| where_key | REGION | State |    City    |
|:---------:|:------:|:-----:|:----------:|
|     1     |   1    |  CT   | Bridgeport |
|     2     |   1    |  CT   |  Hartford  |
|     3     |   1    |  MA   |   Boston   |
|     4     |   1    |  MA   | Cambridge  |

| when_key | where_key | Pneumonia and Influenza Deaths | All Deaths | nrow_agg |
|:--------:|:---------:|:------------------------------:|:----------:|:--------:|
|    1     |     1     |               9                |    131     |    3     |
|    1     |     2     |               5                |    104     |    2     |
|    1     |     3     |               23               |    555     |    2     |
|    1     |     4     |               4                |     39     |    1     |
|    2     |     1     |               2                |     46     |    1     |
|    2     |     2     |               12               |    192     |    3     |
|    2     |     3     |               10               |    276     |    1     |
|    3     |     1     |               8                |     45     |    1     |
|    3     |     2     |               3                |     53     |    1     |
|    3     |     3     |               22               |    569     |    2     |
|    3     |     4     |               13               |     84     |    3     |
