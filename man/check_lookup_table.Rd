% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flat_table_join.R
\name{check_lookup_table}
\alias{check_lookup_table}
\alias{check_lookup_table.flat_table}
\title{Check the result of joining a flat table with a lookup table}
\usage{
check_lookup_table(ft, fk_attributes, lookup)

\method{check_lookup_table}{flat_table}(ft, fk_attributes = NULL, lookup)
}
\arguments{
\item{ft}{A \code{flat_table} object.}

\item{fk_attributes}{A vector of strings, attribute names.}

\item{lookup}{A \code{flat_table} object.}
}
\value{
A \code{tibble} with attribute values.
}
\description{
Before joining a flat table with a lookup table we can check the result to
determine if we need to adapt the values of some instances or add new elements
to the lookup table. This function returns the values of the foreign key of
the flat table that do not correspond to the primary key of the lookup table.
}
\details{
If no attributes are indicated, those that form the primary key of the lookup
table are considered in the flat table.
}
\examples{

lookup <- flat_table('iris', iris) |>
  lookup_table(
    measures = c("Sepal.Length", "Sepal.Width", "Petal.Length", "Petal.Width"),
    measure_agg = c('MAX', 'MIN', 'SUM', 'MEAN')
  )
values <- flat_table('iris', iris) |>
  check_lookup_table(lookup = lookup)

}
\seealso{
\code{\link{flat_table}}

Other flat table join functions: 
\code{\link{get_pk_attribute_names}()},
\code{\link{join_lookup_table}()},
\code{\link{lookup_table}()}
}
\concept{flat table join functions}
