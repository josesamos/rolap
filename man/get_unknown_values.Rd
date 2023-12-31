% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flat_table.R
\name{get_unknown_values}
\alias{get_unknown_values}
\alias{get_unknown_values.flat_table}
\title{Get unknown attribute values}
\usage{
get_unknown_values(ft, attributes, col_as_vector)

\method{get_unknown_values}{flat_table}(ft, attributes = NULL, col_as_vector = NULL)
}
\arguments{
\item{ft}{A \code{flat_table} object.}

\item{attributes}{A vector of strings, attribute names.}

\item{col_as_vector}{A string, name of the column to include a vector of values.}
}
\value{
A \code{tibble} with unknown values in instances.
}
\description{
Obtain the instances that have an empty or unknown value in any given attribute.
If no attribute is given, all are considered.
}
\details{
If a name is indicated in the \code{col_as_vector} parameter, it includes a column
with the data in vector form to be used in other functions.
}
\examples{

iris2 <- iris
iris2[10, 'Species'] <- NA
instances <- flat_table('iris', iris2) |>
  get_unknown_values()

}
\seealso{
\code{\link{star_database}}

Other flat table definition functions: 
\code{\link{as_star_database}()},
\code{\link{flat_table}()},
\code{\link{get_table}()},
\code{\link{get_unknown_value_defined}()},
\code{\link{read_flat_table_file}()},
\code{\link{read_flat_table_folder}()}
}
\concept{flat table definition functions}
