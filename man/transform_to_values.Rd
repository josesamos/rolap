% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flat_table_transform.R
\name{transform_to_values}
\alias{transform_to_values}
\alias{transform_to_values.flat_table}
\title{Transform measure names into attribute values}
\usage{
transform_to_values(ft, attribute, measure, id_reverse, na_rm)

\method{transform_to_values}{flat_table}(
  ft,
  attribute = NULL,
  measure = NULL,
  id_reverse = NULL,
  na_rm = TRUE
)
}
\arguments{
\item{ft}{A \code{flat_table} object.}

\item{attribute}{A string, new attribute that will store the measures names.}

\item{measure}{A string, new measure that will store the measure value.}

\item{id_reverse}{A string, name of a new attribute that will store the row id.}

\item{na_rm}{A boolean, remove rows from output where the value column is NA.}
}
\value{
A \code{flat_table} object.
}
\description{
Transforms the measure names into values of a new attribute. The values of
the measures will become values of the new measure that is indicated.
}
\details{
If we wanted to perform the reverse operation later using the \code{transform_from_values}
function, we would need to uniquely identify each original row. By indicating
a value in the \code{id_reverse} parameter, an identifier is added that will allow
us to always carry out the inverse operation.
}
\examples{

ft <- flat_table('iris', iris) |>
  transform_to_values(attribute = 'Characteristic',
                      measure = 'Value')

ft <- flat_table('iris', iris) |>
  transform_to_values(attribute = 'Characteristic',
                      measure = 'Value',
                      id_reverse = 'id')

}
\seealso{
\code{\link{flat_table}}

Other flat table transformation functions: 
\code{\link{add_custom_column}()},
\code{\link{remove_instances_without_measures}()},
\code{\link{replace_empty_values}()},
\code{\link{replace_string}()},
\code{\link{replace_unknown_values}()},
\code{\link{select_attributes}()},
\code{\link{select_instances}()},
\code{\link{select_instances_by_comparison}()},
\code{\link{select_measures}()},
\code{\link{separate_measures}()},
\code{\link{transform_attribute_format}()},
\code{\link{transform_from_values}()},
\code{\link{transform_to_attribute}()},
\code{\link{transform_to_measure}()}
}
\concept{flat table transformation functions}
