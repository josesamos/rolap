% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flat_table_transform.R
\name{transform_from_values}
\alias{transform_from_values}
\alias{transform_from_values.flat_table}
\title{Transform attribute values into measure names}
\usage{
transform_from_values(ft, attribute)

\method{transform_from_values}{flat_table}(ft, attribute = NULL)
}
\arguments{
\item{ft}{A \code{flat_table} object.}

\item{attribute}{A string, attribute that stores the measures names.}
}
\value{
A \code{flat_table} object.
}
\description{
The values of an attribute will become measure names. There can only be one
measure that will be from where the new defined measures take the values.
}
\examples{

ft <- flat_table('iris', iris) |>
  transform_to_values(attribute = 'Characteristic',
                      measure = 'Value',
                      id_reverse = 'id')
ft <- ft |>
  transform_from_values(attribute = 'Characteristic')

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
\code{\link{transform_to_attribute}()},
\code{\link{transform_to_measure}()},
\code{\link{transform_to_values}()}
}
\concept{flat table transformation functions}
