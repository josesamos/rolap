% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geolayer.R
\name{set_variables}
\alias{set_variables}
\alias{set_variables.geolayer}
\title{Set variables layer}
\usage{
set_variables(gl, variables, keep_all_variables_na)

\method{set_variables}{geolayer}(gl, variables, keep_all_variables_na = FALSE)
}
\arguments{
\item{gl}{A \code{geolayer} object.}

\item{variables}{A \code{tibble} object.}

\item{keep_all_variables_na}{A boolean, keep rows with all variables NA.}
}
\value{
A \code{sf} object.
}
\description{
The variables layer includes the names and description through various fields
of the variables contained in the reports.
}
\details{
When we set the variables layer, after filtering it, the data layer is also
filtered keeping only the variables from the variables layer.

By default, rows that are NA for all variables are eliminated.
}
\examples{

gl <- mrs_db_geo |>
  as_geolayer()

v <- gl |>
  get_variables()

v <- v |>
  dplyr::filter(year == '1966' | year == '2016')

gl_sel <- gl |>
  set_variables(v)

}
\seealso{
Other query functions: 
\code{\link{as_GeoPackage}()},
\code{\link{as_geolayer}()},
\code{\link{filter_dimension}()},
\code{\link{get_layer}()},
\code{\link{get_variable_description}()},
\code{\link{get_variables}()},
\code{\link{run_query}()},
\code{\link{select_dimension}()},
\code{\link{select_fact}()},
\code{\link{set_layer}()},
\code{\link{star_query}()}
}
\concept{query functions}
