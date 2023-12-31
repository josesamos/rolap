% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/role_playing.R
\name{role_playing_dimension}
\alias{role_playing_dimension}
\alias{role_playing_dimension.star_database}
\title{Define a role playing dimension and its associated dimensions}
\usage{
role_playing_dimension(db, rpd, roles, rpd_att_names, att_names)

\method{role_playing_dimension}{star_database}(db, rpd, roles, rpd_att_names = FALSE, att_names = NULL)
}
\arguments{
\item{db}{A \code{star_database} object.}

\item{rpd}{A string, dimension name (role playing dimension).}

\item{roles}{A vector of strings, dimension names (dimension roles).}

\item{rpd_att_names}{A boolean, common attribute names taken from rpd dimension.}

\item{att_names}{A vector of strings, common attribute names.}
}
\value{
A \code{star_database} object.
}
\description{
The same dimension can play several roles in relation to the facts. We can
define the main dimension and the dimensions that play different roles.
}
\details{
As a result, all the dimensions will have the same instances and, if we deem
it necessary, also the same name of their attributes (except the surrogate key).
}
\examples{

s <- star_schema() |>
  define_facts(fact_schema(
    name = "mrs_cause",
    measures = c(
      "Pneumonia and Influenza Deaths",
      "All Deaths"
    )
  )) |>
  define_dimension(dimension_schema(
    name = "When",
    attributes = c(
      "Year",
      "WEEK",
      "Week Ending Date"
    )
  )) |>
  define_dimension(dimension_schema(
    name = "When Available",
    attributes = c(
      "Data Availability Year",
      "Data Availability Week",
      "Data Availability Date"
    )
  )) |>
  define_dimension(dimension_schema(
    name = "When Received",
    attributes = c(
      "Reception Year",
      "Reception Week",
      "Reception Date"
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

db <- star_database(s, ft_cause_rpd) |>
  role_playing_dimension(
    rpd = "When",
    roles = c("When Available", "When Received"),
    rpd_att_names = TRUE
  )

db <- star_database(s, ft_cause_rpd) |>
  role_playing_dimension("When",
                         c("When Available", "When Received"),
                         att_names = c("Year", "Week", "Date"))

}
\seealso{
\code{\link{star_schema}}, \code{\link{flat_table}}

Other star database definition functions: 
\code{\link{get_dimension_names}()},
\code{\link{get_dimension_table}()},
\code{\link{get_fact_names}()},
\code{\link{get_role_playing_dimension_names}()},
\code{\link{get_table_names}()},
\code{\link{group_dimension_instances}()},
\code{\link{star_database}()}
}
\concept{star database definition functions}
