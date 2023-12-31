% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoattribute.R
\name{define_geoattribute_from_layer}
\alias{define_geoattribute_from_layer}
\title{Define geoattribute from a layer}
\usage{
define_geoattribute_from_layer(
  db,
  dimension = NULL,
  attribute = NULL,
  geoatt = NULL,
  from_layer = NULL,
  by = NULL
)
}
\arguments{
\item{db}{A \code{star_database} object.}

\item{dimension}{A string, dimension name.}

\item{attribute}{A string, attribute name.}

\item{geoatt}{A string, geoattribute name.}

\item{from_layer}{A \code{sf} object}

\item{by}{a vector of correspondence of attributes of the dimension with the
\code{sf} structure.}
}
\value{
A \code{star_database} object.
}
\description{
Define geoattribute from a layer
}
\keyword{internal}
