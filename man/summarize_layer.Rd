% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geoattribute.R
\name{summarize_layer}
\alias{summarize_layer}
\title{Summarize geometry of a layer}
\usage{
summarize_layer(layer, attribute)
}
\arguments{
\item{layer}{A \code{sf} object.}

\item{attribute}{A string, attribute name.}
}
\value{
A \code{sf} object.
}
\description{
Groups the geometric elements of a layer according to the values of the indicated
attribute.
}
\examples{

layer <-
  summarize_layer(us_layer_state, "REGION")

}
\seealso{
Other star database geographic attributes: 
\code{\link{check_geoattribute_geometry}()},
\code{\link{define_geoattribute}()},
\code{\link{get_geoattribute_geometries}()},
\code{\link{get_geoattributes}()},
\code{\link{get_layer_geometry}()},
\code{\link{get_point_geometry}()}
}
\concept{star database geographic attributes}
