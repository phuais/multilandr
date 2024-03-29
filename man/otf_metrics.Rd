% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{otf_metrics}
\alias{otf_metrics}
\title{'MultiLandMetrics' object}
\format{
An object of class \code{MultiLandMetrics} of length 1.
}
\usage{
otf_metrics
}
\description{
An object of class 'MultiLandMetrics' generated with \code{\link[=metrics]{metrics()}}, for the purposes of package
examples of the following functions: \code{\link[=landscape_filter]{landscape_filter()}}, \code{\link[=optimize_gradient]{optimize_gradient()}}, \code{\link[=pair_corr]{pair_corr()}},
\code{\link[=pair_plots]{pair_plots()}} and \code{\link[=bind_metrics]{bind_metrics()}}. See 'MultiLand-class' for general information about these objects.
}
\details{
The main internal object is a data.frame (accesible through \code{otf_metrics@data}) with information about the values of two landscape metrics:
"pland" (percentage of landscape) and "np" (number of patches).

The object was created from the MultiLand object named "otf_design", which
received a raster layer from a small portion of the ecoregion "El Chaco" as main input.
The rasterlayer was provided by the project "MapBiomas Chaco" for the year 2000.
}
\seealso{
See the examples sections of \code{\link[=metrics]{metrics()}} and \code{\link[=mland]{mland()}}
for more context.

#' @references
Project MapBiomas Chaco – Collection 4.0 of annual land cover and land use maps,
accessed during July 2022 through the following link: \href{https://chaco.mapbiomas.org/}{MapBiomas Chaco}
}
\keyword{datasets}
