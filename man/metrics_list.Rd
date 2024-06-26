% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/metrics_list.R
\name{metrics_list}
\alias{metrics_list}
\title{Metrics list}
\usage{
metrics_list(
  level = NULL,
  metric = NULL,
  name = NULL,
  type = NULL,
  what = NULL
)
}
\arguments{
\item{level}{Character vector. Level of metrics. Either "patch", "class" or "landscape"
(or a vector with a combination of these). Default NULL considers all levels.}

\item{metric}{Abbreviation of metrics (e.g. "area").}

\item{name}{Full name of metrics (e.g. "core area").}

\item{type}{Character vector. Type according to FRAGSTATS grouping. One or more of the following:
"area and edge", "core area", "shape", "aggregation", "complexity", and or "diversity". Default
NULL considers all types.}

\item{what}{Selected level of metrics: either "patch", "class" or "landscape". It is also
possible to specify functions as a vector of strings, e.g. what = c("lsm_c_ca", "lsm_l_ta").}
}
\value{
A data.frame with the list of available landscape metrics, including information
regarding the level, type, metric, name and function name provided by package \link{landscapemetrics}.
}
\description{
List of available landscape metrics provided by package \link{landscapemetrics} to be calculated with
\code{\link[=mland_metrics]{mland_metrics()}}. It simply calls \code{\link[landscapemetrics:list_lsm]{landscapemetrics::list_lsm()}}. For more information regarding the
definition and equations of metrics, please check the user manual of \link{landscapemetrics}.
}
\references{
Hesselbarth, M.H.K., Sciaini, M., With, K.A., Wiegand, K., Nowosad, J. 2019. landscapemetrics:
an open-source R tool to calculate landscape metrics. - Ecography 42:1648-1657(ver. 0).

McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis Program for
Categorical and Continuous Maps. Computer software program produced by the authors at the
University of Massachusetts, Amherst. \cr
Available at the following web site: https://www.umass.edu/landeco/
}
