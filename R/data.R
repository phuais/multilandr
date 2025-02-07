#' 'MultiLandMetrics' object
#'
#' An object of class 'MultiLandMetrics' generated with [mland_metrics()], for the purposes of package
#' examples of the following functions: [metrics_filter()], [metrics_gradient()],
#' [metrics_corr()], [metrics_plots()] and [metrics_bind()]. See 'MultiLand-class'
#' for general information about these objects.
#'
#' The main internal object is a data.frame (accesible through `ed_metrics@data`) with information
#' about the values of two landscape metrics: "pland" (percentage of landscape) and "ed" (edge density).
#'
#' The object was created from the MultiLand object named "ernesdesign", which
#' received two raster layers from a small portion of the ecoregion "El Chaco" as main inputs. The main rasterlayer
#' was provided by the project "MapBiomas Chaco" for the year 2000.
#' The extra rasterlayer contained the NDVI values of cells within the same extent of the main rasterlayer, and was provided by Landsat.
#'
#' @seealso See the examples sections of [mland_metrics()] and [mland()]
#' for more context.
#'
#' @references
#' Project MapBiomas Chaco – Collection 4.0 of annual land cover and land use maps,
#' accessed during July 2022 through the following link: \href{https://chaco.mapbiomas.org/}{MapBiomas Chaco}
#'
#' Landsat-5 image courtesy of the U.S. Geological Survey
#'
"ed_metrics"

#' 'MultiLandMetrics' object
#'
#' An object of class 'MultiLandMetrics' generated with [mland_metrics()], for the purposes of package
#' examples of the following functions: [metrics_filter()], [metrics_gradient()],
#' [metrics_corr()], [metrics_plots()] and [metrics_bind()]. See 'MultiLand-class' for general
#' information about these objects.
#'
#' The main internal object is a data.frame (accesible through `otf_metrics@data`) with information about the values of two landscape metrics:
#' "pland" (percentage of landscape) and "np" (number of patches).
#'
#' The object was created from the MultiLand object named "otf_design", which
#' received a raster layer from a small portion of the ecoregion "El Chaco" as main input.
#' The rasterlayer was provided by the project "MapBiomas Chaco" for the year 2000.
#'
#' @seealso See the examples sections of [mland_metrics()] and [mland()]
#' for more context.
#'
#' #' @references
#' Project MapBiomas Chaco – Collection 4.0 of annual land cover and land use maps,
#' accessed during July 2022 through the following link: \href{https://chaco.mapbiomas.org/}{MapBiomas Chaco}
"otf_metrics"

#' multilandr_data
#'
#' A character vector containing the links to external data (.tif and .gpkg files) to be used in examples.
"multilandr_data"
