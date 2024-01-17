#' 'MultiLandMetrics' object
#'
#' An object of class 'MultiLandMetrics' generated with [metrics()], for the purposes of package
#' examples of the following functions: [landscape_filter()], [optimize_gradient()], [pair_corr()],
#' [pair_plots()] and [bind_metrics()]. See 'MultiLand-class' for general information about these objects.
#'
#' The main internal object is a data.frame (accesible through `ed_metrics@data`) with information
#' about the values of two landscape metrics: "pland" (percentage of landscape) and "np" (number of
#' patches). The object was created from the MultiLand object named "ernesdesign".
#'
#' @seealso See the examples sections of [metrics()] and [mland()]
#' for more context.
"ed_metrics"

#' 'MultiLandMetrics' object
#'
#' An object of class 'MultiLandMetrics' generated with [metrics()], for the purposes of package
#' examples of the following functions: [landscape_filter()], [optimize_gradient()], [pair_corr()],
#' [pair_plots()] and [bind_metrics()]. See 'MultiLand-class' for general information about these objects.
#'
#' The main internal object is a data.frame (accesible through `otf_metrics@data`) with information about the values of two landscape metrics:
#' "pland" (percentage of landscape) and "np" (number of patches). The object was created from
#' the MultiLand object named "otf_design".
#'
#' @seealso See the examples sections of [metrics()] and [mland()]
#' for more context.
"otf_metrics"
