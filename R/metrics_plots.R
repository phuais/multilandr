#' Pairwise metric plots
#'
#' Plots pair of metric values in two-dimensional plots.
#'
#' @inheritParams mland_metrics
#' @param raster,ext_raster,classes,radii,l_level,c_level Parameters to subset plots. See Details.
#' @param show_class_names logical. Whether to show classes with its previously defined names (if defined)
#' when generating the 'MultiLand' object (TRUE), or not (FALSE, default).
#' @param smooth logical. If TRUE (default) a pattern between the pair of metric values
#' is plotted, with a smoothing method as defined in `method`.
#' @param method Smoothing method (function) to use, as in [ggplot2::geom_smooth()]. It accepts
#' "loess" (default), "lm", "gam", among others. See ?ggplot2::geom_smooth() for more details.
#' @param se logical. Whether to show (TRUE) or not (FALSE) confidence intervals when `smooth = TRUE`.
#' @param ... Other parameters to be passed to [ggplot2::geom_smooth()], if `smooth = TRUE`.
#' @param upper logical. Whether to plot upper-diagonal plots or not. Default TRUE
#' @param diag logical. Whether to plot diagonal density plots or not. Default TRUE.
#' @param st_points List of aesthetic arguments for points plotting:
#'   \code{shape} for points shape, \code{size} for points size, \code{col} for
#'   points border color, \code{fill} for points fill color and \code{alpha} for point transparency.
#' @param st_lines List of aesthetic arguments for lines plotting (if `smooth = TRUE`):
#'   \code{lty} for linetype, \code{lwd} for linewidth,
#'   \code{col} for line color and \code{alpha} for line transparency.
#'
#' @details [metrics_plots()] mainly relies on [GGally::ggpairs()] to generate pair plots
#' between metrics values. Arguments `upper` and `diag` are specific arguments of
#' [GGally::ggpairs()], here adapted to the context of continuous values only.
#'
#' Argument `raster`, `classes`, `radii`, `l_level` and `c_level` can be defined to
#' subset the plots. In each one of these, an all-positive or an
#' all-negative vector can be passed, whether to include (all-positive) or exclude (all-negative)
#' the elements to be taken into account for the subsetting:
#' * rasterlayers: a numeric vector with the number of the raster layers to be included/excluded.
#' For example: `c(1, 2, 4)` to include raster layers 1, 2 and 4; `c(-2, -3)` to exclude raster layers 2
#' and 3.
#' * classes: must be a list with as many elements as defined raster layers in argument
#' `raster`. Each element of the list must be a numeric vector (classes identities) with the
#' classes to be included/excluded. If provided a character vector, [metrics_corr()] assumes that
#' classes names are provided. For example, for the case with 2 raster layers:
#' `list(c(3, 20, 35), c("Forest", "Crops"))` would include classes 3, 20 and 35 from raster layer 1
#' and classes "Forest" and "Crops" for raster layer 2. For the case of a unique raster layer, there
#' is no need to input a list. For example, for the case of a unique raster layer and the
#' exclusion of some classes: `c(-5, -10, -15)` to exclude classes 5, 10 and 15 of
#' the unique raster layer; `c("-Forest", "-Grassland")` to exclude classes "Forest" and "Grassland".
#' Note the "-" before each class name to indicate the exclusion of the classes.
#' * radii: a numeric vector to include/exclude particular radii. For example: c(1000, 2000) to
#' include only radii of 1000 and 2000 m; c(-500, -1500) to exclude radii of 500 and 1500 m.
#' * c_level: character vector with the class-level metrics to be included/excluded from
#' the analysis. For example: `c("np", "pland")` will include only the metrics "number of patches"
#' ("np") and "percentage of the landscape" ("pland") in the analysis, whereas `c("-np", "-pland")`
#' will exclude them. Note the "-" before each metric name to indicate the exclusion of the
#' metrics.
#' * l_level: character vector with the landscape-level metrics to be included/excluded from
#' the analysis. Other calculations for extra raster layers are considered as landscape-level metrics,
#' and must be provided as "fun_" + the name of the function (e.g. "fun_mean").
#'
#' Names of the available metrics of the 'MultiLandMetrics' object provided in `x` can
#' be accessed with `x@metrics` and `x@ext_calc`.
#'
#' Note that patch-level metrics, if exists in `x` metric's data.frame, are excluded from
#' calculations, as this function works at a landscape scale.
#'
#' @return A panel with several plots returned by [GGally::ggpairs()] relating pair of metrics
#' values. Metrics
#' names are presented at the top and right of the panel (strips), with the following format:
#' "level"_"metric_name"_"radius". For a landscape-level metric, a plausible metric name could be
#' "l_np_1500" indicating a landscape-level metric, which is "np" ("number of patches") at a scale
#' (radius) of 1500 m. For a class-level metric a plausible metric name could be "c4_pland_1000",
#' indicating a class-level metric of class 4 (the value of the raster), which is "pland"
#' ("percentage of landscape") at a scale (radius) of 1000 m. If more that one rasterlayer is
#' being analyzed, the prefix "r1", "r2", "r3", ..., "rn" (referring to rasterlayer 1, 2, 3, ..., n) is
#' added to the metric name.
#'
#' @seealso [mland_metrics()], [metrics_corr()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Pair plots between metrics "pland" of classes 1 to 4, for radius 3000 m
#' metrics_plots(ed_metrics, classes = 1:4, radii = 3000, show_class_names = TRUE,
#'               c_level = "pland")
#'
#' # Without smooth pattern
#' metrics_plots(ed_metrics, classes = 1:4, radii = 3000, show_class_names = TRUE,
#'               c_level = "pland", smooth = FALSE)
#'
#' # Changing aesthetics
#' metrics_plots(ed_metrics, classes = 1:4, radii = 3000, show_class_names = TRUE,
#'               c_level = "pland", smooth = FALSE, size = 1.5, shape = 21,
#'               fill = "red", alpha = 0.4)
#'
#' # Assessing two radii values at the same time
#' metrics_plots(ed_metrics, classes = 1:4, radii = c(1000, 5000),
#'               show_class_names = TRUE, c_level = "pland", smooth = FALSE,
#'               size = 1.5, shape = 21, fill = "red", alpha = 0.4)
#'
#' # An example with hundreds of points
#' metrics_plots(otf_metrics, classes = c("Forest", "Crops"))
#'
#' # Plots can be combined with ggplot2::theme
#' # library(ggplot2)
#' metrics_plots(otf_metrics, classes = c("Forest", "Crops")) +
#'   theme_bw()
#' }
metrics_plots <- function(x,
                          raster = NULL,
                          classes = NULL,
                          radii = NULL,
                          c_level = NULL,
                          l_level = NULL,
                          ext_raster = NULL,
                          show_class_names = FALSE,
                          upper = TRUE,
                          diag = TRUE,
                          smooth = TRUE,
                          method = "loess",
                          se = FALSE,
                          st_points = list(shape = 21,
                                           size = 2,
                                           col = "black",
                                           fill = "white",
                                           alpha = 1),
                          st_lines = list(lty = 1,
                                          lwd = 1,
                                          col = "black",
                                          alpha = 0.6),
                          ...){

  if(!is(x, "MultiLandMetrics")) stop("x must be an object of class 'MultiLandMetrics'.")
  environment(.metrics_plots_chk_args) <- environment()
  chk <- .metrics_plots_chk_args()
  if(length(chk[[1]]) > 0)
    for(w in 1:length(chk[[1]])){
      warning(strwrap(chk[[1]], prefix = "\n", initial = ""), call. = FALSE)
    }
  if(length(chk[[2]]) > 0){
    errors <- chk[[2]]
    stop(strwrap(errors, prefix = "\n", initial = "\n"))
  } else {
    objs <- names(chk)
    for(i in 3:length(chk)){ assign(objs[i], chk[[i]]) }
  }

  towide <- T
  environment(.pair_subsets) <- environment()
  df <- tryCatch(.pair_subsets(),
                 error = function(e){
                   message("")
                   stop(e)})
  new_df_wide <- df[[1]]

  # asks if it is okay to plot so many plots
  if(length(4:ncol(new_df_wide)) > 10){
    ask <- askYesNo("You are attempting to plot more than one hundred plots. Are you sure?")
    if(is.na(ask) | !ask) stop("Operation cancelled")
  }

  # plotting function, combining points with geom_smooth
  # Credit to user20650 from stackoverflow
  my_fn <- function(data, mapping, meth = method, ...){
    p <- ggplot2::ggplot(data = data, mapping = mapping) +
      ggplot2::geom_point(shape = st_points$shape, fill = st_points$fill,
                          col = st_points$col, alpha = st_points$alpha,
                          size = st_points$size, na.rm = T)

    if(smooth){
      p <- p + ggplot2::geom_smooth(method = meth, formula = y ~ x, se = se, na.rm = T,
                                    lty = st_lines$lty, lwd = st_lines$lwd,
                                    col = ggplot2::alpha(st_lines$col, st_lines$alpha), ...)
    }
    p
  }

  # Pair scatterplots
  if(upper) upper <- list(continuous = my_fn) else upper = "blank"
  if(diag) diag <- list(continuous = "densityDiag") else diag = list(continuous = "blankDiag")
  suppressWarnings(GGally::ggpairs(new_df_wide[3:ncol(new_df_wide)],
                                   lower = list(continuous = my_fn),
                                   upper = upper, diag = diag, progress = F))
}

