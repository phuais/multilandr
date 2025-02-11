#' Scalograms
#'
#' Plots the value of metrics across different spatial scales (radius)
#'
#' @param x An object of class 'MultiLandMetrics' generated with [mland_metrics()].
#' @param points Numeric or character vector of points to be considered. See Details.
#' @param raster,ext_raster,classes,radii,l_level,c_level Parameters to select
#' what to plot. See Details.
#' @param show_class_names Logical. If TRUE, raster classes will be identified
#' with the names of the classes, if available in `x`. Default FALSE.
#' @param aggregation Logical. Should data be aggregated by site? See Details.
#' @param fun Function to apply during aggregation. Default is "mean". See Details.
#' @param ... Parameters passed to ggplot2::geom_line().
#'
#' @details [metrics_scalogram()] generates scalograms. In these plots, the value
#' of a landscape metric is plotted in relation to different spatial scales (which
#' in this context are defined by the radii of buffers) (Wu, 2004). Curves are disaggregated by
#' raster classes (if applies), and a label named "landscape" is provided for those landscape-level
#' metrics.
#'
#' If argument `points` is a character vector,
#' [metrics_scalogram()] will assume that the 'MultiLandMetrics' object inputted in argument `x` contains
#' the identification names of each site/point. Therefore, the inputted values in argument `points` will be taken as these
#' identification names. Otherwise, if a numeric vector is inputted, these values
#' will be taken as point ids.
#'
#' Arguments `raster`, `ext_raster`, `classes`, `radii`, `c_level` and `l_level`
#' can be defined to select what metrics, classes, raster layers and radii will
#' be considered for plotting. In each one of these, an all-positive or an
#' all-negative vector can be passed, whether to include (all-positive) or
#' exclude (all-negative) the elements to be taken into account for the
#' selection:
#' * raster: a numeric vector with the number of the raster layers to be included/excluded.
#' For example: `c(1, 2, 4)` to include raster layers 1, 2 and 4; `c(-2, -3)` to exclude raster layers 2
#' and 3.
#' * ext_raster: a numeric vector with the number of the extra raster layers to be included/excluded,
#' as in the raster slot.
#' * classes: must be a list with as many elements as defined raster layers in argument
#' `raster`. Each element of the list must be a numeric vector (classes identities) with the
#' classes to be included/excluded. If provided a character vector, [metrics_scalogram()] assumes that
#' classes names are provided. For example, for the case with 2 raster layers:
#' `list(c(3, 20, 35), c("Forest", "Crops"))` would include classes 3, 20 and 35 from raster layer 1
#' and classes "Forest" and "Crops" for raster layer 2. For the case of a unique raster layer, there
#' is no need to input a list. For example, for the case of a unique raster layer and the
#' exclusion of some classes: `c(-5, -10, -15)` to exclude classes 5, 10 and 15 of
#' the unique raster layer; `c("-Forest", "-Grassland")` to exclude classes "Forest" and "Grassland".
#' Note the "-" before each class name to indicate the exclusion of the classes.
#' * radii: a numeric vector to include/exclude particular radii. For example: `c(1000, 2000)` to
#' include only radii of 1000 and 2000 m; `c(-500, -1500)` to exclude radii of 500 and 1500 m.
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
#' If `aggregation` is TRUE, the values of the selected metrics for different sites will be aggregated.
#' By default, for each spatial scale and raster layer, [metrics_scalogram()] will
#' calculate the mean value from the values of all available sites/points. A different
#' function (could be user-defined) can be provided in argument `fun`.
#'
#' @return A panel with ggplot2 facet plots relating the value of the provided
#' metrics and the radii. Plots are
#'
#' @references Wu, J. (2004). Effects of changing scale on landscape pattern analysis: scaling relations. Landscape ecology, 19, 125-138.
#'
#' @export
#'
#' @examples
#' \donttest{
#' # We will generate a 'MultiLand' obejct with several radii. The objective is
#' # evaluate metrics across a wide range of spatial scales
#'
#' # Loads main raster with land covers
#' elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
#'
#' # Loads extra raster with NDVI values
#' elchaco_ndvi <- terra::rast(system.file("extdata", "elchaco_ndvi.tif", package = "multilandr"))
#'
#' # Classes names
#' cl_names <- c(1, "Forest",
#'               2, "Grassland",
#'               3, "Crops",
#'               4, "Pastures",
#'               5, "Water",
#'               6, "Urban")
#'
#' # Loads points
#' elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg", package = "multilandr"))
#'
#' # Creates 'MultiLand' object by loading main raster, an extra raster and points.
#' ernesdesign1 <- mland(points_layer = elchaco_sites,
#'                       rast_layer = elchaco,
#'                       radii = seq(500, 5000, 100),
#'                       class_names = list(cl_names),
#'                       site_ref = "name",
#'                       ext_rast_layer = elchaco_ndvi,
#'                       rast_names = c("landcover", "NDVI"),
#'                       segs = 20)
#'
#' # Now, we calculate two metrics: the number of patches for each class
#' # and the total edge considering all classes (i.e. a landscape-level class)
#' ed_metrics5 <- mland_metrics(ernesdesign1,
#'                              what = c("lsm_c_np", "lsm_l_te"),
#'                              ext_calc = list(c(1, "mean")))
#'
#' # Plots scalogram for 3 different sites/landscapes, for raster layer "landcover"
#' # and metric "np"
#' metrics_scalogram(ed_metrics5, points = c("Algarrobo", "Peje", "Itin"),
#'                   raster = "landcover", c_level = "np",
#'                  aggregation = FALSE, show_class_names = TRUE)
#'
#' # Scalogram with aggregation across sites. By default, a mean value among all
#' # considered sites is calculated.
#' metrics_scalogram(ed_metrics5, raster = 1,
#'                   aggregation = TRUE, show_class_names = TRUE)
#'
#' # Here, we only plot those metrics calculated for the extra raster layer
#' # named "NDVI", which in this case is only one metric
#' metrics_scalogram(ed_metrics5, points = c("Algarrobo", "Peje", "Itin"),
#'                   ext_raster = "NDVI",
#'                   aggregation = FALSE, show_class_names = TRUE)
#'
#' # Scalogram with aggregation across three sites. By default, a mean value among
#' # the three considered sites is calculated.
#' metrics_scalogram(ed_metrics5, ext_raster = 1,
#'                   points = c("Yuchan", "Coco", "Tala"),
#'                   aggregation = FALSE, show_class_names = TRUE)
#'
#' # The output can be customized as every ggplot object
#' library(ggplot2)
#' metrics_scalogram(ed_metrics5, points = c("Algarrobo", "Peje", "Itin"), raster = 1,
#'                   c_level = "np",
#'                   aggregation = FALSE, show_class_names = TRUE, lwd = 1) +
#'   scale_color_brewer(type = "div", palette = 1) +
#'   theme_bw() +
#'   theme(aspect.ratio = 1, legend.title = element_blank())
#' }
metrics_scalogram <- function(x,
                              raster = NULL,
                              points = NULL,
                              classes = NULL,
                              radii = NULL,
                              c_level = NULL,
                              l_level = NULL,
                              ext_raster = NULL,
                              show_class_names = FALSE,
                              aggregation = FALSE,
                              fun = "mean",
                              ...){

  if(!is(x, "MultiLandMetrics")) stop("x must be an object of class 'MultiLandMetrics'.")
  environment(.metrics_scalogram_chk_args) <- environment()
  chk <- .metrics_scalogram_chk_args()
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

  # if points is null, take all of them
  if(is.null(points)){
    points <- x@points$id
  }

  towide <- F
  environment(.pair_subsets) <- environment()
  df <- tryCatch(.pair_subsets(),
                 error = function(e){
                   message("")
                   stop(e)})

  df <- df[df$point_id %in% points, ]

  if(show_class_names) cn <- "classname" else cn <- "class"

  # Set "landscape" as the label for landscape-level metrics
  if(any(is.na(df[, cn]))){
    df[is.na(df$class), "class"] <- df[is.na(df$classname), "classname"] <- "landscape"
    lvs <- unique(df[, cn])
    lvs <- lvs[-which(lvs == "landscape")]
    lvs <- c(lvs, "landscape")
    df[, cn] <- factor(df[, cn], levels = lvs)
  }

  # Change names for facet labels
  df$rasterlayer <- paste0("raster: ", df$rasterlayer)
  df$site <- paste0("site: ", df$site)
  df$metric <- paste0("metric: ", df$metric)

  if(aggregation){
    df <- aggregate(value ~ radius + class + classname + metric + rasterlayer, df,
                    FUN = fun, na.action = na.exclude)
    nplots <- nrow(unique(df[, c("rasterlayer", "metric")]))
    # asks if it is okay to plot so many plots
    if(nplots > 100){
      ask <- askYesNo("You are attempting to plot more than one hundred faceted plots. Are you sure?")
      if(is.na(ask) | !ask) stop("Operation cancelled")
    }
    plot <- suppressMessages(ggplot2::ggplot(df, ggplot2::aes(x = radius, y = value, col = get(cn))) +
                               ggplot2::geom_line(na.rm = T, ...) +
                               ggplot2::theme(legend.title = ggplot2::element_blank()))
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(metric, rasterlayer), scales = "free")
  } else {
    nplots <- nrow(unique(df[, c("rasterlayer", "site", "metric")]))
    # asks if it is okay to plot so many plots
    if(nplots > 100){
      ask <- askYesNo("You are attempting to plot more than one hundred faceted plots. Are you sure?")
      if(is.na(ask) | !ask) stop("Operation cancelled")
    }
    plot <- suppressMessages(ggplot2::ggplot(df, ggplot2::aes(x = radius, y = value, col = get(cn))) +
                               ggplot2::geom_line(na.rm = T, ...) +
                               ggplot2::theme(legend.title = ggplot2::element_blank()))
    plot <- plot +
      ggplot2::facet_wrap(ggplot2::vars(metric, rasterlayer, site), scales = "free")
  }

  plot +
    ggplot2::scale_x_continuous(name = "Radius [m]") +
    ggplot2::scale_y_continuous(name = "Metric value")
}
