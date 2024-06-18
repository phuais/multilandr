#' Generates optimized metrics gradient
#'
#' Selects a set of points whose associated landscapes comprise an optimized gradient for a given landscape metric.
#'
#' @param x An object of class 'MultiLandMetrics' generated with [mland_metrics()].
#' @param rasterlayer The raster layer to be considered. If
#' an extra raster layer must be specified, the string "ext" must precede the raster layer number
#' (e.g. "ext1", "ext2")
#' @param class The class to be considered, as a number or as a string with the name of the class.
#' @param radius The radius to be considered.
#' @param metric The metric to be considered. Metrics as extra calculations for extra raster layers must be
#' provided as "fun_" + the name of the function.
#' @param n The number of points that will comprise the gradient. See Details.
#' @param cutpoints A sequence of numbers that will serve as numeric approximations to select the
#' points that will comprise the gradient. See Details.
#' @param breaks A unique number with the number of breaks that will generate the cutpoints for the
#' specified metric values. Default is 10. See Details.
#' @param random Logical. If TRUE, random points will be selected.
#' @param output One of the following: "MLM" to return an updated version of the 'MultiLandMetrics' object provided in `x` (default),
#' "spatial" to return a 'SpatVector' with the points
#' of the selected landscapes, "data" to return a data.frame with the metric values information or "coords"
#' to return a data.frame with geographical information of the selected points.
#'
#' @details Selects a subset of landscapes that overall will
#' generate an optimized gradient of values for a given landscape metric of a specified raster layer,
#' class and radius. One can define a gradient as optimized if
#' its values fulfill to cover a good range of values between a minimum and a maximum value. The
#' final gradient will comprise the number of points specified in argument `n`. Note that
#' only one landscape metric can be specified at a time.
#'
#' The algorithm will select those points whose associated landscapes present values for the specified landscape metric that are
#' the most close to the specified `cutpoints`. Alternatively, the user can provide a number of
#' `breaks` from which the sequence of cutpoints will be generated. If both arguments are specified,
#' the function will consider the values inputted in `cutpoints`. If both arguments are NULL, the
#' algorithm will simply select `n` random points.
#'
#' @return
#' A 'MultiLandMetrics' if `output = "MLM"`, a 'SpatVector' if `output = "spatial"`,
#' a data.frame if `output = "data"` or a data.frame with geographical information of the points if `output = "coords"`.
#'
#' @seealso [metrics_filter()]
#'
#' @export
#'
#' @examples
#' # Generates an optimized gradient for the landscape metric "pland", for the class "Forest".
#' pland_gradient <- metrics_gradient(otf_metrics, rasterlayer = 1, class = "Forest",
#'                                    radius = 2000, metric = "pland", n = 15, breaks = 10)
#' # Note that, in this case, specifications for the rasterlayer and the radius are
#' # redundant, and could be simply ignored and left as default, asthe object otf_metrics
#' # only comprises a unique rasterlayer and radius.
#'
#' # By default, the output is an updated version of the object otf_metrics. In order to
#' # inspect the returned values, let's select only the dataframe containing the
#' # metric's values.
#' foo <- subset(pland_gradient@data, metric == "pland" & classname == "Forest",
#'               select = value)
#'
#' # Next, we output the range of values we have obtained, note there are 15 points, as
#' # previously specified in the function definition in the argument 'n'
#' round(sort(foo$value), digits = 2)
#'
#' # 1.15  1.57  8.17  8.19 15.24 22.32 29.27 36.32 43.17 43.20 49.79 50.25 55.44 57.62 64.53
#'
#' # Alternatively, we can define specific cutpoints around the landscapes will be selected
#' # in termsof its numeric closeness.
#' pland_gradient <- metrics_gradient(otf_metrics, rasterlayer = 1, class = "Forest",
#'                                    radius = 2000,metric = "pland", n = 15,
#'                                    cutpoints = seq(1, 60, 5))
#'
#' # Again, we inspect the dataframe with the metric values to see our results.
#' foo <- subset(pland_gradient@data, metric == "pland" & classname == "Forest",
#'               select = value)
#'
#' round(sort(foo$value), digits = 2)
#'
#' # 1.15  6.02  6.03 10.99 15.97 20.99 26.01 31.02 35.95 41.14 41.34 45.93 51.41 54.56 55.44
#'
#' # Both alternatives generated a wide-ranged gradient of values for the forest metric "pland"
metrics_gradient <- function(x,
                             rasterlayer = NULL,
                             class = NULL,
                             radius = NULL,
                             metric = NULL,
                             n,
                             cutpoints = NULL,
                             breaks = NULL,
                             random = FALSE,
                             output = "MLM"){

  if(!is(x, "MultiLandMetrics")) stop("x must be an object of class 'MultiLandMetrics'.")

  environment(.metrics_gradient_chk_args) <- environment()
  chk <- .metrics_gradient_chk_args()
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

  dat <- x@data[x@data$rasterlayer == rasterlayer &
                 x@data$class == class &
                 x@data$radius == radius &
                 x@data$metric == metric, ]

  if(random){
    pp <- sample(1:nrow(dat), n)
    message("As random = TRUE, arguments \"cutpoints\" or \"breaks\" were not considered. Random points were selected.")
  } else {
    if(is.null(cutpoints) & is.null(breaks)){
      breaks <- n
    }
    if(!is.null(cutpoints) & !is.null(breaks)){
      message("Argument \"breaks\" was discarded.")
      breaks <- length(cutpoints)
    }
    if(is.null(cutpoints) & !is.null(breaks)){
      cutpoints <- seq(min(dat$value), max(dat$value), length.out = breaks)
    }
    if(!is.null(cutpoints)){
      breaks <- length(cutpoints)
    }

    points_assign <- rep(0, breaks)
    # Pseudo-Random assignment of the points
    tandas <- floor(n/breaks)
    for(i in 1:tandas){ points_assign <- points_assign + 1 }
    rest <- n - sum(points_assign)
    if(rest > 0){
      foo <- sample(1:breaks, rest)
      points_assign[foo] <- points_assign[foo] + 1
    }

    pp <- vector("numeric")
    dat2 <- dat
    for(i in 1:breaks){
      for(j in 1:points_assign[i]){
        min_diff_id <- which.min(abs(cutpoints[i] - dat2$value))
        rownames(dat2[min_diff_id, ])
        pp <- c(pp, rownames(dat2[min_diff_id, ]))
        dat2 <- dat2[-min_diff_id, ]
      }
    }
  }

  # Output MultiLandMetrics, updated
  df_tmp <- x@data[x@data$point_id %in% dat[pp, ]$point_id, ]
  if(output == "MLM"){
    out <- x
    # Update object
    out@points <- out@points[out@points$id %in% unique(df_tmp$point_id), ]
    out@n_points <- length(unique(df_tmp$point_id))
    out@n_layers <- sum(!grepl("ext", unique(out@data$rasterlayer)))
    out@classes <- na.exclude(unique(out@data[, c("rasterlayer", "class", "classname")]))
    out@classes <- out@classes[order(out@classes$rasterlayer, out@classes$class, out@classes$classname), ]
    out@metrics <- na.exclude(unique(out@data[, c("level", "metric")]))
    # Updating ext_calcs
    foo <- unique(out@data[, c("rasterlayer", "layer_name", "metric")])
    foo <- foo[grepl("ext", foo$rasterlayer), ]
    foo$rasterlayer <- as.numeric(gsub("ext", "", foo$rasterlayer))
    foo$metric <- gsub("fun_", "", foo$metric)
    colnames(foo)[2] <- "name"
    if(nrow(foo) > 0) rownames(foo) <- 1:nrow(foo)
    out@ext_calcs <- foo
    out@data <- df_tmp
  } else {
    points_vec <- terra::vect(cbind(x = x@points$x, y = x@points$y),
                              atts = x@points[3:ncol(x@points)],
                              crs = terra::crs(x@crs_proj))
    # output vector
    if(output == "spatial"){
      out <- points_vec[unique(df_tmp$point_id), ]
    } else {
      # output points dataframe
      if(output == "coords" & nrow(df_tmp) > 0){
        out <- aggregate(point_id ~ site, df_tmp, unique)
      } else {
        # output metrics dataframe
        out <- df_tmp
      }

      # add points coordinates
      coordinates <- as.data.frame(terra::crds(points_vec))
      coordinates$point_id <- 1:length(points_vec)
      out <- merge(out, coordinates, "point_id", all.x = TRUE)

      # order columns
      if(output == "data"){
        out <- out[, c("rasterlayer", "point_id", "site", "x", "y",
                       "radius", "level", "class", "classname", "patch_id",
                       "metric", "value")]
      }
    }
  }

  invisible(out)
}

