#' Filters landscapes
#'
#' Selects landscapes that meet certain pre-defined conditions in relation to its metrics from
#' a 'MultiLandMetrics' object.
#'
#' @param x An object of class 'MultiLandMetrics' generated with [metrics()].
#' @param conditions List. Conditions to be met by the landscapes. See Details.
#' @param output One of the following: "MLM" to return an updated version of the 'MultiLandMetrics' object provided in `x` (default),
#' "spatial" to return a 'SpatVector' with the points
#' of the selected landscapes, "data" to return a data.frame with the metric values information or "coords"
#' to return a data.frame with geographical information of the filtered points.
#'
#' @details This function allows to select landscapes that meet certain conditions in relation to the values of
#' their landscape metrics. The function will retain those points associated with the landscapes
#' that meet all the defined conditions at the same time. Conditions must be provided through a list,
#' each element an inside list, as follows:
#'
#' \preformatted{
#'  list(list(rasterlayers, class, radii, metric, minimum value, maximum value),
#'       list(rasterlayers, class, radii, metric, minimum value, maximum value),
#'            ...)
#' }
#'
#' * rasterlayers: the rasterlayers to be considered. If NA, all rasterlayers will be considered. If
#' an extra rasterlayer must be specified, the string "ext" must precede the rasterlayer number
#' (e.g. "ext1", "ext2").
#' * class: the classes to be considered, as numbers or strings with the names of the classes. If NA, all classes of required rasterlayers will be
#' considered. If NULL, the function will assume that the metric to be considered is a landscape-level metric.
#' Take into account that metrics from extra calculations are considered as landscape-level metrics.
#' * radii: the radii to be considered. If NA, all radii will be considered.
#' * metrics: the name of the metric to be considered (as defined with its abbreviation by column "metric" in [metrics_list()]).
#' Only one metric per condition can be defined. Metrics as extra calculations for extra rasterlayers must be
#' provided as "fun_" + the name of the function (e.g. "fun_mean").
#' * minimum value: the minimum value that the metric must have in the filtered landscapes. If equal
#' to -Inf, and a maximum value is defined, landscapes whose values in the defined metric are equal
#' or lower to the maximum value will be retained.
#' * maximum value: the maximum value that the metric must have in the filtered landscapes. If equal
#' to Inf, and a minimum value is defined, landscapes whose values in the defined metric are equal
#' or higher to the minimum value will be retained.
#'
#' A plausible list of conditions could be the following:
#'
#' \preformatted{
#'  list(list(1, 2, 1000, "pland", 20, 30),
#'       list(1, 4, 1000, "np", 1, 15),
#'       list("ext1", NULL, 1000, "fun_mean", 70, 80))
#' }
#'
#' And it would indicate that landscapes of radius equal to 1000 m should present values of "pland"
#' (percentage of the landscape) for class 2 from rasterlayer 1, between 20 and 30%. At the same time, landscapes of radius
#' equal to 1000 m should present values of "np" (number of patches) for class 4 from rasterlayer 1,
#' between 1 and 15 patches. Finally, all selected landscapes of radius equal to 1000 m should
#' present values for "fun_mean" (applied to extra rasterlayer "ext1") between 70 and 80. Note that the
#' slot for "class" is NULL, as extra rasterlayers do not hold classes.
#'
#' @return
#' A 'MultiLandMetrics' if `output = "MLM"`, a 'SpatVector' if `output = "spatial"`,
#' a data.frame if `output = "data"` or a data.frame with geographical information of the points if `output = "coords"`.
#'
#' @seealso [optimize_gradient()]
#'
#' @export
#'
#' @examples
#' # Filter landscapes that have between 20 and 30% of forest at a radius of 2000 m
#' # and output the data.frame with metrics values
#' otf_subset <- landscape_filter(otf_metrics,
#'                                conditions = list(list(NA, "Forest", 2000, "pland", 20, 30)),
#'                                output = "data")
#'
#' # The same but returning a data.frame with information of the retained points
#' otf_subset_points <- landscape_filter(otf_metrics,
#'                                       conditions = list(list(NA, "Forest", 2000, "pland", 20, 30)),
#'                                       output = "coords")
#'
#' # Filter landscapes that have between 20 and 30% of forest at a radius of 2000 m
#' # and a maximum of 60$ of Crops.
#' otf_subset2 <- landscape_filter(otf_metrics,
#'                                conditions = list(list(NA, "Forest", 2000, "pland", 20, 30),
#'                                                  list(NA, "Crops", 2000, "pland", -Inf, 60)),
#'                                output = "data")
landscape_filter <- function(x, conditions = list(rasterlayers = NULL,
                                                  classes = NULL,
                                                  radii = NULL,
                                                  metric = NULL,
                                                  min_value = NULL,
                                                  max_value = NULL), output = "MLM"){

  if(!is(x, "MultiLandMetrics")) stop("x must be an object of class 'MultiLandMetrics'.")

  environment(.landscape_filter_chk_args) <- environment()
  chk <- .landscape_filter_chk_args()
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

  df_tmp <- x@data
  # subset data.frame by each condition
  sel_land <- vector("numeric")
  proh_land <- vector("numeric")
  for(i in 1:length(conditions)){
    if(is.null(conditions[[i]][[2]])){
      level <- "landscape"
      conditions[[i]][[2]] <- NA
    } else { level <- "class"}

    df_check <- df_tmp[df_tmp$rasterlayer %in% conditions[[i]][[1]] &
                         df_tmp$level %in% level &
                         df_tmp$class %in% conditions[[i]][[2]] &
                         df_tmp$radius %in% conditions[[i]][[3]] &
                         df_tmp$metric %in% conditions[[i]][[4]], ]
    if(nrow(df_check) == 0){
      stop(strwrap(paste0("Inconsistencies were found in the definition of condition ", i, ".
      Are you sure that the condition was correctly defined? See ?landscape_filter."),
                   prefix = "\n", initial = "\n"))
    }
    for(r in 1:nrow(df_tmp)){
      if(!is.na(df_tmp[r, "value"])){
        if(df_tmp[r, "rasterlayer"] %in% conditions[[i]][[1]] &
           df_tmp[r, "level"] %in% level &
           df_tmp[r, "class"] %in% conditions[[i]][[2]] &
           df_tmp[r, "radius"] %in% conditions[[i]][[3]] &
           df_tmp[r, "metric"] %in% conditions[[i]][[4]]){
          if(df_tmp[r, "value"] >= as.numeric(conditions[[i]][[5]]) &
             df_tmp[r, "value"] <= as.numeric(conditions[[i]][[6]])){
            if(!df_tmp[r, "point_id"] %in% proh_land){
              if(!df_tmp[r, "point_id"] %in% sel_land){
                sel_land <- append(sel_land, df_tmp[r, "point_id"])
              }
            }
          } else {
            if(df_tmp[r, "point_id"] %in% sel_land){
              sel_land <- sel_land[sel_land != df_tmp[r, "point_id"]]
            }
            if(!df_tmp[r, "point_id"] %in% proh_land){
              proh_land <- append(proh_land, df_tmp[r, "point_id"])
            }
          }
        }
      }
    }
  }
  df_tmp <- df_tmp[df_tmp$point_id %in% sel_land, ]

  if(nrow(df_tmp) > 0){
    filtered_points <- length(unique(df_tmp$point_id))

    # output MultiLandMetrics, updated
    if(output == "MLM"){
      out <- x
      # Update object
      out@points <- out@points[out@points$id %in% unique(df_tmp$point_id), ]
      out@n_points <- length(unique(df_tmp$point_id))
      out@n_layers <- sum(!grepl("ext", unique(out@data$rasterlayer)))
      out@classes <- na.exclude(unique(out@data[, c("rasterlayer", "class", "classname")]))
      out@classes <- out@classes[order(out@classes$rasterlayer, out@classes$class, out@classes$classname), ]
      out@metrics <- na.exclude(unique(out@data[, c("level", "metric")]))
      out@metrics <- out@metrics[!grepl("fun", out@metrics$metric), ]
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
      # output shapefile
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

    cat(filtered_points, " points were retained out of ", x@n_points, " initial points (",
        round(100*filtered_points/x@n_points, digits = 2), "%).\n", sep = "")
  } else {
    stop("- no landscapes were found that meet the required conditions.\n")
  }

  invisible(out)
}
