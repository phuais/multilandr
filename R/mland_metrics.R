# Pre-processes intersection
.classes_subset <- function(x, classes){
  uni <- terra::unique(x)[, 1]
  if(!any(uni %in% classes)){
    x <- terra::rast(nrows = 1, ncols = 1, crs = terra::crs(x), vals = NA)
  }
  x
}

# Generates temporal intersections
.intersecting_tmp <- function(ra){

  if(ra == "ext_raster"){
    clip <- suppressWarnings(tryCatch(terra::crop(x@landscapes$ext_rasters[[lay]],
                                                  terra::ext(x@buffers[row_id, ])), error = c))
  } else {
    clip <- suppressWarnings(tryCatch(terra::crop(x@landscapes$lsm_rasters[[raster[r]]],
                                                  terra::ext(x@buffers[row_id, ])), error = c))
  }

  if(!is.list(clip)){
    out <- terra::mask(clip, x@buffers[row_id, ])
  } else {
    empty_raster <- terra::rast(nrows = 1, ncols = 1, crs = terra::crs(x@buffers[1, ]),
                                  vals = NA)
    out <- empty_raster
  }

  return(out)
}

# Adds missing rows
.add_missing_rows <- function(){
  for(m in 1:length(class_metrics)){
    for(i in points){
      tmp <- tab2_classlevel[tab2_classlevel$point_id == i, ]
      if(nrow(tmp) < total_rows_pp){
        for(j in radii){
          for(k in 1:n_classes){
            if(!j %in% tmp[tmp$class == classes_tmp[k] & tmp$metric == class_metrics[m], "radius"]){
              if(!is.null(x@site_ref)){
                site_id <- as.data.frame(x@points[i, x@site_ref])[1, 1]
              } else { site_id <- i }
              new_row <- data.frame(layer = df_reference[df_reference$point_id == i &
                                                           df_reference$radius == j, "layer"],
                                    level = "class",
                                    class = classes_tmp[k],
                                    id = NA,
                                    metric = class_metrics[m],
                                    value = absence_values[[class_metrics[m]]],
                                    point_id = i,
                                    site = site_id,
                                    radius = j)
              tab2_classlevel <- rbind(tab2_classlevel, new_row)
            }
          }
        }
      }
    }
  }
  return(tab2_classlevel)
}

# Extra raster calculations
ext_calc_func <- function(){

  f <- function(x, func_name){
    if(na.exclude){
      obj <- na.exclude(terra::values(x))
    } else {
      obj <- terra::values(x)
    }
    calc <- do.call(func_name, list(obj))
  }

  ext_calc_output <- function(){
    for(func in ext_funcs){
      tmp_tab <- tab
      vals <- unlist(lapply(tmp_landscape, FUN = f, func))
      tmp_tab$metric <- paste0("fun_", func)
      tmp_tab$value <- vals
      tmp_tab$rasterlayer <- paste0("ext", layers[j])
      if(nrow(x@rast_names[[2]] > 0)){
        foo <- x@rast_names[[2]][x@rast_names[[2]]$rasterlayer == layers[j], "name"]
        if(is.na(foo)){
          tmp_tab$layer_name <- NA
        } else {
          tmp_tab$layer_name <- foo
        }
      } else {
        tmp_tab$layer_name <- NA
      }
      tmp_tab$level <- "landscape"
      tmp_tab$id <- tmp_tab$class <- tmp_tab$classname <- NA
      tmp_tab <- tmp_tab[, -which(colnames(tmp_tab) == "row_id")]
      tab_list[[j]] <- rbind(tab_list[[j]], tmp_tab)
      pos <- pos + 1
    }

    return(tab_list)
  }

  layers <- as.numeric(unique(ext_calc_ref$rasterlayer))
  tab_list <- vector("list", length(layers))

  pos <- 1
  for(j in 1:length(layers)){
    ext_funcs <- unique(ext_calc_ref[ext_calc_ref$rasterlayer == layers[j], "fun"])
    if(!x@on_the_fly){
      tmp_landscape <- x@landscapes$ext_rasters[[layers[j]]][df_reference$row_id]
      tab <- df_reference
      environment(ext_calc_output) <- environment()
      tab_list <- ext_calc_output()
    } else {
      for(i in 1:nrow(df_reference)){
        row_id <- df_reference$row_id[i]
        environment(.intersecting_tmp) <- environment()
        tmp_landscape <- list(.intersecting_tmp("ext_raster"))
        tab <- df_reference[i, ]
        environment(ext_calc_output) <- environment()
        tab_list <- ext_calc_output()
      }
    }
  }

  return(tab_list)
}

lsm_output <- function(tab, on_the_fly, i){
  if(length(class(tab$value)) >= 2){
    if(any(class(tab$value)[1:2] %in% c("simpleError", "error"))){
      if(grepl("Selected metrics do not exist", tab$value)){
        cat("\n")
        stop(strwrap("Selected metrics do not exist. Mispelled? Use metrics_list() to see all
                  available metrics.", prefix = "\n", initial = ""), call. = FALSE)
      } else {
        cat("\n")
        message("landscapemetrics errors:")
        stop(tab$value)
      }
    } else {
      tab <- tab$value
      if(on_the_fly) tab$layer <- i
    }
  }
  return(tab)
}

# Update metrics data
mlmetrics_update <- function(df, update, rasters_classes_tab, metrics_list, ext_calc_ref,
                             points, radii){

  df <- rbind_and_replace(df, update@data, c("rasterlayer", "point_id", "radius",
                                             "level", "class", "metric"))
  rasters_classes_tab <- rbind_and_remove(rasters_classes_tab, update@classes,
                                          c("rasterlayer", "class", "classname"))
  rasters_classes_tab <- rasters_classes_tab[order(rasters_classes_tab$rasterlayer,
                                                   rasters_classes_tab$class), ]
  metrics_list <- rbind_and_remove(metrics_list, update@metrics, c("level", "metric"))
  ext_calc_ref <- rbind_and_remove(ext_calc_ref, update@ext_calcs,
                                   c("rasterlayer", "name", "fun"))
  points <- rbind_and_remove(points, update@points, c("x", "y", "id"))
  n_points <- nrow(points)
  radii <- sort(unique(c(radii, update@radii)))

  out <- list(df, rasters_classes_tab, metrics_list, ext_calc_ref, points, n_points, radii)
  return(out)
}

# Sort dataframe
sort_df <- function(df){
  if(any(grepl("ext", df$rasterlayer))){
    df$aux_rl <- rep(NA, nrow(df))
    exts <- grep("ext", df$rasterlayer)
    no_exts <- (1:length(df$rasterlayer))[-exts]
    df$aux_rl[no_exts] <- df$rasterlayer[no_exts]
    df$aux_rl[exts] <- as.numeric(gsub("ext", "", df$rasterlayer[exts])) + length(no_exts)
    df <- df[order(df$point_id, df$radius, df$aux_rl, df$level,
                   df$metric, df$class, df$patch_id), ]
    df <- df[, -ncol(df)]
  } else {
    df <- df[order(df$point_id, df$radius, df$rasterlayer, df$level,
                   df$metric, df$class, df$patch_id), ]
  }
  return(df)
}

# Final details for exported object
final_details <- function(df, x, rasters_classes_tab, ext_calc_ref){
  # Update rownames
  rownames(df) <- 1:nrow(df)

  # Names for used raster layers
  if(nrow(x@rast_names[[1]]) > 0){
    x@rast_names[[1]] <- x@rast_names[[1]][x@rast_names[[1]]$rasterlayer %in%
                                               unique(rasters_classes_tab$rasterlayer), ]
    colnames(x@rast_names[[1]])[2] <- "name"
  }
  if(nrow(x@rast_names[[2]]) > 0){
    x@rast_names[[2]] <- x@rast_names[[2]][x@rast_names[[2]]$rasterlayer %in%
                                               unique(ext_calc_ref$rasterlayer), ]
    colnames(x@rast_names[[2]])[2] <- "name"
  }

  if(nrow(rasters_classes_tab) > 0){
    n_classes <- aggregate(class ~ rasterlayer, rasters_classes_tab, length_unique)$class
  } else {
    n_classes <- numeric(0)
  }

  out <- list(df, x, n_classes)
  return(out)
}

#' Calculates landscape metrics
#'
#' Calculates landscape metrics of patch, class and/or landscape level via the package
#' [landscapemetrics] and user-defined functions from an object of class 'MultiLand'.
#'
#' @param x An object of class 'Multiland' generated with [mland()].
#' @param ... Other arguments passed to [landscapemetrics::calculate_lsm()]. See Details.
#' @param raster Vector depicting the raster layers of `x` from which metrics will be
#' calculated. It can be a numeric vector, for rasterlayer numbers, or a character vector, for
#' raster layer names (if provided during the generation of `x`). If NULL, all raster layers will be
#' considered.
#' @param points Numeric or character vector with the points from which metrics will be calculated.
#' If NULL, all points will be considered. See Details.
#' @param radii Numeric vector depicting the radii from which metrics will be calculated.
#' If NULL, all radii will be considered.
#' @param classes List containing the classes or classes names from which metrics will be calculated.
#' If NULL, all classes will be considered. See Details.
#' @param level,metric,name,type,what Arguments passed to [landscapemetrics::calculate_lsm()],
#' which define which metrics will be calculated. See Details.
#' @param report_absences Logical. If TRUE (default), intersections with absences of particular
#' classes will be returned in the final data.frame. See Details.
#' @param absence_values A list depicting which value for each class-level metric should be printed
#' if `report_absences = TRUE`. See Details.
#' @param ext_calc A list containing vectors, each one of length equal to 2 or more: the first element of the vector
#' with the identification number of the extra raster layer defined in `x`, and next elements with a string with
#' the name of the function to be applied to the defined raster. See Details.
#' @param na.exclude Logical. Whether to exclude (default) or not the NA values when performing
#' extra calculations to extra raster layers. Only applies if `ext_calc` is not NULL. See Details.
#' @param coords Logical. If TRUE, the coordinates of the points will be returned in the data.frame
#' containing the values of the required metrics. Default FALSE.
#' @param update An object of class 'MultiLandMetrics', if it is intended to be updated with
#' new or updated metrics data. See Details.
#' @param output Either "MLM" (default) to output an object of class 'MultiLandMetrics' or "data" to output
#' only the data.frame with metric values.
#' @param progress Logical. If TRUE (default), progress of calculations will be printed.
#'
#' @details Calculates landscape metrics from an object of class `MultiLand`
#' created with [mland()]. The function allows to define which metrics will be calculated in the
#' form defined by the function [calculate_lsm()] from package [landscapemetrics], by
#' specifying one or more of the following arguments:
#'
#' * level: level of metrics. Either "patch", "class" or "landscape" (or vector with combination).
#' * metric: abbreviation of metrics (e.g. "area").
#' * name: full name of metrics (e.g. "core area").
#' * type: type according to FRAGSTATS grouping (e.g. "aggregation metrics").
#' * what: selected level of metrics: either "patch", "class" or "landscape". It is also possible to
#' specify functions as a vector of strings, e.g. what = c("lsm_c_ca", "lsm_l_ta").
#'
#' Available metrics can be seen in [metrics_list()] and in the associated documentation of package
#' `landscapemetrics`.
#'
#' [mland_metrics()] also allows to define some other parameters that filter how metrics are calculated,
#' by defining the raster layers, points, radii and classes to be taken into account.
#'
#' If `report_absences = TRUE` (default), the function will print values of class-level metrics
#' from classes that are not present in particular landscapes, as a distinct row in the final
#' data.frame. This is particularly useful for certain class-level metrics in which the absence
#' of the class should be acknowledged, for instance, the percentage of landscape ('pland') for a forest class. For this
#' metric, a value of 0 (zero) should be printed for those landscapes where the class forest is not present.
#' By default, if `report_absences = TRUE`, the function will consider `NA` as the value
#' to be declared in the case that the class is absent in the landscape. To declare a different value
#' for a particular class-level metric, this can be declared inside argument `absence_values`. If
#' not NULL, this must be a list with the value that one ore more class-level metric should
#' have in the case of an absence of a class. For example, in the case of "pland", the argument
#' should be defined as follows: `absence_values = list("pland" = 0)`. Note that the metric must be
#' identified with its abbreviation. You can see abbreviations for all available metrics in [metrics_list()],
#' under the column "metric".
#'
#' If argument `points` is a character vector,
#' [mland_metrics()] assumes that the 'MultiLand' object inputted in argument `x` was created with
#' `site_ref = TRUE`. This is, there is an column/attribute in the points layer with the names for
#' each distinct point. Therefore, the inputted values in argument `points` will be taken as these
#' identification names. Otherwise, if a numeric vector is inputted, these values
#' will be taken as the automatically generated point ids (created when running [mland()]).
#'
#' The user may specify which classes will be considered when calculating the metrics, by
#' passing this information in the argument `classes`. Of course, this information only applies for
#' class-level metrics. The argument must be a list with as many elements as raster layers to be
#' considered (defined in argument `raster`, in ascending order: 1, 2, 3, ...). Each element must be a numeric vector
#' with the classes values (raster values) to be considered, or a character vector with
#' the names of the classes (if provided when generating `x`).
#'
#' Other arguments can be passed to function [landscapemetrics::calculate_lsm()] through argument
#' `...`. These include specific arguments relative to the calculation of particular landscape
#' metrics. See the documentation of this function for more information.
#'
#' Extra calculations can be performed through `ext_calc`. The functions defined here will take
#' the values of the extra raster layers defined in `x` as input. For instance, a plausible definition
#' could be `ext_calc = list(1, "mean")`, which will take the values from the extra raster layer 1,
#' and calculate its mean for each landscape. If `na.exclude = TRUE` (default), NA values will be excluded
#' from this task.
#'
#' A previously generated 'MultiLandMetrics' object can be updated with new information regarding
#' other metrics, probably from other points, radii, raster layers, etc, that haven´t been
#' calculated in the previous time (or not). In this way, the returned object will be
#' the object provided in this argument, plus the additions of information about new metrics, and
#' changes to previously metric calculations. Note that if a particular metric is calculated for a
#' given raster layer, points, radii and or class, that were previously generated in the object
#' provided in `update`, the information of these metrics from the latter will be overwritten. Also
#' note that if in the previous 'MultiLandMetrics' object `report_absences` was `TRUE` for a given
#' set of metrics and other parameters (e.g. points, radii, raster layers, etc.), and in the new call
#' `report_absences` is `FALSE` (for the same set of other parameters), the rows depicting landscapes
#' with empty classes from the previous call will be mantained. If the intention is the removal of these rows,
#' the user should create a fresh new 'MultiLandMetrics' from scratch.
#'
#' @return If `output = "MLM"`, an object of class 'MultiLandMetrics' will be returned. This object
#' can then be passed to functions [metrics_corr()], [metrics_plots], [metrics_filter()], [metrics_gradient()]
#' and [metrics_bind()]. See
#' ?MultiLandMetrics for more information regarding the content of this object. Otherwise,
#' if `output = "data"`, only a data.frame with the calculated metrics will be returned.
#'
#' @seealso [metrics_corr()], [metrics_plots()], [metrics_filter()], [metrics_gradient()], [metrics_bind()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Loads a 'MultiLand' object
#' ernesdesign <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
#' ernesdesign <- mland_load(ernesdesign)
#'
#' # Creates a 'MultiLandMetrics' object. It will calculate the "percentage of landscape"
#' # ("pland") and "edge density" ("ed") for all classes. Note that an absence value
#' # for each metric is declared, as the absence of a class for these metrics should be
#' # acknowledged as a 0 (percentage of zero and zero patches).
#' ed_metrics <- mland_metrics(ernesdesign, level = "class", metric = c("pland", "ed"),
#'                             absence_values = list("pland" = 0))
#'
#' # Returns data.frame with the values of all metrics for each landscape
#' head(ed_metrics@data)
#'
#' # Shows which metrics were calculated and are contained in the data.frame
#' ed_metrics@metrics
#'
#' # If output = "data", only the data.frame will be returned
#' data <- mland_metrics(ernesdesign, level = "class", metric = "pland",
#'                       classes = c("Forest", "Crops"),
#'                       absence_values = list("pland" = 0),
#'                       output = "data")
#'
#' # Calculate landscape metrics plus extra calculations for extra rasterlayer 1,
#' # the mean value, and a user defined function, which is the mean divided
#' # standard deviation.
#'
#' # User-defined function
#' mean_sd <- function(x){ mean(x)/sd(x) }
#'
#' ed_metrics2 <- mland_metrics(ernesdesign, level = "class",
#'                              metric = c("pland", "ed"),
#'                              absence_values = list("pland" = 0),
#'                              ext_calc = list(c(1, "mean"), c(1, "mean_sd")))
#'
#' # We can calculate metrics for extra raster layers only
#' ed_metrics3 <- mland_metrics(ernesdesign, ext_calc = list(c(1, "mean", "mean_sd")))
#'
#' # If metrics of different levels must be calculated, a better approach is to declare
#' # them inside the argument 'what', by naming the function associated with the metric.
#' # Also in this case, only the landscapes with a radius of 5000 m are considered.
#' # A list of available metrics with its names, abbreviations and function names can
#' # be seen in metrics_list() and in the documentation of the package landscapemetrics.
#' ed_metrics4 <- mland_metrics(ernesdesign,
#'                              what = c("lsm_c_area_mn", "lsm_l_ed", "lsm_l_shdi"),
#'                              radii = 5000)
#'
#' # Calculates patch-level metrics of a particular landscape
#' ed_patchs <- mland_metrics(ernesdesign, points = "Algarrobo",
#'                            level = "patch", class = "Forest",
#'                            radii = 1000)
#' }
mland_metrics <- function(x,
                          raster = NULL,
                          points = NULL,
                          radii = NULL,
                          classes = NULL,
                          level = NULL,
                          metric = NULL,
                          name = NULL,
                          type = NULL,
                          what = NULL,
                          report_absences = TRUE,
                          absence_values = NULL,
                          ext_calc = NULL,
                          na.exclude = TRUE,
                          coords = FALSE,
                          update = NULL,
                          output = "MLM",
                          progress = TRUE,
                          ...){

  # Check arguments
  if(!is(x, "MultiLand")) stop("- argument 'x' must be an object of class 'MultiLand'.")
  environment(.mland_metrics_chk_args) <- environment()
  chk <- .mland_metrics_chk_args()
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

  df_reference <- x@l_ref

  # if points and/or radii are null, take all of points and radii defined in x
  points <- if(is.null(points)) 1:length(x@points) else as.numeric(points)
  if(is.null(radii)) radii <- x@radii

  # subset df_reference
  df_reference <- df_reference[df_reference$point_id %in% points &
                                 df_reference$radius %in% radii, ]
  df_reference$layer <- 1:nrow(df_reference)

  # final number of radii and points to be evaluated
  n_radii  <- length(radii)
  n_points <- length(points)

  # number of raster layers to be evaluated
  n_layers <- length(raster)

  # metrics calculation
  metrics_list <- data.frame()
  df <- data.frame()
  if(!is.null(raster)){
    if(any(unlist(lapply(list(level, metric, name, type, what),
                         function(x) !is.null(x))))){
      tab_list <- vector("list", n_layers)
      warn_lm <- 0
      for(r in 1:n_layers){
        if(progress) message(paste0("rasterlayer ", raster[r], "..."))
        if(!x@on_the_fly){
          if(progress) message("   Calculating metrics via landscapemetrics")
          tmp_landscape <- x@landscapes$lsm_rasters[[raster[r]]][df_reference$row_id]
          tmp_landscape <- lapply(tmp_landscape, .classes_subset, classes = classes[[r]])
          tab <- tryCatch.W.E(landscapemetrics::calculate_lsm(landscape = tmp_landscape,
                                                              level = level, metric = metric, name = name,
                                                              type = type, what = what, progress = progress, ...))
          tab <- tryCatch(lsm_output(tab, x@on_the_fly, 0),
                          error = function(e){
                            message("")
                            stop(e)})
        } else {
          if(progress){
            message("   Generating intersections and calculating metrics via landscapemetrics")
          }
          tab <- data.frame()
          for(i in 1:nrow(df_reference)){
            row_id <- df_reference$row_id[i]
            environment(.intersecting_tmp) <- environment()
            tmp_landscape <- .intersecting_tmp("rast")
            tmp_landscape <- .classes_subset(tmp_landscape, classes[[r]])
            tmp_tab <- tryCatch.W.E(landscapemetrics::calculate_lsm(landscape = tmp_landscape,
                                                                    level = level, metric = metric,
                                                                    name = name, type = type, what = what,
                                                                    progress = F, ...))
            tmp_tab <- tryCatch(lsm_output(tmp_tab, x@on_the_fly, i),
                                error = function(e){
                                  message("")
                                  stop(e)})
            tab <- rbind(tab, tmp_tab)
            if(progress){
              perc <- round(100*i/nrow(df_reference))
              cat("\r> Progress: ", i, " / ", nrow(df_reference), " (", perc, "%)", sep = "")
            }
          }
        }

        # addition of point, site reference and radius information
        tab2 <- merge(tab, df_reference, "layer", all.x = TRUE)
        tab2 <- tab2[, -which(names(tab2) %in% "row_id")]

        # calculated metrics
        metrics_list <- unique(tab2[c("level", "metric")])
        rownames(metrics_list) <- 1:nrow(metrics_list)
        # calculated class-level metrics
        class_metrics <- metrics_list[metrics_list$level == "class", "metric"]
        # subset by required classes
        if(!all(x@classes[x@classes$rasterlayer == raster[r], "class"] %in%
                classes[[r]])){
          tmp_class <- tab2[tab2$level == "class", ]
          tmp_class <- tmp_class[tmp_class$class %in% classes[[r]], ]
          if(nrow(tmp_class) > 0) tab2 <- tab2[as.numeric(rownames(tmp_class)), ]
          if("landscape" %in% tab2$level & warn_lm == 0){
            warning(strwrap("- Landscape-level metrics were calculated considering all available
        classes, even though a subset of classes was required.", prefix = "\n", initial = ""),
                    call. = FALSE)
            if(warn_lm == 0) warn_lm <- 1
          }
        }

        if(report_absences & "class" %in% tab2$level){
          # absence_values argument checking/construction
          if(is.null(absence_values)){
            absence_values <- vector("list", length(class_metrics))
            absence_values[1:length(class_metrics)] <- NA
            names(absence_values) <- class_metrics
          } else {
            abs_eval <- T
            if(is.null(names(absence_values))){
              abs_eval <- F
            } else {
              if(!all(names(absence_values) %in% class_metrics)) abs_eval <- F
            }

            if(!abs_eval){
              absence_values <- vector("list", length(class_metrics))
              absence_values[1:length(class_metrics)] <- NA
              names(absence_values) <- class_metrics
              warning(strwrap("- absence_values was not defined properly. The argument was
            ignored and taken as NULL. See details in ?mland_metrics",
                              prefix = "\n", initial = ""))
            }

            if(length(absence_values) < length(class_metrics)){
              for(i in 1:length(class_metrics)){
                if(!class_metrics[i] %in% names(absence_values)){
                  absence_values <- append(absence_values, list(new = NA))
                  names(absence_values)[length(absence_values)] <- class_metrics[i]
                }
              }
            }
          }

          # number of classes of rasterlayer r
          n_classes <- length(classes[[r]])
          # classes of rasterlayer r
          classes_tmp <- classes[[r]]
          # expected number of rows for class metrics
          total_rows <- n_points * n_radii * n_classes * length(class_metrics)
          # expected number of rows for each point
          total_rows_pp <- n_radii * n_classes * length(class_metrics)
          # subset dataframe by metrics of class level
          tab2_classlevel <- tab2[tab2$level == "class", ]
          # remove rows with NA values in the class column
          tab2_classlevel <- tab2_classlevel[!is.na(tab2_classlevel$class), ]

          # add missing rows of class level metrics, if necessary
          if(nrow(tab2_classlevel) < total_rows){
            environment(.add_missing_rows) <- environment()
            tab2_classlevel <- .add_missing_rows()
          }

          # add new class level metric dataframe to the previus dataset
          tab2 <- rbind(tab2_classlevel, tab2[tab2$level != "class", ])
        }

        tab2$rasterlayer <- raster[r]

        # addition of class names
        cl <- x@classes[x@classes$rasterlayer == raster[r], 2:3]
        cl <- rbind(cl, data.frame(class = NA, classname = NA))
        tab2 <- merge(tab2, cl, "class")

        # addition of raster layers names
        if(nrow(x@rast_names[[1]] > 0)){
          colnames(x@rast_names[[1]])[2] <- "layer_name"
          tab2 <- merge(tab2, x@rast_names[[1]], "rasterlayer")
        } else {
          tab2$layer_name <- NA
        }

        tab_list[[r]] <- tab2
      }
      # rbind dataframes from different raster layers
      df <- do.call("rbind", tab_list)
    }
  }

  # Perform calculations of extra rasters
  if(!is.null(ext_calc)){
    if(progress) message("Performing calculations for extra raster layers")
    environment(ext_calc_func) <- environment()
    ext_tab_list <- ext_calc_func()
    ext_df <- do.call("rbind", ext_tab_list)

    # rbind with general data frame
    df <- rbind(df, ext_df)
  } else { ext_calc <- vector("list") }

  # filter, order and rename columns
  df <- df[, c("rasterlayer", "layer_name", "point_id", "site", "radius", "level", "class", "classname",
               "id", "metric", "value")]
  colnames(df)[9] <- "patch_id"

  # Add points coordinates for exported object
  points <- cbind(terra::crds(x@points), terra::values(x@points))

  # If a MultiLandMetrics object is provided, replace and add new rows if necessary
  if(!is.null(update)){
    updated_df <- mlmetrics_update(df, update, rasters_classes_tab, metrics_list, ext_calc_ref,
                                   points, radii)
    df <- updated_df[[1]]
    rasters_classes_tab <- updated_df[[2]]
    metrics_list <- updated_df[[3]]
    ext_calc_ref <- updated_df[[4]]
    points <- updated_df[[5]]
    n_points <- updated_df[[6]]
    radii <- updated_df[[7]]
  }

  # Sort database
  df <- sort_df(df)

  # Add coordinates to main metrics table, if required
  if(coords){
    coordinates <- points
    colnames(coordinates)[3] <- "point_id"
    df <- merge(df, coordinates, "point_id", all.x = TRUE)
    df <- df[, c("rasterlayer", "layer_name", "point_id", "site", "x", "y", "radius", "level",
                 "class", "classname", "patch_id", "metric", "value")]
  }

  final_dets <- final_details(df, x, rasters_classes_tab, ext_calc_ref)
  df <- final_dets[[1]]
  x  <- final_dets[[2]]

  if(output == "MLM"){
    out <- new("MultiLandMetrics",
               call        = match.call(),
               idkey       = x@idkey,
               crs_proj    = terra::crs(x@points),
               n_layers    = length(unique(rasters_classes_tab$rasterlayer)),
               rast_names  = x@rast_names,
               classes     = rasters_classes_tab,
               n_classes   = final_dets[[3]],
               points      = points,
               n_points    = n_points,
               site_names  = if(is.null(x@site_ref)) F else T,
               radii       = radii,
               metrics     = metrics_list,
               ext_calcs   = ext_calc_ref,
               data        = df)
  } else {
    out <- df
  }

  return(out)
}
