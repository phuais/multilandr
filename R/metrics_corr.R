# subsetting information extracted from user inputs
.subset_fn <- function(pattern){
  minus_pos <- grep("-", pattern)
  if(length(minus_pos) > 0){
    if(length(minus_pos) == length(pattern)){
      action <- "drop"
      subset <- gsub("-", "", pattern)
    } else {
      action <- "error"
      subset <- NA
    }
  } else {
    action <- "stay"
    subset <- pattern
  }

  out <- list(action, subset)
  return(out)
}

.pair_subsets <- function(){

  df <- x@data

  # drop patch-level metrics
  if("patch" %in% x@metrics$level){
    df <- df[df$level != "patch", ]
    message("Patch-level metrics were dropped.")
  }

  # subset raster layers
  if(!is.null(raster)){
    if(is.character(raster)){
      if(!all(raster %in% x@rast_names[[1]]$name)){
        stop(strwrap(
          paste0("- in raster: one or more raster layers could not be found in x. Mispelled?"),
          prefix = "\n", initial = "\n"), call. = FALSE)
      } else {
        raster <- sort(x@rast_names[[1]][x@rast_names[[1]]$name %in% raster, "rasterlayer"])
      }
    }
    subset_r <- .subset_fn(raster)
    if(subset_r[[1]] == "error"){
      stop(strwrap("Inconsistencies were found in the definition of raster layers.
           This should be an all positive or an all negative numeric vector.", prefix = "\n",
                   initial = "\n"), call. = FALSE)
    } else {
      if(!all(subset_r[[2]] %in% 1:x@n_layers)){
        stop(strwrap("Inconsistencies were found between the required raster layers and the raster
                   layers associated with 'x'. Mispelled?", prefix = "\n", initial = "\n"), call. = FALSE)
      }
    }
  } else {
    subset_r <- NULL
    if(nrow(x@classes) > 0){
      raster <- unique(x@classes$rasterlayer)
    }
  }

  # subset extra rasters
  if(!is.null(ext_raster)){
    if(is.character(ext_raster)){
      if(!all(ext_raster %in% x@rast_names[[2]]$name)){
        stop(strwrap(
          paste0("- in ext_raster: one or more raster layers could not be found in x. Mispelled?"),
          prefix = "\n", initial = "\n"), call. = FALSE)
      } else {
        ext_raster <- sort(x@rast_names[[2]][x@rast_names[[2]]$name %in% ext_raster, "rasterlayer"])
      }
    }
    subset_er <- .subset_fn(ext_raster)
    if(subset_er[[1]] == "error"){
      stop(strwrap("Inconsistencies were found in the definition of 'ext_raster'.
           This should be an all positive or an all negative numeric or character vector.", prefix = "\n",
                   initial = "\n"), call. = FALSE)
    } else {
      if(!all(subset_er[[2]] %in% unique(as.numeric(x@ext_calcs$layer)))){
        stop(strwrap("Inconsistencies were found between the required extra raster layers and the
                   extra layers associated with 'x'. Mispelled?", prefix = "\n", initial = "\n"),
             call. = FALSE)
      }
      subset_er[[2]] <- paste0("ext", subset_er[[2]])
    }
  } else {
    subset_er <- NULL
  }

  fix_conflicts <- function(subset_1, subset_2){
    subset <- vector("list", 4)
    subset[[4]] <- F
    if(any(!is.null(subset_1), !is.null(subset_2))){
      if(all(!is.null(subset_1), !is.null(subset_2))){
        if(subset_1[[1]] != subset_2[[1]]){
          subset[[1]] <- "stay"
          subset[[4]] <- T
          ll <- list(subset_1, subset_2)
          for(i in 1:2){
            if(ll[[i]][[1]] == "stay"){
              subset[[2]] <- ll[[i]][[2]]
              if(i == 1){
                subset[[3]] <- c(rep("class", length(subset[[2]])))
              } else {
                subset[[3]] <- c(rep("landscape", length(subset[[2]])))
              }
              break
            }
          }
        } else {
          subset[[1]] <- subset_1[[1]]
          subset[[2]] <- c(subset_1[[2]], subset_2[[2]])
          subset[[3]] <- c(rep("class", length(subset_1[[2]])),
                           rep("landscape", length(subset_2[[2]])))
        }
      } else {
        ll <- list(subset_1, subset_2)
        for(i in 1:2){
          if(!is.null(ll[[i]])){
            subset[[1]] <- ll[[i]][[1]]
            subset[[2]] <- ll[[i]][[2]]
            if(i == 1){
              subset[[3]] <- c(rep("class", length(subset[[2]])))
            } else {
              subset[[3]] <- c(rep("landscape", length(subset[[2]])))
            }
            break
          }
        }
      }
    }
    subset
  }

  subset <- fix_conflicts(subset_r, subset_er)
  if(!is.null(subset[[1]])){
    if(subset[[1]] == "stay"){
      df <- df[df$rasterlayer %in% subset[[2]], ]
      if(subset[[4]]){
        message(paste0("The following raster layers were included in the analysis: ",
                       paste(subset[[2]], collapse = " ")))
      }
    } else {
      df <- df[!df$rasterlayer %in% subset[[2]], ]
      if(subset[[4]]){
        message(paste0("The following raster layers were excluded from the analysis: ",
                       paste(subset[[2]], collapse = " ")))
      }
    }
  }

  # subset radii
  if(!is.null(radii)){
    subset <- .subset_fn(radii)
    if(subset[[1]] == "error"){
      stop(strwrap("Inconsistencies were found in the definition of radii.
           This should be an all positive or an all negative numeric vector.", prefix = "\n",
                   initial = "\n"), call. = FALSE)
    } else {
      if(!all(subset[[2]] %in% x@radii)){
        stop(strwrap("Inconsistencies were found between the required radii and the radii associated
                   with 'x'. Mispelled?", prefix = "\n", initial = "\n"), call. = FALSE)
      }
      if(subset[[1]] == "stay"){
        df <- df[df$radius %in% subset[[2]], ]
        radii <- sort(unique(subset[[2]]))
      } else {
        df <- df[!df$radius %in% subset[[2]], ]
        radii <- sort(unique(df[!df$radius %in% subset[[2]], "radius"]))
      }
    }
  } else {
    radii <- x@radii
  }

  # function to label metrics based on the raster, the level and the class
  m_label <- function(df){
    df$metric_label <- rep("", nrow(df))
    for(i in 1:nrow(df)){
      if(grepl("ext", df$rasterlayer[i])){
        df$metric_label[i] <- paste0("r", df$rasterlayer[i], "_", df$metric[i], "_", df$radius[i])
      } else {
        if(df$level[i] == "class"){
          if(class_names){
            if(all(is.na(x@classes$classname))){
              cl_ref <- "class"
              level <- "c"
              message("No class names definition was found in 'x'. Argument 'class_names' was taken as FALSE.")
            } else {
              cl_ref <- "classname"
              level <- ""
            }
          } else {
            cl_ref <- "class"
            level <- "c"
          }

          df$metric_label[i] <- paste0("r", df$rasterlayer[i], "_", level, df[i, cl_ref], "_",
                                       df$metric[i], "_", df$radius[i])
        }
        if(df$level[i] == "landscape"){
          df$metric_label[i] <- paste0("r", df$rasterlayer[i], "_", df$metric[i], "_",
                                       df$radius[i])
        }
      }
    }
    return(df)
  }

  # subset class-level metrics
  df_c_level <- df[df$level == "class", ]
  if(!is.null(c_level)){
    subset_c <- .subset_fn(c_level)
    if(subset_c[[1]] == "error"){
      stop(strwrap("Inconsistencies were found in the definition of argument 'c_level'
      This argument should be an all positive or an all negative character vector.",
                   prefix = "\n", initial = "\n"), call. = FALSE)
    } else {
      av_mets <- x@metrics[x@metrics$level == "class", "metric"]
      if(!all(subset_c[[2]] %in% av_mets)){
        stop(strwrap("Inconsistencies were found between the required metrics
        and the class-level metrics included in 'x'. Mispelled?", prefix = "\n",
                     initial = "\n"), call. = FALSE)
      }
    }
  } else { subset_c <- NULL }
  if(nrow(df_c_level) > 0) df_c_level <- m_label(df_c_level)

  # subset landscape-level metrics
  df_l_level <- df[df$level == "landscape", ]
  if(!is.null(l_level)){
    subset_l <- .subset_fn(l_level)
    if(subset_l[[1]] == "error"){
      stop(strwrap("Inconsistencies were found in the definition of argument 'l_level'
      This argument should be an all positive or an all negative character vector.",
                   prefix = "\n", initial = "\n"), call. = FALSE)
    } else {
      av_mets <- c(x@metrics[x@metrics$level == "landscape", "metric"],
                   paste0("fun_", x@ext_calcs$fun))
      if(!all(subset_l[[2]] %in% av_mets)){
        stop(strwrap("Inconsistencies were found between the required landscape-level metrics
        and the metrics included in 'x'. Mispelled?", prefix = "\n",
                     initial = "\n"), call. = FALSE)
      }
    }
  } else { subset_l <- NULL }
  if(nrow(df_l_level) > 0) df_l_level <- m_label(df_l_level)

  subset <- fix_conflicts(subset_c, subset_l)
  if(!is.null(subset[[1]])){
    if(subset[[1]] == "stay"){
      if("class" %in% subset[[3]]){
        df_c_level <- df_c_level[df_c_level$metric %in%
                                   subset[[2]][which(subset[[3]] == "class")], ]
        if(subset[[4]]){
          message(paste0("The following class-level metrics were included in the analysis: ",
                         paste(subset[[2]][which(subset[[3]] == "class")], collapse = " ")))
        }
      } else { df_c_level <- data.frame() }
      if("landscape" %in% subset[[3]]){
        df_l_level <- df_l_level[df_l_level$metric %in%
                                   subset[[2]][which(subset[[3]] == "landscape")], ]
        if(subset[[4]]){
          message(paste0("The following landscape-level metrics were included in the analysis: ",
                         paste(subset[[2]][which(subset[[3]] == "landscape")], collapse = " ")))
        }
      } else { df_l_level <- data.frame() }
    } else {
      if("class" %in% subset[[3]]){
        df_c_level <- df_c_level[!df_c_level$metric %in%
                                   subset[[2]][which(subset[[3]] == "class")], ]
        if(subset[[4]]){
          message(paste0("The following class-level metrics were excluded from the analysis: ",
                         paste(subset[[2]][which(subset[[3]] == "class")], collapse = " ")))
        }
      }
      if("landscape" %in% subset[[3]]){
        df_l_level <- df_l_level[!df_l_level$metric %in%
                                   subset[[2]][which(subset[[3]] == "landscape")], ]
        if(subset[[4]]){
          message(paste0("The following landscape-level metrics were excluded from the analysis: ",
                         paste(subset[[2]][which(subset[[3]] == "landscape")], collapse = " ")))
        }
      }
    }
  }

  # subset classes for each rasterlayer
  if(!is.null(classes) & !is.null(raster)){
    if(!is.list(classes)) classes <- list(classes)
    if(length(classes) > length(raster)){
      stop("The number of elements of classes must be equal to the required raster layers.")
    } else {
      if(length(classes) != length(raster)){
        for(i in (length(classes)+1):length(raster)){
          classes <- append(classes, list(x@classes[x@classes$rasterlayer == i, "class"]))
        }
      }
    }
    df_end <- data.frame()
    for(i in 1:length(classes)){
      if(!is.null(classes[[i]])){
        subset <- .subset_fn(classes[[i]])
        if(subset[[1]] == "error"){
          stop(strwrap("Inconsistencies were found in the definition of 'classes'. This should be a
                       list, with as many elements (each one an all negative or all positive numeric
                       vector) as available raster layers in 'x'.",
                       prefix = "\n", initial = "\n"), call. = FALSE)
        } else {
          for(r in 1:length(subset[[2]])){
            foo <- x@classes[x@classes$rasterlayer == i, ]
            if(is.numeric(subset[[2]][r])){
              if(!subset[[2]][r] %in% foo$class){
                stop(strwrap(paste0("Could not found class '", subset[[2]][r], "' from rasterlayer
                ", i, " inside 'x'. Mispelled?"), prefix = "\n", initial = "\n"), call. = FALSE)
              }
            } else {
              if(!subset[[2]][r] %in% foo$classname){
                stop(strwrap(paste0("Could not found class '", subset[[2]][r], "' from rasterlayer
                ", i, " inside 'x'. Mispelled?"), prefix = "\n", initial = "\n"), call. = FALSE)
              }
              subset[[2]][r] <- foo[foo$classname == subset[[2]][r], "class"]
            }
          }

          if(subset[[1]] == "stay"){
            df_tmp <- df_c_level[df_c_level$rasterlayer == i &
                                   df_c_level$class %in% subset[[2]], ]
          } else {
            df_tmp <- df_c_level[df_c_level$rasterlayer == i &
                                   !df_c_level$class %in% subset[[2]], ]
          }
        }
      }
      df_end <- rbind(df_end, df_tmp)
    }
    df_c_level <- df_end
  }

  new_df <- rbind(df_c_level, df_l_level)

  if(nrow(new_df) == 0){
    stop(strwrap("The final data.frame after the required subsetting threw a data.frame with zero
                 rows. Nothing to do.", prefix = "\n", initial = "\n"), call. = FALSE)
  }

  # transform data.frame from long to wide
  new_df_wide <- reshape(new_df, idvar = c("point_id", "site"),
                         timevar = c("metric_label"), drop = c("rasterlayer", "layer_name",
                                                               "patch_id", "class", "radius",
                                                               "classname", "metric", "level", "x",
                                                               "y"),
                         direction = "wide")

  colnames(new_df_wide)[3:ncol(new_df_wide)] <- gsub("value.", "",
                                                     colnames(new_df_wide)[3:ncol(new_df_wide)])

  # sort columns by rasterlayer
  new_df_wide_pp <- new_df_wide[, 1:2]
  new_df_wide_mm <- new_df_wide[3:ncol(new_df_wide)]
  if(ncol(new_df_wide_mm) > 1) new_df_wide_mm <- new_df_wide_mm[, order(names(new_df_wide_mm))]
  new_df_wide <- cbind(new_df_wide_pp, new_df_wide_mm)

  out <- list(new_df_wide, radii)

  return(out)
}

.rad_display <- function(summary, radii){
  summary_rad <- vector("list", length(radii))
  for(r in 1:length(radii)){
    metrics <- strsplit(rownames(summary), "_")
    cols <- which(sapply(metrics, FUN = function(x) radii[r] %in% x))
    f_names <- colnames(summary)[cols]
    f_names <- gsub(paste0("_", radii[r]), "", f_names)
    summary_rad[[r]] <- as.matrix(summary[cols, cols])
    colnames(summary_rad[[r]]) <- rownames(summary_rad[[r]]) <- f_names
    names(summary_rad)[[r]] <- paste0("radius_", radii[r])
  }
  return(summary_rad)
}

#' Pairwise metric correlations
#'
#' Calculates pairwise correlations between landscape metrics.
#'
#' @param x An object of class 'MultiLandMetrics' generated with [mland_metrics()].
#' @param method The method to be used to calculate pair correlations: "pearson" (default),
#' "spearman" or "kendall".
#' @param fun A user-defined function to calculate correlations. See Details.
#' @param raster,ext_raster,classes,radii,l_level,c_level Parameters to subset calculations of
#' correlations. See Details.
#' @param class_names Logical. If TRUE, row and column of returned matrices will be identified
#' with the names of the classes, if available in `x`. Default FALSE.
#' @param display Defines how correlations are presented: "radii" (default), "rl" or "both".
#' See Details.
#' @param ... Other arguments passed to function [cor()] or to the user-defined function provided
#' in `fun`.
#'
#' @details Correlations are calculated, by default, through the function [cor()], by specifying
#' the method through the argument `method`. Alternatively, a user-defined function can be provided
#' in the argument `fun`. If not NULL, the function will assume that a user-defined function
#' have been provided. This must be a function already loaded in the environment, and
#' must take at least two arguments. These initial pair of arguments should be capable of receiving
#' two numeric vectors (one in each argument), process them in some way, and return a numeric
#' value (i.e. the supposed correlation).
#'
#' Arguments `raster`, `ext_raster`, `classes`, `radii`, `c_level` and `l_level` can be defined to
#' subset the calculations of pair correlations. In each one of these, an all-positive or an
#' all-negative vector can be passed, whether to include (all-postive) or exclude (all-negative)
#' the elements to be taken into account for the subsetting:
#' * raster: a numeric vector with the number of the raster layers to be included/excluded.
#' For example: `c(1, 2, 4)` to include raster layers 1, 2 and 4; `c(-2, -3)` to exclude raster layers 2
#' and 3.
#' * ext_raster: a numeric vector with the number of the extra raster layers to be included/excluded,
#' as in the raster slot.
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
#' * radii: a numeric vector to include/exclude particular radii. For example: `c(1000, 2000)` to
#' include only radii of 1000 and 2000 m; `c(-500, -1500)` to exclude radii of 500 and 1500 m.
#' * c_level: character vector with the class-level metrics to be included/excluded from
#' the analysis. For example: `c("np", "pland")` will include only the metrics "number of patches"
#' ("np") and "percentage of the landscape" ("pland") in the analysis, whereas `c("-np", "-pland")`
#' will exclude them. Note the "-" before each metric name to indicate the exclusion of the
#' metrics.
#' * l_level: character vector with the landscape-level metrics to be included/excluded from
#' the analysis. Extra calculations for extra raster layers are considered as landscape-level metrics,
#' and must be provided as "fun_" + the name of the function (e.g. "fun_mean").
#'
#' Names of the available metrics of the 'MultiLandMetrics' object provided in `x` can
#' be accessed with `x@metrics` and `x@ext_calc`.
#'
#' Note that patch-level metrics, if exists in `x` metric's data.frame, are excluded from
#' calculations, as this function works at a landscape-scale analysis.
#'
#' Argument `display` defines how correlation values will be presented. If equals to "radii"
#' (default), correlation values are disaggregated by radii. If "rl", correlation values are
#' disaggregated by rasterlayer: correlations between different radii will be presented.
#' If "both", correlation values are firstly disaggregated by rasterlayer, and by radii secondly.
#' Disaggregations by raster layers only make sense for 'MultiLandMetrics' objects with more than one raster layer.
#'
#' @return A list with matrices containing correlation values between pair of metrics. Matrices
#' are disaggregated by radius if `display = "radii"`, by rasterlayer if `display = "rl"` or by
#' rasterlayer and radii if `display = "both"`. Metrics
#' names are presented as row and column names of the matrices, with the following format:
#' "level"_"metric_name"_"radius". For a landscape-level metric, a plausible metric name could be
#' "l_np_1500" indicating a landscape-level metric, which is "np" ("number of patches") at a scale
#' (radius) of 1500 m. For a class-level metric a plausible metric name could be "c4_pland_1000",
#' indicating a class-level metric of class 4 (the value of the raster), which is "pland"
#' ("percentage of landscape") at a scale (radius) of 1000 m. If more that one raster layer is
#' being analyzed, the prefix "r1", "r2", "r3", ..., "rn" (referring to raster layer 1, 2, 3, ..., n) is
#' added to the metric name.
#'
#' @seealso [mland_metrics()], [metrics_plots()]
#'
#' @export
#'
#' @examples
#' # Calculates pearson correlations between metrics of a MultiLandMetrics object
#' metrics_corr(ed_metrics)
#'
#' # Only for radius 5000 m and with classes names rather than classes values
#' metrics_corr(ed_metrics, radii = 5000, class_names = TRUE)
#'
#' # Only selecting the metric "pland"
#' metrics_corr(ed_metrics, radii = 5000, class_names = TRUE, c_level = "pland")
#'
#' # Excluding the metric "pland"
#' metrics_corr(ed_metrics, radii = 5000, class_names = TRUE, c_level = "-pland")
#'
#' # Excluding the metric radii of 4000 and 5000 m
#' metrics_corr(ed_metrics, radii = c(-4000, -5000), class_names = TRUE)
#'
#' # Correlations of metric "pland" between classes 1 to 3, and between radii
#' # 1000 and 5000 m, disaggregating by rasterlayer.
#' metrics_corr(ed_metrics, radii = c(1000, 5000), classes = 1:3,
#'              c_level = "pland", display = "rl")
metrics_corr <- function(x,
                         method = "pearson",
                         fun = NULL,
                         raster = NULL,
                         classes = NULL,
                         radii = NULL,
                         c_level = NULL,
                         l_level = NULL,
                         ext_raster = NULL,
                         class_names = FALSE,
                         display = "radii",
                         ...){

  if(!is(x, "MultiLandMetrics")) stop("x must be an object of class 'MultiLandMetrics'.")
  environment(.metrics_corr_chk_args) <- environment()
  chk <- .metrics_corr_chk_args(...)
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

  environment(.pair_subsets) <- environment()
  df <- tryCatch(.pair_subsets(),
                 error = function(e){
                   message("")
                   stop(e)})
  new_df_wide <- df[[1]]
  radii <- df[[2]]

  # Generating matrix
  summary <- matrix(data = rep(NA, length(3:ncol(new_df_wide))^2),
                    nrow = length(3:ncol(new_df_wide)),
                    ncol = length(3:ncol(new_df_wide)))

  d <- match.call()
  if("use" %in% names(as.list(d[-1]))){
    use <- d$use
  } else {
    use <- "pairwise.complete.obs"
  }
  for(j in 3:ncol(new_df_wide)){
    p1 <- new_df_wide[, j]
    for(i in 3:ncol(new_df_wide)){
      if(j <= i){
        p2 <- new_df_wide[, i]
        if(is.null(fun)){
          cor <- cor(p1, p2, method = method, use = use)
        } else {
          cor <- do.call(fun, list(p1, p2, ...))
        }
        summary[i-2, j-2] <- cor
      }
    }
  }
  summary <- as.matrix(summary)
  colnames(summary) <- rownames(summary) <- colnames(new_df_wide)[3:ncol(new_df_wide)]

  col_names <- strsplit(rownames(summary), "_")
  layers <- unique(sapply(col_names, FUN = function(x) x[1]))
  if(display == "rl" | display == "both"){
    summary_rl <- vector("list", length(layers))
    for(i in 1:length(layers)){
      if(length(layers) > 1){
        cols <- which(sapply(col_names, FUN = function(x) layers[i] %in% x))
        summary_rl[[i]] <- summary[cols, cols]
      } else {
        summary_rl[[i]] <- summary
      }
      names(summary_rl)[[i]] <- layers[i]

      if(display == "both"){
        summary_rl[[i]] <- .rad_display(summary_rl[[i]], radii)
      }
    }
    summary <- summary_rl
  } else {
    if(display == "radii"){
      summary <- .rad_display(summary, radii)
    }
  }

  return(summary)
}
