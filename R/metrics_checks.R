.metrics_chk_args <- function(){

  what_lsm <- what
  messages <- NULL
  what     <- NULL

  # Points
  chk_points <- .chk_points(messages, what, x, points)
  messages <- chk_points[[1]]
  what     <- chk_points[[2]]
  points   <- chk_points[[3]]

  # Rasterlayer
  raster <- sort(unique(raster))
  if(!is.null(raster)){
    if(is.character(raster)){
      if(!all(raster %in% x@layer_names[[1]]$name)){
        messages <- append(messages,
                           "- in argument 'raster': required rasterlayers were not found in
                           'x'. Mispelled?")
        what     <- append(what, 2)
      } else {
        raster <- sort(x@layer_names[[1]][x@layer_names[[1]]$name %in% raster, "rasterlayer"])
      }
    } else {
      if(!is.positive.wholenumber(raster)){
        messages <- append(messages,
                           "- in argument 'raster': if not NULL, it must be a character vector with
                           rasterlayer names or a vector of positive wholenumbesr.")
        what     <- append(what, 2)
      } else {
        if(!all(raster %in% 1:x@n_layers)){
          messages <- append(messages,
                             "- in argument 'raster': required rasterlayers were not found in
                           'x'. Mispelled?")
          what     <- append(what, 2)
        }
      }
    }
  }

  # Extra calculations
  ext_calc_ref <- data.frame(rasterlayer = numeric(0), name = character(0), fun = character(0))
  if(!is.null(ext_calc)){
    if(length(x@landscapes$ext_rasters) > 0){
      if(!is.list(ext_calc)){
        messages <- append(messages, "- if not NULL, argument 'ext_calc' must be a list. Details in
                         ?metrics.")
        what     <- append(what, 2)
      } else {
        for(i in 1:length(ext_calc)){
          if(length(ext_calc[[i]]) > 1){
            if(!is.numberinchar(ext_calc[[i]][1])){
              layname <- ext_calc[[i]][1]
              lay <- x@layer_names[[2]][x@layer_names[[2]]$name == layname, "rasterlayer"]
              if(length(lay) == 0) lay <- NA
            } else {
              lay <- layname <- as.numeric(ext_calc[[i]][1])
            }
            if(!lay %in% x@layer_names[[2]]$rasterlayer){
              messages <- append(messages,
                                 paste0("- in argument 'ext_calc': defined extra rasterlayer \"",
                                        layname,"\" was not found as an extra raster in 'x'.
                             Mispelled?"))
              what     <- append(what, 2)
            } else {
              for(j in 2:length(ext_calc[[i]])){
                if(exists(ext_calc[[i]][j])){
                  ext_calc_ref <- rbind(ext_calc_ref, data.frame(rasterlayer = lay,
                                                                 name = x@layer_names[[2]][
                                                                   x@layer_names[[2]]$rasterlayer == lay,
                                                                   "name"],
                                                                 fun = ext_calc[[i]][j]))
                } else {
                  messages <- append(messages,
                                     paste0("- in argument 'ext_calc': could not find a function
                                          named '", ext_calc[[i]][j], "'. Mispelled?"))
                  what     <- append(what, 2)
                }
              }
            }
          } else {
            messages <- append(messages,
                               paste0("- argument 'ext_calc' must be a list containing one or more
                               vectors, each one's length equal to 2 or more. See ?metrics."))
            what     <- append(what, 2)
            break
          }
        }
      }
    } else {
      messages <- append(messages, "- if argument 'ext_calc' is defined, 'x' must contain extra
      rasterlayers. Details in ?metrics.")
      what     <- append(what, 2)
    }
  }

  # Radii
  chk_radii <- .chk_radii(messages, what, x, radii)
  messages <- chk_radii[[1]]
  what     <- chk_radii[[2]]

  # If no extra calculations, take all main rasterlayers
  if(is.null(raster)){
    if(nrow(ext_calc_ref) == 0){
      raster <- 1:x@n_layers
    } else {
      if(!all(unlist(lapply(list(what_lsm, type, level, name, metric), is.null)))){
        raster <- 1:x@n_layers
      }
    }
  }

  # Classes
  rasters_classes_tab <- data.frame()
  if(!is.null(raster)){
    if(!is.null(classes)){
      if(!is.list(classes)) classes <- list(classes)
      if(length(classes) != length(raster)){
        messages <- append(messages, "- inconsistencies were found between the number of required
                         rasterlayers and the definition of argument 'classes'. More details in
                         ?metrics.")
        what     <- append(what, 2)
      } else {
        for(i in 1:length(classes)){
          x_classes <- x@classes[x@classes$rasterlayer == raster[i], "class"]
          x_classnames <- x@classes[x@classes$rasterlayer == raster[i], "classname"]

          if(is.numeric(classes[[i]]))
            cond <- all(classes[[i]] %in% x_classes)
          else
            cond <- all(classes[[i]] %in% x_classnames)
          if(!cond){
            messages <- append(messages, paste0("- in argument 'classes': one or more classes defined
                                              for rasterlayer ", raster[i], " did not match
                                              with the classes defined in 'x' for that rasterlayer.
                                              Mispelled?"))
            what     <- append(what, 2)
          } else {
            if(!is.numeric(classes[[i]])){
              classes[[i]] <- x_classes[x_classnames %in% classes[[i]]]
            } else {
              classes[[i]] <- sort(classes[[i]])
            }
            tmp_df <- x@classes[x@classes$rasterlayer == raster[i] &
                                  x@classes$class %in% classes[[i]], ]
            rasters_classes_tab <- rbind(rasters_classes_tab, tmp_df)
          }
        }
      }
    } else {
      classes <- vector("list", length(raster))
      rasters_classes_tab <- data.frame()
      for(i in 1:length(classes)){
        classes[[i]] <- x@classes[x@classes$rasterlayer == raster[i], "class"]
        tmp_df <- x@classes[x@classes$rasterlayer == raster[i] &
                              x@classes$class %in% classes[[i]], ]
        rasters_classes_tab <- rbind(rasters_classes_tab, tmp_df)
      }
    }
  }

  # Metrics
  mets_info <- list(what_lsm, level, name, type, metric)
  lsm_cols <- c("function_name", "level", "name", "type", "metric")
  for(i in 1:length(mets_info)){
    if(!is.null(mets_info[[i]])){
      if(is.character(mets_info[[i]])){
        for(j in 1:length(mets_info[[i]])){
          if(!mets_info[[i]][j] %in% metrics_list()[, lsm_cols[i]]){
            messages <- append(messages, "- one or more required metrics are not in the list of
                       available metrics. Mispelled? Please check metrics_list()")
            what     <- append(what, 2)
            break
          }
        }
      } else {
        messages <- append(messages, "- if not NULL, arguments 'level', 'metric', 'name', 'type'
                           and 'what' must be character.")
        what     <- append(what, 2)
        break
      }
    }
  }

  if(all(unlist(lapply(list(what_lsm, type, level, name, metric, ext_calc), is.null)))){
    messages <- append(messages, "- none metrics of any type were required. Nothing to do.")
    what     <- append(what, 2)
  }

  # Report absences
  if(!is.logical(report_absences)){
    messages <- append(messages, "- argument 'report_absences' must be logical. Default TRUE was
                       taken.")
    what     <- append(what, 1)
    report_absences <- TRUE
  }

  # Absences values
  if(!is.null(absence_values)){
    if(!is.list(absence_values)){
      messages <- append(messages, "- argument 'absence_values' must be NULL or a list. Details in
                         ?metrics.")
      what     <- append(what, 2)
    }
  }

  # na.exclude
  if(!is.logical(na.exclude)){
    messages <- append(messages, "- argument 'na.exclude' must be logical. Default TRUE was taken.")
    what     <- append(what, 1)
    na.exclude <- TRUE
  }

  # Output
  if(!output[1] %in% c("MLM", "data")){
    messages <- append(messages, "- argument 'output' must be \"MLM\" or \"data\". Default \"MLM\" was
                       taken.")
    what     <- append(what, 1)
    output   <- "MLM"
  }

  # Coordinates
  if(!is.logical(coords)){
    messages <- append(messages, "- argument 'coords' must be logical. Default FALSE was taken.")
    what     <- append(what, 1)
    coords   <- FALSE
  }

  # Update
  if(!is.null(update)){
    if(!is(update, "MultiLandMetrics")){
      messages <- append(messages,
                         "- argument 'update' must be an object of class 'MultiLandMetrics'.")
      what     <- append(what, 2)
    } else {
      if(x@idkey != update@idkey){
        messages <- append(messages,
                           "- key of the 'MultiLand' object provided in 'x' does not match the
                         key of the 'MultiLandMetrics' object provided in 'update'. See
                         details in ?metrics.")
        what     <- append(what, 2)
      }
    }
  }

  # Progress
  chk_progress <- .chk_progress(messages, what, progress)
  messages <- chk_progress[[1]]
  what     <- chk_progress[[2]]
  progress <- chk_progress[[3]]

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              raster = raster,
              classes = classes,
              rasters_classes_tab = rasters_classes_tab,
              points = points,
              report_absences = report_absences,
              output = output,
              coords = coords,
              ext_calc_ref = unique(ext_calc_ref),
              na.exclude = na.exclude,
              progress = progress)

  return(out)
}
