.optimize_gradient_chk_args <- function(){

  messages <- NULL
  what     <- NULL

  # Rasterlayer
  if(is.null(rasterlayer)){
    rasterlayer <- unique(x@data$rasterlayer)
  }
  if(!rasterlayer %in% unique(x@data$rasterlayer)){
    messages <- append(messages,
                       paste0("- specified rasterlayer was not found as a defined layer in
                                        'x'. Mispelled?"))
    what     <- append(what, 2)
  } else {
    # Class
    if(!is.null(class)){
      if(!grepl("ext", rasterlayer)){
        foo <- x@classes[x@classes$rasterlayer %in% rasterlayer, ]
        if(is.numeric(class)){
          if(!class %in% foo$class){
            messages <- append(messages,
                               paste0("- specified class was not found as a defined
                                 class of rasterlayer ", rasterlayer, " in 'x'. Mispelled?"))
            what     <- append(what, 2)
          }
        } else {
          if(is.character(class)){
            if(!class %in% foo$classname){
              messages <- append(messages,
                                 paste0("- specified class was not found as a defined
                                                class of rasterlayer ", rasterlayer,
                                        " in 'x'. Mispelled?"))
              what     <- append(what, 2)
            } else {
              class <- foo[foo$classname == class, "class"]
            }
          }
        }
      }
    } else {
      class <- x@classes[x@classes$rasterlayer %in% rasterlayer, ]$class
    }
  }

  # Radius
  if(!is.null(radius)){
    if(!radius %in% x@radii){
      messages <- append(messages,
                         paste0("- specified radius was not found as a defined radius in
                                        'x'. Mispelled?"))
      what     <- append(what, 2)
    }
  } else {
    radius <- unique(x@data$radius)
  }

  # Metric
  if(!is.null(metric)){
    if(!all(metric %in% unique(x@data$metric))){
      messages <- append(messages,
                         paste0("- specified metric was not found as a defined metric in
                                      'x'. Mispelled?"))
      what     <- append(what, 2)
    }
  } else {
    metric <- unique(x@data$metric)
  }

  foo <- x@data[x@data$rasterlayer %in% rasterlayer &
                  x@data$class %in% class &
                  x@data$radius %in% radius &
                  x@data$metric %in% metric, ]
  if(nrow(foo) == 0){
    messages <- append(messages,
                       paste0("- no metric values where found in \"x\" given the specified
                              parameters."))
    what     <- append(what, 2)
  } else {
    if(nrow(unique(foo[, c("rasterlayer", "class", "radius", "metric")])) != 1){
      messages <- append(messages,
                         paste0("- only one gradient of a unique metric of a particular rasterlayer,
                         class and radius can be optimized at a time."))
      what     <- append(what, 2)
    }
  }

  if(is.numeric(n)){
    if(length(n) != 1){
      messages <- append(messages,
                         paste0("- argument \"n\" must be a unique number."))
      what     <- append(what, 2)
    } else {
      if(!2 %in% what){
        if(n > nrow(foo)){
          messages <- append(messages,
                             paste0("- The number of required points (", n, ") is higher than the total number
                                  of points where to pick from (", nrow(foo), "), given the specified parameters.
                                  All points were selected, nothing have changed."))
          what     <- append(what, 2)
          n <- nrow(foo)
        }
      }
    }
  } else {
    messages <- append(messages,
                       paste0("- argument \"n\" must be numeric."))
    what     <- append(what, 2)
  }

  if(!is.null(cutpoints)){
    if(!is.null(cutpoints)){
      if(!is.numeric(cutpoints)){
        messages <- append(messages,
                           paste0("- argument \"cutpoints\" must be numeric."))
        what     <- append(what, 2)
      } else {
        if(length(cutpoints) > n){
          messages <- append(messages,
                             paste0("- number of cutpoints cannot be higher than the number of required points."))
          what     <- append(what, 2)
        }
      }
    }
  }

  if(!is.null(breaks)){
    if(is.numeric(breaks)){
      if(length(breaks) != 1){
        messages <- append(messages,
                           paste0("- argument \"breaks\" must be a unique number."))
        what     <- append(what, 2)
      }
    } else {
      messages <- append(messages,
                         paste0("- argument \"breaks\" must be numeric."))
      what     <- append(what, 2)
    }
  }

  if(!output %in% c("MLM", "spatial", "data", "coords")){
    messages <- append(messages,
                       "- argument output must be \"MLM\", \"spatial\", \"data\" or \"coords\". Default
                       \"MLM\" was taken. See ?landscape_select")
    what     <- append(what, 1)
    output <- "MLM"
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              n = n,
              class = class,
              output = output)
  return(out)
}
