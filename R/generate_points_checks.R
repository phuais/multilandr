.generate_points_chk_args <- function(){

  messages <- NULL
  what     <- NULL

  # Raster
  if(!class(raster) %in% c("RasterLayer", "RasterStack", "RasterBrick", "SpatRaster")){
    messages <- append(messages,
                       "- argument 'raster' must be a rasterlayer. See ?generate_coods")
    what     <- append(what, 2)
  }

  # approach
  if(!approach %in% c("grid", "random", "patch")){
    messages <- append(messages,
                       "- argument 'approach' must be \"grid\", \"random\" or \"patch\".")
    what     <- append(what, 2)
  } else {
    if(approach == "random"){
      # n
      if(!is.null(n)){
        if(!is.positive.wholenumber(n)){
          messages <- append(messages,
                             "- argument 'n' must be a positive whole number.")
          what     <- append(what, 2)
        }
      } else {
        messages <- append(messages,
                           "- argument 'n' must be a positive whole number.")
        what     <- append(what, 2)
      }

      # try
      if(!is.null(try)){
        if(!is.positive.wholenumber(try)){
          messages <- append(messages,
                             "- argument 'try' must be a positive integer.")
          what     <- append(what, 2)
        }
      }

      # values
      if(!is.null(values)){
        if(!is.numeric(values)){
          messages <- append(messages,
                             "- argument 'values' must be a numeric vector.")
          what     <- append(what, 2)
        }
        if(class(raster) %in% c("RasterStack", "RasterBrick")){
          messages <- append(messages,
                             "- if specific values are defined, the input cannot be an object of class
                             'RasterStack' or 'RasterBrick'.")
          what     <- append(what, 2)
        }
      }

      # trim
      if(!is.logical(trim)){
        messages <- append(messages,
                           "- argument 'trim' must be logical. Default TRUE was taken.")
        what     <- append(what, 1)
        trim <- TRUE
      }

      # attempts
      if(!is.positive.wholenumber(attempts)){
        messages <- append(messages,
                           "- argument 'attempts' must a postive whole number. Default 10 was taken.")
        what     <- append(what, 1)
        attempts <- 10
      }

      # offset
      if(!is.logical(offset)){
        messages <- append(messages,
                           "- argument 'offset' must be logical. Default FALSE was taken.")
        what     <- append(what, 1)
        offset <- FALSE
      }
    }

    if(approach == "patch"){
      # patch_conditions
      if(!is.list(patch_conditions)){
        messages <- append(messages,
                           "- argument 'patch_conditions' must be a list.")
        what     <- append(what, 2)
      } else {
        for(i in 1:length(patch_conditions)){
          if(!is.list(patch_conditions[[i]])){
            messages <- append(messages,
                               paste0("- patch condition number ", i, " is not a list. Each condition must
                                    be a list. See ?generate_points"))
            what     <- append(what, 2)
          } else {
            if(length(patch_conditions[[i]]) != 4){
              messages <- append(messages,
                                 paste0("- patch_condition number ", i, " must be a list with four elements:
                                  c(class, metric, min. value, max. value).
                                    More details in ?generate_points"))
              what     <- append(what, 2)
            } else {

              # Metric
              if(length(patch_conditions[[i]][[2]]) > 1){
                messages <- append(messages,
                                   "- only one metric can be defined per patch condition.")
                what     <- append(what, 2)
              } else {
                met <- metrics_list(level = "patch")$metric
                if(!patch_conditions[[i]][[2]] %in% met){
                  messages <- append(messages,
                                     paste0("- metric \"", patch_conditions[[i]][[2]], "\" from
                                         patch condition ", i, " was not found as an available
                                         metric to be calculated. See metrics_list(level = \"patch\").
                                          Mispelled?"))
                  what     <- append(what, 2)
                }
              }

              # Min. value
              if(is.na(suppressWarnings(as.numeric(patch_conditions[[i]][[3]])))){
                messages <- append(messages,
                                   paste0("- min. value (3rd element) from patch condition ", i,
                                          " must be numeric."))
                what     <- append(what, 2)
              }
              # Max. value
              if(is.na(suppressWarnings(as.numeric(patch_conditions[[i]][[4]])))){
                messages <- append(messages,
                                   paste0("- max. value (4th element) from patch condition ", i,
                                          " must be numeric."))
                what     <- append(what, 2)
              }
            }
          }
        }
      }
    } else {
      if(!is.null(patch_conditions)){
        messages <- append(messages,"- patch conditions were ignored.")
        what     <- append(what, 2)
      }
    }
  }

  # distance
  if(!is.null(distance)){
    if(!is.positive.numeric(distance)){
      messages <- append(messages,
                         "- argument 'distance' must a postive number.")
      what     <- append(what, 2)
    }
  } else {
    if(approach == "grid"){
      messages <- append(messages,
                         "- if approach = 'grid', argument 'distance' must a postive number.")
      what     <- append(what, 2)
    } else {
      distance <- 0
    }
  }

  # parallel
  if(!is.logical(parallel)){
    messages <- append(messages,
                       "- argument 'parallel' must be logical. Default FALSE was taken.")
    what     <- append(what, 1)
    parallel <- FALSE
  }

  # closest cell
  if(!is.logical(closest_cell)){
    messages <- append(messages,
                       "- argument 'closest_cell' must be logical. Default FALSE was taken.")
    what     <- append(what, 1)
    closest_cell <- FALSE
  }

  # Progress
  chk_progress <- .chk_progress(messages, what, progress)
  messages <- chk_progress[[1]]
  what     <- chk_progress[[2]]

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              trim = trim,
              attempts = attempts,
              offset = offset,
              parallel = parallel,
              closest_cell = closest_cell,
              distance = distance,
              progress = progress)
  return(out)
}
