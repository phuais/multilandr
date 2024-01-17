# mland argument checking
.mland_chk_args <- function() {

  messages <- NULL
  what     <- NULL

  # Points
  if(!class(points) %in% c("character", "SpatVector", "SpatialPointsDataFrame", "SpatialPoints")){
    messages <- append(messages,
                            "- argument 'points' must be a filename (string) or an object of class
                       'SpatVector', 'SpatialPointsDataFrame' or 'SpatialPoints'.")
    what     <- append(what, 2)
  } else {
    if(!class(points) %in% c("SpatVector", "SpatialPointsDataFrame", "SpatialPoints")){
      if(!file.exists(points)){
        messages <- append(messages, paste("- could not find points layer file: ", points, "."))
        what     <- append(what, 2)
      }
    }
  }

  # Rasters
  if(is.null(raster) & is.null(ext_raster)){
    messages <- append(messages, "- at least one rasterlayer must be specified in 'raster'
                       and/or 'ext_raster'.")
    what     <- append(what, 2)
  } else {
    # Raster
    chk_raster <- .chk_raster(messages, what, raster, arg_name = "raster")
    messages   <- chk_raster[[1]]
    what       <- chk_raster[[2]]
    raster     <- chk_raster[[3]]

    # Extra Raster
    chk_ext_raster <- .chk_raster(messages, what, ext_raster, arg_name = "ext_raster")
    messages       <- chk_ext_raster[[1]]
    what           <- chk_ext_raster[[2]]
    ext_raster     <- chk_ext_raster[[3]]
   }

  # On the fly
  if(!is.logical(onthefly)){
    messages <- append(messages, "- argument 'onthefly' must be logical.")
    what     <- append(what, 2)
  }

  # Classnames
  if(!is.null(classnames)){
    if(is.list(classnames)){
      if(length(classnames) > 0){
        for(i in 1:length(classnames)){
          if(is.character(classnames[[i]])){
            cl <- seq(1, length(classnames[[i]]), 2)
            if((length(classnames[[i]]) %% 2) != 0 |
               !all(!is.na(suppressWarnings(as.numeric(classnames[[i]][cl]))))){
              messages   <- append(messages, "- argument 'classnames' was not defined properly. The
                               argument was ignored. See ?mland.")
              what       <- append(what, 1)
              classnames <- list()
              break
            } else {
              if(!is.character(classnames[[i]])){
                messages   <- append(messages, "- argument 'classnames' must be a list with vector of
                                 strings in even elements. The argument was ignored. See ?mland")
                what       <- append(what, 1)
                classnames <- list()
                break
              }
            }
          } else {
            messages   <- append(messages, "- argument 'classnames' was not defined properly. The
                               argument was ignored. See ?mland.")
            what       <- append(what, 1)
            classnames <- list()
            break
          }
        }
      }
    } else {
      messages   <- append(messages, "- argument 'classnames' must be a list. The argument was
                         ignored.")
      what       <- append(what, 1)
      classnames <- list()
    }
  } else { classnames <- list() }

  # Raster names
  if(!is.null(layer_names)){
    if(!is.character(layer_names)){
      messages   <- append("- argument 'layer_names' must be a character vector.
                           Argument was discarded.")
      what       <- append(what, 1)
      layer_names <- vector("character")
    } else {
      if(any(duplicated(layer_names))){
        messages   <- append("- argument 'layer_names' contains duplicated string. Rasterlayers
        names should be unique to avoid ambiguites. Argument was discarded.")
        what       <- append(what, 1)
        layer_names <- vector("character")
      }
    }
  } else { layer_names <- vector("character") }

  # Site reference
  if(!is.null(site_ref)){
    if(!is.character(site_ref) | length(site_ref) > 1){
      messages   <- append(messages, "- argument 'site_ref' must be a string. The argument was
                           ignored.")
      what       <- append(what, 1)
      site_ref <- NULL
    }
  }

  # Radii
  if(!is.numeric(radii) | !all(radii > 0)){
    messages <- append(messages, "- argument 'radii' must be a vector of positive numbers.")
    what     <- append(what, 2)
  }

  # Type of buffer
  if(length(bufftype) > 1 | !is.character(bufftype) |
     !tolower(bufftype[1]) %in% c("round", "square")){
    messages <- append(messages, "- argument 'bufftype' must be \"round\" or \"square\". Default \"round\"
                       was taken.")
    what     <- append(what, 1)
    bufftype <- "round"
  }

  # Segs of buffers
  if(!is.numeric(segs) | length(segs) > 1 | segs[1] < 0){
    messages <- append(messages, "- argument 'segs' must be a positive integer. Default 20 was
                       taken.")
    what     <- append(what, 1)
    segs     <- 20
  }

  # Progress
  chk_progress <- .chk_progress(messages, what, progress)
  messages <- chk_progress[[1]]
  what     <- chk_progress[[2]]

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              classnames = classnames,
              site_ref = site_ref,
              bufftype = bufftype,
              segs = segs,
              raster = raster,
              ext_raster = ext_raster,
              layer_names = layer_names,
              progress = progress)
  return(out)
}

.check_pointsCRS <- function(points){
  mess <- FALSE
  if(terra::crs(points) == ""){
    mess <- "- points layer must be projected in a crs with meters unit." }
  return(mess)
}

.check_classCRS <- function(points, class){
  #mess <- if(!sp::identicalCRS(points, class)) 1 else 0
  mess <- if(!terra::same.crs(points, class)) 1 else 0
  return(mess)
}

# .check_pointsref <- function(points, site_ref){
#   mess <- 0
#     if(!site_ref %in% colnames(points@data)){
#       mess <- 1
#     } else {
#       if(anyDuplicated(points@data[site_ref][, 1]) != 0){
#         mess <- 2 ; return(mess)
#       }
#       if(any(is.na(points@data[site_ref][, 1]))){
#         mess <- 3 ; return(mess)
#       }
#     }
#   return(mess)
# }

.check_pointsref <- function(points, site_ref){
  mess <- 0
  if(!site_ref %in% names(points)){
    mess <- 1
  } else {
    if(anyDuplicated(terra::as.list(points)[[site_ref]]) != 0){
      mess <- 2 ; return(mess)
    }
    if(any(is.na(terra::as.list(points)[[site_ref]]))){
      mess <- 3 ; return(mess)
    }
  }
  return(mess)
}
