.chk_site_ref <- function(messages, what, x, site_ref, points){
  # site_ref
  if(!is.logical(site_ref)){
    messages <- append(messages,
                       "- argument 'site_ref' must be logical. The argument was ignored.")
    what     <- append(what, 1)
    site_ref <- FALSE
  } else {
    if(site_ref){
      if(is.null(x@site_ref)){
        messages <- append(messages,
                           "- argument 'site_ref' was TRUE, but x does not declare the same. Set site_ref to FALSE or get an x object with site_ref equal to TRUE.")
        what     <- append(what, 2)
      } else {
        if(!is.null(points)){
          if(!all(unique(points) %in% x@points[, x@site_ref])){
            messages <- append(messages,
                               "- one or more required points were not found in x. Mispelled?")
            what     <- append(what, 2)
          }
        }
      }
    } else {
      if(!is.null(points)){
        if(!all(unique(points) %in% 1:length(x@points))){
          messages <- append(messages,
                             "- one or more required points were not found in x. Mispelled?")
          what     <- append(what, 2)
        }
      }
    }
  }

  out <- list(messages, what, site_ref)
  return(out)
}

.chk_raster <- function(messages, what, raster, arg_name){
  if(!is.null(raster)){
    if(!class(raster) %in% c("RasterLayer", "RasterStack", "RasterBrick", "SpatRaster", "list")){
      messages <- append(messages, paste0("- argument '", arg_name, "' must be a rasterlayer or a
                                          list of raster layers. See ?mland"))
      what     <- append(what, 2)
    }
    if(is(raster, "list")){
      cl <- vector("logical", length(raster))
      for(i in 1:length(raster)){ cl[i] <- class(raster[[i]]) %in% c("RasterLayer", "SpatRaster") }
      if(!all(cl)){
        messages <- append(messages, paste0("- if provided a list in argument '", arg_name, "', this must be a
                         list of objects of raster layers. See ?mland"))
        what     <- append(what, 2)
      }
    }
  } else {
    raster <- vector("list")
  }

  out <- list(messages, what, raster)
  return(out)
}

.chk_radii <- function(messages, what, x, radii){
  if(!is.null(radii)){
    if(!is.numeric(radii) | !all(radii > 0)){
      messages <- append(messages, "- argument 'radii' must be a vector of positive numbers.")
      what     <- append(what, 2)
    } else {
      if(!all(radii %in% x@radii)){
        messages <- append(messages, "- one or more defined radii were not found in x definition.")
        what     <- append(what, 2)
      }
    }
  }

  out <- list(messages, what)
  return(out)
}

.chk_points <- function(messages, what, x, points){
  force_sitename <- F
  if(!is.null(points)){
    for(i in 1:length(points)){
      if(is.numeric(points[i])){
        if(!points[i] %in% terra::values(x@points)[, "id"]){
          messages <- append(messages,
                             paste0("- could not found point ", points[i], " in x points data
                               definition. Mispelled?"))
          what     <- append(what, 2)
        }
      } else {
        if(is.character(points[i])){
          if(!is.null(x@site_ref)){
            if(!points[i] %in% terra::values(x@points)[, x@site_ref]){
              messages <- append(messages,
                                 paste0("- could not found site \"", points[i], "\" in x points data
                               definition. Mispelled?"))
              what     <- append(what, 2)
            } else {
              points[i] <- terra::values(x@points)[terra::values(x@points)[, x@site_ref] == points[i], "id"]
              force_sitename <- T
            }
          } else {
            messages <- append(messages,
                               "- site names were declared in argument 'points' but no names were
                               found in 'x'. Declare point ids or generate an 'x' object that
                               includes site names.")
            what     <- append(what, 2)
            break
          }
        } else {
          messages <- append(messages,
                             "- If not NULL, argument 'points' must be a numeric or a character
                             vector.")
          what     <- append(what, 2)
          break
        }
      }
    }
  }

  out <- list(messages, what, points, force_sitename)
  return(out)
}

.chk_points2 <- function(messages, what, x, points){
  if(!is.null(points)){
    for(i in 1:length(points)){
      if(is.numeric(points[i])){
        if(!points[i] %in% x@points$id){
          messages <- append(messages,
                             paste0("- could not found point ", points[i], " in x. Mispelled?"))
          what     <- append(what, 2)
        }
      } else {
        if(is.character(points[i])){
          if(x@site_names){
            if(!points[i] %in% x@points$name){
              messages <- append(messages,
                                 paste0("- could not found site \"", points[i], "\" in x. Mispelled?"))
              what     <- append(what, 2)
            } else {
              points[i] <- x@points[x@points$name == points[i], ]$id
            }
          } else {
            messages <- append(messages,
                               "- site names were declared in argument 'points' but no names were
                               found in 'x'.")
            what     <- append(what, 2)
            break
          }
        } else {
          messages <- append(messages,
                             "- if not NULL, argument 'points' must be a numeric or a character
                             vector.")
          what     <- append(what, 2)
          break
        }
      }
    }
  }

  out <- list(messages, what, points)
  return(out)
}

.chk_title <- function(messages, what, x, title){
  # title
  if(!title %in% c("id", "sitename")){
    messages <- append(messages,
                       "- argument 'title' must be one of the following: \"id\" or \"sitename\". Default
                       \"id\" was taken.")
    what     <- append(what, 1)
    title    <- "id"
  } else {
    if(title == "sitename"){
      if(is.null(x@site_ref)){
        messages <- append(messages,
                           "- if argument 'title' equals \"sitename\", x@site_ref must be TRUE. Default
                           \"id\" was taken.")
        what     <- append(what, 1)
        title    <- "id"
      }
    }
  }

  out <- list(messages, what, title)
  return(out)
}

.chk_classnames <- function(messages, what, show_class_names){

  if(!is.logical(show_class_names)){
    messages <- append(messages,
                       "- argument 'show_class_names' must be logical. Default FALSE was taken.")
    what     <- append(what, 1)
    show_class_names <- FALSE
  }

  out <- list(messages, what, show_class_names)
  return(out)
}

.chk_progress <- function(messages, what, progress){
  if(!is.logical(progress)){
    messages <- append(messages, "- argument 'progress' was taken as TRUE.")
    what     <- append(what, 1)
    progress <- TRUE
  }

  out <- list(messages, what, progress)
  return(out)
}

