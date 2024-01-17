# mland_plot() arguments checking
.mland_plot_check_args <- function(){

  messages <- NULL
  what     <- NULL

  # Rasterlayer
  arg_name <- "raster"
  raster_isok <- T
  if(is.null(raster) & is.null(ext_raster)){
    raster <- 1
    if(x@n_layers == 0){
      arg_name <- "ext_raster"
      message("- argument 'ext_raster' was taken as equal to 1.")
    } else {
      message("- argument 'raster' was taken as equal to 1.")
    }
  } else {
    if(all(!is.null(raster), !is.null(ext_raster))){
      messages <- append(messages, "- only one rasterlayer can be plotted, which can be specified
      either in arguments 'raster' or 'ext_raster'.")
      what     <- append(what, 2)
      raster_isok <- F
    } else {
      if(!is.null(ext_raster)){
        raster <- ext_raster
        arg_name <- "ext_raster"
        if(length(x@landscapes$ext_rasters) > 0){
          rast_search <- x@layer_names[[2]]
        } else {
          rast_search <- F
        }
      } else {
        if(length(x@landscapes$lsm_rasters) > 0){
          rast_search <- x@layer_names[[1]]
        } else {
          rast_search <- F
        }
      }
      if(is.logical(rast_search)){
        messages <- append(messages,
                           paste0("- in argument '", arg_name,
                                  "': required rasterlayer was not found in x."))
        what     <- append(what, 2)
        raster_isok <- F
      } else {
        if(length(raster) > 1){
          messages <- append(messages,
                             paste0("- in argument '", arg_name,
                                    "': only one rasterlayer can be selected."))
          what     <- append(what, 2)
          raster_isok <- F
        } else {
          if(is.character(raster)){
            raster <- rast_search[rast_search$name == raster, "rasterlayer"]
            if(length(raster) == 0) raster <- max(rast_search$rasterlayer) + 1
            if(is.na(raster)) raster <- max(rast_search$rasterlayer) + 1
          }
          if(!is.positive.wholenumber(raster)){
            messages <- append(messages,
                               paste0("- argument '", arg_name, "' must be a positive wholenumber
                               depicting the rasterlayer number, or a string depicting the
                                      rasterlayer name."))
            what     <- append(what, 2)
            raster_isok <- F
          } else {
            if(!raster %in% rast_search$rasterlayer){
              messages <- append(messages,
                                 paste0("- in argument '", arg_name,
                                        "': required rasterlayer was not found in x."))
              what     <- append(what, 2)
              raster_isok <- F
            }
          }
        }
      }
    }
  }

  # Points
  chk_points <- .chk_points(messages, what, x, points)
  messages <- chk_points[[1]]
  what     <- chk_points[[2]]
  points   <- chk_points[[3]]

  # Radii
  chk_radii <- .chk_radii(messages, what, x, radii)
  messages <- chk_radii[[1]]
  what     <- chk_radii[[2]]

  # title
  chk_title <- .chk_title(messages, what, x, title)
  messages  <- chk_title[[1]]
  what      <- chk_title[[2]]
  title     <- chk_title[[3]]
  if(chk_points[[4]]) title <- "sitename"

  # nrow and ncol
  if(!is.null(nrow)){
    if(!is.positive.wholenumber(nrow)){
      messages <- append(messages,
                         "- if not NULL, argument 'nrow' must be a positive wholenumber. Default
                         NULL was taken.")
      what     <- append(what, 1)
      nrow     <- NULL
    }
  }
  if(!is.null(ncol)){
    if(!is.positive.wholenumber(ncol)){
      messages <- append(messages,
                         "- if not NULL, argument 'ncol' must be a positive wholenumber. Default
                         NULL was taken.")
      what     <- append(what, 1)
      ncol     <- NULL
    }
  }

  # points style
  args <- list(shape = 21, size = 2, col = "black", fill = "white", alpha = 1)
  if(!all(names(args) %in% names(st_points))){
    for(i in 1:5){
      if(!names(args)[i] %in% names(st_points)){
        st_points <- append(st_points, args[i])
        names(st_points)[length(st_points)] <- names(args)[i]
      }
    }
  }

  # buffers style
  args <- list(lty = 1, lwd = 1, col = "black", alpha = 1)
  if(!all(names(args) %in% names(st_buffers))){
    for(i in 1:5){
      if(!names(args)[i] %in% names(st_buffers)){
        st_buffers <- append(st_buffers, args[i])
        names(st_buffers)[length(st_buffers)] <- names(args)[i]
      }
    }
  }

  # classes style
  args <- list(palette = "Spectral", fill = NULL, alpha = NULL, na_value = c("white", 1))
  if(!all(names(args) %in% names(st_classes))){
    for(i in 1:4){
      if(!names(args)[i] %in% names(st_classes)){
        st_classes <- append(st_classes, args[i])
        names(st_classes)[length(st_classes)] <- names(args)[i]
      }
    }
  }

  if(raster_isok){
    if(arg_name == "raster"){
      # check fill parameters
      cl_vals <- x@classes[x@classes$rasterlayer == raster, "class"]
      cl_names <- x@classes[x@classes$rasterlayer == raster, "classname"]
      orig_cl <- c(cl_vals, cl_names)
      if(!is.null(st_classes$fill)){
        if(length(st_classes$fill)/2 != length(cl_vals)){
          warning(strwrap("- number of colors defined in st_classes$fill differ with the number of
                      potential classes to plot. Default NULL was taken.",
                          prefix = "\n", initial = ""), call. = FALSE)
          st_classes$fill <- c(rbind(cl_vals,
                                     hcl.colors(length(cl_vals), palette = st_classes$palette)))
        } else {
          ok <- T
          for(i in seq(1, length(st_classes$fill), 2)){
            if(!st_classes$fill[i] %in% orig_cl){
              warning(paste0("- in st_classes$fill: class '", st_classes$fill[i],
                             "' is not a defined class in 'x'. Default NULL was taken."),
                      call. = FALSE)
              ok <- F
              break
            }
          }
          if(!ok){
            st_classes$fill <- c(rbind(cl_vals,
                                       hcl.colors(length(cl_vals), palette = st_classes$palette)))
          }
        }
      } else {
        st_classes$fill <- c(rbind(cl_vals,
                                   hcl.colors(length(cl_vals), palette = st_classes$palette)))
      }

      for(i in seq(1, length(st_classes$fill), 2)){
        if(is.na(suppressWarnings(as.numeric(st_classes$fill[i])))){
          st_classes$fill[i] <- x@classes[x@classes$classname == st_classes$fill[i], "class"]
        }
      }

      if(any(duplicated(st_classes$fill[seq(1, length(st_classes$fill), 2)]))){
        warning("- in st_classes$fill: duplicated classes were found. Default NULL was taken.",
                call. = FALSE)
        st_classes$fill <- c(rbind(cl_vals,
                                   hcl.colors(length(cl_vals), palette = st_classes$palette)))
      }

      # check alpha parameters
      if(!is.null(st_classes$alpha)){
        if(length(st_classes$alpha)/2 != length(cl_vals)){
          warning(strwrap("- number of alpha values defined in st_classes$alpha differ with the number of
                      potential classes to plot. Default NULL was taken.",
                          prefix = "\n", initial = ""), call. = FALSE)
          st_classes$alpha <- c(rbind(cl_vals, rep(1, length(cl_vals))))
        } else {
          ok <- T
          for(i in seq(1, length(st_classes$alpha), 2)){
            if(!st_classes$alpha[i] %in% orig_cl){
              warning(paste0("- in st_classes$alpha: class '", st_classes$alpha[i],
                             "' is not a defined class in 'x'. Default NULL was taken."),
                      call. = FALSE)
              ok <- F
              break
            }
          }
          if(!ok){
            st_classes$alpha <- c(rbind(cl_vals, rep(1, length(cl_vals))))
          }
        }
      } else {
        st_classes$alpha <- c(rbind(cl_vals, rep(1, length(cl_vals))))
      }

      for(i in seq(1, length(st_classes$alpha), 2)){
        if(is.na(suppressWarnings(as.numeric(st_classes$alpha[i])))){
          st_classes$alpha[i] <- x@classes[x@classes$classname == st_classes$alpha[i], "class"]
        }
      }

      if(any(duplicated(st_classes$alpha[seq(1, length(st_classes$alpha), 2)]))){
        warning("- in st_classes$fill: duplicated classes were found. Default NULL was taken.",
                call. = FALSE)
        st_classes$alpha <- c(rbind(cl_vals, rep(1, length(cl_vals))))
      }
    } else {
      if(any(length(st_ext) != 2, !is.character(st_ext))){
        messages <- append(messages, "- argument 'st_ext' must be a character vector with the name
        of two colors. Default options were taken.")
        what     <- append(what, 1)
        st_ext <- c("chartreuse", "firebrick1")
      }
    }
  }

  errors   <- messages[which(what == 2)]
  warnings <- messages[which(what == 1)]

  out <- list(warnings = warnings,
              errors = errors,
              arg_name = arg_name,
              raster = raster,
              points = points,
              title = title,
              ncol = ncol,
              nrow = nrow,
              st_points = st_points,
              st_buffers = st_buffers,
              st_classes = st_classes,
              st_ext = st_ext)
  return(out)
}
