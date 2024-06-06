.mland_export_gis_chk <- function(){

  messages <- NULL
  what     <- NULL

  # Points
  chk_points <- .chk_points(messages, what, x, points)
  messages <- chk_points[[1]]
  what     <- chk_points[[2]]
  points   <- chk_points[[3]]

  if(is.null(raster) & is.null(ext_raster)){
    raster      <- 1:x@n_layers
    ext_raster  <- 1:length(x@landscapes$ext_rasters)
  } else {
    # Raster layers
    if(!is.null(raster)){
      if(is.character(raster)){
        if(!all(raster %in% x@rast_names[[1]]$name)){
          messages <- append(messages,
                             "- in argument 'raster': required raster layers were not found in
                           'x'. Mispelled?")
          what     <- append(what, 2)
        } else {
          raster <- sort(x@rast_names[[1]][x@rast_names[[1]]$name %in% raster, "rasterlayer"])
        }
      } else {
        if(!all(is.positive.wholenumber(raster))){
          messages <- append(messages, "- argument 'raster' must be a vector of positive
                             wholenumbers or strings.")
          what     <- append(what, 2)
        } else {
          if(!all(raster %in% 1:x@n_layers)){
            messages <- append(messages,"- in argument 'raster': one or more required raster layer
            were not found in x. Mispelled?")
            what     <- append(what, 2)
          }
        }
      }
    }
    # Extra rasters
    if(!is.null(ext_raster)){
      if(is.character(ext_raster)){
        if(!all(ext_raster %in% x@rast_names[[2]]$name)){
          messages <- append(messages,
                             "- in argument 'ext_raster': required raster layers were not found in
                           'x'. Mispelled?")
          what     <- append(what, 2)
        } else {
          ext_raster <- sort(x@rast_names[[2]][x@rast_names[[2]]$name %in% ext_raster,
                                                "rasterlayer"])
        }
      } else {
        if(!all(is.positive.wholenumber(ext_raster))){
          messages <- append(messages, "- argument 'ext_raster' must be a vector of positive wholenumbers.")
          what     <- append(what, 2)
        } else {
          if(!all(ext_raster %in% 1:max(x@rast_names[[2]]$rasterlayer))){
            messages <- append(messages,"- in argument 'ext_raster': one or more required rasterlayer was not found in x.
                           Mispelled?")
            what     <- append(what, 2)
          }
        }
      }
    }
  }

  # Radii
  chk_radii <- .chk_radii(messages, what, x, radii)
  messages <- chk_radii[[1]]
  what     <- chk_radii[[2]]

  errors   <- messages[which(what == 2)]
  warnings <- messages[which(what == 1)]

  out <- list(warnings = warnings,
              errors = errors,
              points = points,
              raster = raster,
              ext_raster = ext_raster)

  return(out)
}

#' Exports a 'MultiLand' object as GIS data
#'
#' Exports points, buffers and intersections between buffers and raster layers, as vector and raster
#' files.
#'
#' @param x  An object of class 'MultiLand' generated with [mland()].
#' @param points Numeric or character vector of points to be processed. See Details.
#' @param radii Numeric vector of radii to be processed.
#' @param name Character. Name of the zip file where files will be exported.
#' @param raster,ext_raster Numeric. The raster layers to be exported.
#' @param gdal GeoTiff creation options for rasters (\href{https://gdal.org/drivers/raster/gtiff.html}{GeoTiff file format}).
#' [mland_export_gis()] uses the following compression options:
#' c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9").
#' @param ... Other arguments passed to [terra::writeRaster].
#'
#' @details
#' If argument `points` is a character vector,
#' [mland_export_gis()] will assume that the 'MultiLand' object inputted in argument `x` was created with
#' `site_ref = TRUE`. This is, there is an attribute in points layer data with the names for
#' each individual point. Therefore, the inputted values in argument `points` will be taken as these
#' identification names. Otherwise, if a numeric vector is declared, the inputted values
#' will be taken as the automatically generated point ids (created when running [mland()]).
#'
#' @return GIS data from a 'MultiLand' object is exported through a zip file.
#'
#' @seealso [mland()], [mland_save()], [mland_load()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Loads a 'MultiLand' object
#' ernesdesign <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
#' ernesdesign <- mland_load(ernesdesign)
#'
#' # Exports as GIS data
#' mland_export_gis(ernesdesign, dir = "ernesdesign")
#' }
mland_export_gis <- function(x,
                             raster = NULL,
                             points = NULL,
                             radii = NULL,
                             ext_raster = NULL,
                             name = NULL,
                             gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9"),
                             ...){

  # Check arguments
  if(!is(x, "MultiLand")){
    stop("- argument 'x' must be an object of class 'MultiLand'.")
  }
  environment(.mland_export_gis_chk) <- environment()
  chk <- .mland_export_gis_chk()
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

  if(!is.null(name)){
    if(!is.character(name))
      stop("- if not NULL, argument 'name' must be a string with the name of the file.")
  } else {
    name <- keygen()
    name <- paste0("mland-GIS_", name)
  }

  if(file.exists(paste0(name, ".zip")))
    stop("name: a file with the same name for the zip file already exist. Please choose another one.")

  df_reference <- x@l_ref

  # if points and/or radii are null, take all of points and radii defined in x
  if(is.null(points)){
    points <- 1:length(x@points)
  } else { points <- as.numeric(points) }
  if(is.null(radii)) radii <- x@radii

  df_reference <- df_reference[df_reference$point_id %in% points &
                                 df_reference$radius %in% radii, ]

  points <- sort(unique(df_reference$point_id))
  radii  <- sort(unique(df_reference$radius))


  dir.create(tmp <- tempfile())
  dir.create(file.path(tmp, "MultiLand-GIS_data"))
  dir.create(file.path(tmp, "MultiLand-GIS_data", "rasters"))

  # Export points
  suppressWarnings(terra::writeVector(x@points[x@points$id %in% points, ],
                                      file.path(tmp, "MultiLand-GIS_data", "points.gpkg"),
                                      options = NULL))

  # Export buffers
  suppressWarnings(terra::writeVector(x@buffers[df_reference$row_id, ],
                                      file.path(tmp, "MultiLand-GIS_data", "buffers.gpkg"),
                                      options = NULL))

  raster_ids <- df_reference[df_reference$radius %in% max(radii), "row_id"]

 # Export intersections
  if(!x@on_the_fly){
    if(!is.null(raster)){
      for(i in 1:length(unique(raster))){
        int_tmp <- mmerge(x@landscapes$lsm_rasters[[raster[i]]][raster_ids])
        terra::writeRaster(int_tmp, file.path(tmp, "MultiLand-GIS_data", "rasters",
                                              paste0("lsm_raster_", raster[i], ".tif")),
                           gdal = gdal, ...)
      }
    }
    if(!is.null(ext_raster)){
      for(i in 1:length(unique(ext_raster))){
        int_tmp <- mmerge(x@landscapes$ext_rasters[[ext_raster[i]]][raster_ids])
        terra::writeRaster(int_tmp, file.path(tmp, "MultiLand-GIS_data", "rasters",
                                              paste0("ext_raster_", raster[i], ".tif")),
                           gdal = gdal, ...)
      }
    }
  } else {
    message("- in 'x': on_the_fly = TRUE. No intersections were exported.")
  }

  cat(strwrap("MultiLand-GIS_data\n\n
              This directory was created by R package multilandr.\n
              GIS data was generated while exporting an object of class 'MultiLand'.",
              prefix = "\n", initial = ""), file = file.path(tmp, "MultiLand-GIS_data", "README.txt"))

  # Generates zip file
  last_wd <- getwd()
  zipfile <- paste0(last_wd, "/", name, ".zip")
  setwd(tmp)
  utils::zip(zipfile, ".")
  setwd(last_wd)
  unlink(file.path(tmp), recursive = T)
  message(paste0("GIS data was successfully exported within file '", name, ".zip'."))
}
