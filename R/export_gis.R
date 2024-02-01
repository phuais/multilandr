export_gis_chk <- function(){

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
    # Rasterlayers
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
        if(!all(is.positive.wholenumber(raster))){
          messages <- append(messages, "- argument 'raster' must be a vector of positive
                             wholenumbers or strings.")
          what     <- append(what, 2)
        } else {
          if(!all(raster %in% 1:x@n_layers)){
            messages <- append(messages,"- in argument 'raster': one or more required rasterlayer
            were not found in x. Mispelled?")
            what     <- append(what, 2)
          }
        }
      }
    }
    # Extra rasters
    if(!is.null(ext_raster)){
      if(is.character(ext_raster)){
        if(!all(ext_raster %in% x@layer_names[[2]]$name)){
          messages <- append(messages,
                             "- in argument 'ext_raster': required rasterlayers were not found in
                           'x'. Mispelled?")
          what     <- append(what, 2)
        } else {
          ext_raster <- sort(x@layer_names[[2]][x@layer_names[[2]]$name %in% ext_raster,
                                                "rasterlayer"])
        }
      } else {
        if(!all(is.positive.wholenumber(ext_raster))){
          messages <- append(messages, "- argument 'ext_raster' must be a vector of positive wholenumbers.")
          what     <- append(what, 2)
        } else {
          if(!all(ext_raster %in% 1:max(x@layer_names[[2]]$rasterlayer))){
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
#' Exports points, buffers and intersections between buffers and rasterlayers, as shapefiles and raster
#' files.
#'
#' @param x  An object of class 'MultiLand' generated with [mland()].
#' @param points Numeric or character vector of points to be processed. See Details.
#' @param radii Numeric vector of radii to be processed.
#' @param dir Character. Name of the target directory where files will be exported. Provide a
#' string with an existing folder, otherwise the function will create it.
#' @param raster,ext_raster Numeric. The rasterlayers to be exported.
#' @param filenames Character vector with the root of the file names to be exported, for
#' points, buffers, main rasterlayers and extra rasters, respectively. Default is
#' c("points", "buffers", "lsm_raster", "ext_raster").
#' @param overwrite Logical. Whether to overwrite (TRUE) or not (FALSE, default) existing files
#' with the same names.
#' @param filetype Character. File format expressed as GDAL driver names
#' (\href{https://gdal.org/drivers/vector/index.html}{Vector Drivers},
#' \href{https://gdal.org/drivers/raster/index.html}{Raster drivers}). Default are
#' c("ESRI Shapefile", "GTiff") for points/rasters and raster intersections, respectively.
#' @param gdal GDAL driver specific datasource creation options. See the GDAL documentation. With the
#' \href{https://gdal.org/drivers/raster/gtiff.html}{GeoTiff file format}, [export_gis()] uses the
#' following compression options: c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9").
#' @param ... Other arguments passed to [terra::writeRaster].
#'
#' @details
#' If argument `points` is a character vector,
#' [export_gis()] will assume that the 'MultiLand' object inputted in argument `x` was created with
#' `site_ref = TRUE`. This is, there is an attribute in points layer data with the names for
#' each individual point. Therefore, the inputted values in argument `points` will be taken as these
#' identification names. Otherwise, if a numeric vector is declared, the inputted values
#' will be taken as the automatically generated point ids (created when running [mland()]).
#'
#' @return No return of an R object. GIS data from a 'MultiLand' object is exported.
#'
#' @seealso [mland()]
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Loads a 'MultiLand' object
#' ernesdesign <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
#' ernesdesign <- load_mland(ernesdesign)
#'
#' # Exports as GIS data
#' export_gis(ernesdesign, dir = "ernesdesign")
#' }
export_gis <- function(x, raster = NULL, points = NULL, radii = NULL, ext_raster = NULL,
                       dir = getwd(), filenames = c("points", "buffers", "lsm_raster", "ext_raster"),
                       overwrite = FALSE, filetype = c("ESRI Shapefile", "GTiff"),
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9"), ...){

  # Check arguments
  if(!is(x, "MultiLand")){
    stop("- argument 'x' must be an object of class 'MultiLand'.")
  }
  environment(export_gis_chk) <- environment()
  chk <- export_gis_chk()
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

  if(!dir.exists(dir)) dir.create(dir, recursive = T)

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

  # Export points
  terra::writeVector(x@points[x@points$id %in% points, ],
                     paste0(dir, "/", filenames[1]), overwrite = overwrite, filetype = filetype[1])

  # Export buffers
  terra::writeVector(x@buffers[df_reference$row_id, ],
                     paste0(dir, "/", filenames[2]), overwrite = overwrite,
                     filetype = filetype[1])

  raster_ids <- df_reference[df_reference$radius %in% max(radii), "row_id"]

  # Export intersections
  if(!x@onthefly){
    dir.create(paste0(dir, "/rasters/"), recursive = T)
    if(!is.null(raster)){
      for(i in 1:length(unique(raster))){
        int_tmp <- mmerge(x@landscapes$lsm_rasters[[raster[i]]][raster_ids])
        terra::writeRaster(int_tmp, paste0(dir, paste0("/rasters/", filenames[3], "_", raster[i])),
                            filetype = filetype[2], overwrite = overwrite, gdal = gdal, ...)
      }
    }
    if(!is.null(ext_raster)){
      for(i in 1:length(unique(ext_raster))){
        int_tmp <- mmerge(x@landscapes$ext_rasters[[ext_raster[i]]][raster_ids])
        terra::writeRaster(int_tmp, paste0(dir, paste0("/rasters/", filenames[4], "_", ext_raster[i])),
                            filetype = filetype[2], overwrite = overwrite, gdal = gdal, ...)
      }
    }
  } else {
    message("- in 'x': onthefly = TRUE. No intersections were exported.")
  }
}
