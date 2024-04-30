save_rasts <- function(tmp, rast_dir, x_obj, gdal, ...){
  if(length(x_obj) > 0){
    for(i in 1:length(x_obj)){
      r_dir <- paste0("raster_", i)
      dir.create(file.path(tmp, "MultiLand", rast_dir[1], rast_dir[2], r_dir))
      if(is.list(x_obj[[i]])){
        r_names <- as.list(names(x_obj[[i]]))
        for(j in 1:length(x_obj[[i]])){
          terra::writeRaster(x_obj[[i]][[j]], file.path(tmp, "MultiLand",
                                                        rast_dir[1], rast_dir[2], r_dir,
                                                        paste0(r_names[j], ".tif")),
                             gdal = gdal, ...)
        }
      } else {
        terra::writeRaster(x_obj[[i]], file.path(tmp, "MultiLand",
                                                      rast_dir[1], rast_dir[2], r_dir,
                                                      paste0("/raster_", i, ".tif")),
                           gdal = gdal, ...)
      }
    }
  }
}

#' Saves a 'MultiLand' or 'MultiLandMetrics' object
#'
#' Exports an object of class 'MultiLand' to be read in the future with [mland_load()], or
#' an object of class 'MultiLandMetrics' as if it was saved with [saveRDS()].
#'
#' @param x Object of class 'MultiLand' or 'MultiLandMetrics'.
#' @param name If `x` is an object of class 'MultiLand', the name of the zip file where
#' files will be saved (without the '.zip'). If `x` is an object of class 'MultiLandMetrics',  the name of the R file (.rds). If NULL (default),
#' the name will be 'mland_' or 'mlandmetrics_' + a large random number.
#' @param dir The directory where to save the file, without the '/' at the beginning/ending of the string.
#' @param gdal GDAL driver specific datasource creation options. See the GDAL documentation. With the
#' \href{https://gdal.org/drivers/raster/gtiff.html}{GeoTiff file format}, [mland_save()] uses the
#' following compression options: c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9"). Only relevant
#' if `x` is an object of class 'MultiLand'.
#' @param ... If `x` is an object of class 'MultiLand', `...` should depict other arguments passed to
#' [terra::writeRaster], the function to write rasterlayers (from intersections and plain rasterlayers).
#' Otherwise, if `x` is an object of class 'MultiLandMetrics', `...` should depict other arguments passed
#' to [save()]. See Details.
#' @details 'MultiLand' objects should be exported with this function rather than exporting as an
#' external representation of R objects with [saveRDS()]. This is because objects of classes
#' 'SpatVector' and 'SpatRaster' (from package terra) contained inside a 'MultiLand'
#' object cannot be exported as regular R objects. The exported object will be a zip file,
#' and can be loaded again into an R session with [mland_load()].
#'
#' Relevant arguments can be passed to the function [terra::writeRaster], which is used to write
#' rasterlayers from a 'MultiLand' object. Particularly, in the argument `gdal` one can specify
#' relevant options regarding raster compression. This may reduce raster sizes significantly. Definition
#' of some other arguments inside [terra::writeRaster] may affect exportation of rasterlayer
#' objects, in the context of a 'MultiLand' object.
#'
#' Objects of class 'MultiLandMetrics', instead, do not contain 'SpatVector' or 'SpatRaster' objects
#' and can be exported as regular R objects with [saveRDS()]. The user may use [saveRDS()] or
#' [mland_save()], and the outcome will be identical.
#'
#' @return If `x` is an object of class 'MultiLand', a zip file or a directory containing all
#' information regarding the 'MultiLand' object provided in 'x'. Otherwise, if `x` is an object of
#' class 'MultiLandMetrics', the function will export the R object as if it was exported as a
#' regular R object with [saveRDS()].
#' @export
#'
#' @seealso [mland_load()], [mland()], [mland_metrics()]
#'
#' @examples
#' \dontrun{
#' # Load MultiLand object
#' mland_obj <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
#' ernesdesign2 <- mland_load(mland_obj)
#'
#' # Save it again
#' mland_save(ernesdesign2)
#'
#' # Save it again but defining a higher compression for rasterlayers
#' mland_save(ernesdesign2, gdal = "COMPRESS=DEFLATE")
#'
#' # Loads a MultiLandMetrics object previously generated with mland_metrics()
#' mlm_obj <- system.file("extdata", "ed_metrics.rds", package = "multilandr")
#' ed_metrics2 <- mland_save(mlm_obj)
#'
#' # Save it again. In this case, mland_save() is the same as using saveRDS()
#' mland_save(ed_metrics2)
#' }
mland_save <- function(x, name = NULL, dir = getwd(),
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9"), ...){

  if(!is(x, "MultiLand") & !is(x, "MultiLandMetrics"))
    stop("- argument 'x' must be an object of class 'MultiLand' or 'MultiLandMetrics'.")

  if(!is.null(name)){
    if(!is.character(name))
      stop("- if not NULL, argument 'name' must be a string with the name of the file.")
  } else {
    name <- keygen()
    if(is(x, "MultiLandMetrics")){
      name <- paste0("mlandmetrics_", name)
    } else {
      name <- paste0("mland_", name)
    }
  }

  if(!dir.exists(dir))
    stop("- directory provided in 'dir' does not exist.")

  if(is(x, "MultiLandMetrics")){
    if(!substr(name, (nchar(name) + 1) - 4, nchar(name)) == ".rds"){
      name <- paste0(name, ".rds")
    }
    saveRDS(x, file = name, ...)
    return(invisible())
  }

  if(file.exists(paste0(name, ".zip")))
    stop("name: a file with the same name for the zip file already exist. Please choose another one.")

  dir.create(tmp <- tempfile())
  dir.create(file.path(tmp, "MultiLand"))
  dir.create(file.path(tmp, "MultiLand", "points"))
  dir.create(file.path(tmp, "MultiLand", "buffers"))
  dir.create(file.path(tmp, "MultiLand", "/landscapes/lsm_rasters/"), recursive = T)
  dir.create(file.path(tmp, "MultiLand", "/landscapes/ext_rasters/"), recursive = T)

  # Save points and buffers
  terra::writeVector(x@points, file.path(tmp, "MultiLand", "points", "points.shp"))
  terra::writeVector(x@buffers, file.path(tmp, "MultiLand", "buffers", "buffers.shp"))

  # Save landscapes of main and rasters
  save_rasts(tmp, c("landscapes", "lsm_rasters"), x@landscapes$lsm_rasters, gdal, ...)
  save_rasts(tmp, c("landscapes", "ext_rasters"), x@landscapes$ext_rasters, gdal, ...)

  # Save object info
  info <- x
  info@points <- info@buffers <- terra::vect()
  if(length(info@landscapes$lsm_rasters) > 0){
    for(i in 1:length(info@landscapes$lsm_rasters)){
      info@landscapes$lsm_rasters[[i]] <- lapply(info@landscapes$lsm_rasters[[i]], function(x) NA)
    }
  }
  if(length(info@landscapes$ext_rasters) > 0){
    for(i in 1:length(info@landscapes$ext_rasters)){
      info@landscapes$ext_rasters[[i]] <- lapply(info@landscapes$ext_rasters[[i]], function(x) NA)
    }
  }
  saveRDS(info, file = file.path(tmp, "MultiLand", "info.rds"))

  cat(strwrap("MultiLand\n\n
              This directory was created by R package multilandr.\n
              No modifications of folders or files should be made by hand.\n
              This folder may be loaded into an R session as a 'MultiLand' object with function
              mland_load().",
              prefix = "\n", initial = ""), file = file.path(tmp, "MultiLand", "README.txt"))

  # Generates zip file
  last_wd <- getwd()
  zipfile <- paste0(last_wd, "/", dir, "/", name, ".zip")
  setwd(tmp)
  utils::zip(zipfile, ".")
  setwd(last_wd)
  unlink(file.path(tmp), recursive = T)
  message(paste0("'MultiLand' object successfully exported as '", name, ".zip'."))
}
