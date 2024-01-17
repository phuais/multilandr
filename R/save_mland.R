save_rasts <- function(dirpath, rast_dir, x_obj, gdal, ...){
  if(length(x_obj) > 0){
    for(i in 1:length(x_obj)){
      r_dir <- paste0("raster_", i)
      dir.create(paste0(dirpath, rast_dir, r_dir))
      if(is.list(x_obj[[i]])){
        r_names <- as.list(names(x_obj[[i]]))
        for(j in 1:length(x_obj[[i]])){
          terra::writeRaster(x_obj[[i]][[j]], paste0(dirpath, rast_dir, r_dir, "/",
                                                     r_names[j], ".tif"),
                             gdal = gdal, ...)
        }
      } else {
        terra::writeRaster(x_obj[[i]], paste0(dirpath, rast_dir, r_dir, "/raster_", i, ".tif"),
                           gdal = gdal, ...)
      }
    }
  }
}

#' Saves a 'MultiLand' or 'MultiLandMetrics' object
#'
#' Exports an object of class 'MultiLand' to be read in the future with [load_mland()], or
#' an object of class 'MultiLandMetrics' as if it was saved with [saveRDS()].
#'
#' @param x Object of class 'MultiLand' or 'MultiLandMetrics'.
#' @param name If `x` is an object of class 'MultiLand', the name of the zip file or directory where
#' files will be saved, or the name of the R file if `x` is an object of class 'MultiLandMetrics'. If NULL (default),
#' the name will be 'mland_' or 'mlandmetrics_' + a large random number.
#' @param zip If TRUE (default and recommended), the object will be exported as a zip file. Otherwise, as a
#' directory. Only relevant if `x` is an object of class 'MultiLand'.
#' @param gdal GDAL driver specific datasource creation options. See the GDAL documentation. With the
#' \href{https://gdal.org/drivers/raster/gtiff.html}{GeoTiff file format}, [save_mland()] uses the
#' following compression options: c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9"). Only relevant
#' if `x` is an object of class 'MultiLand'.
#' @param ... If `x` is an object of class 'MultiLand', `...` should depict other arguments passed to
#' [terra::writeRaster], the function to write rasterlayers (from intersections and plain rasterlayers).
#' Otherwise, if `x` is an object of class 'MultiLandMetrics', `...` should depict other arguments passed
#' to [save()]. See Details.
#' @details 'MultiLand' objects should be exported with this function rather than exporting as an
#' externar representation of R objects with [saveRDS()]. This is because objects of classes
#' 'SpatVector' and 'SpatRaster' (from package terra) contained inside a 'MultiLand'
#' object cannot be exported as regular R objects. The exported object will be a zip file or a folder,
#' and can be loaded again into an R session with [load_mland()].
#'
#' Relevant arguments can be passed to the function [terra::writeRaster], which is used to write
#' rasterlayers from a 'MultiLand' object. Particularly, in the argument `gdal` one can specify
#' relevant options regarding raster compression. This may reduce raster sizes significantly. Definition
#' of some other arguments inside [terra::writeRaster] may affect exportation of rasterlayer
#' objects, in the context of a 'MultiLand' object.
#'
#' Objects of class 'MultiLandMetrics', instead, do not contain 'SpatVector' or 'SpatRaster' objects
#' and can be exported as regular R objects with [saveRDS()]. The user may use [saveRDS()] or
#' [save_mland()], and the outcome will be identical.
#'
#' @return If `x` is an object of class 'MultiLand', a zip file or a directory containing all
#' information regarding the 'MultiLand' object provided in 'x'. Otherwise, if `x` is an object of
#' class 'MultiLandMetrics', the function will export the R object as if it was exported as a
#' regular R object with [saveRDS()].
#' @export
#'
#' @seealso [load_mland()], [mland()], [metrics()]
#'
#' @examples
#' \dontrun{
#' # Load MultiLand object
#' mland_obj <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
#' ernesdesign2 <- load_mland(mland_obj)
#'
#' # Save it again
#' save_mland(ernesdesign2)
#'
#' # Save it again but defining a higher compression for rasterlayers
#' save_mland(ernesdesign2, gdal = "COMPRESS=DEFLATE")
#'
#'
#' # Loads a MultiLandMetrics object previously generated with metrics()
#' mlm_obj <- system.file("extdata", "ed_metrics.rds", package = "multilandr")
#' ed_metrics2 <- load_mland(mlm_obj)
#'
#' # Save it again. In this case, save_mland() is the same as using saveRDS()
#' save_mland(ed_metrics2)
#' }
save_mland <- function(x, name = NULL, zip = TRUE,
                       gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2", "ZLEVEL=9"), ...){

  if(!is(x, "MultiLand") & !is(x, "MultiLandMetrics"))
    stop("- argument 'x' must be an object of class 'MultiLand' or 'MultiLandMetrics'.")

  if(!is.null(name)){
    if(!is.character(name))
      stop("- if not NULL, argument 'name' must be string depicting the name of the directory.")
  } else {
    name <- keygen()
    if(is(x, "MultiLandMetrics")){
      name <- paste0("mlandmetrics_", name)
    } else {
      name <- paste0("mland_", name)
    }
  }

  if(is(x, "MultiLandMetrics")){
    if(!substr(name, (nchar(name) + 1) - 4, nchar(name)) == ".rds"){
      name <- paste0(name, ".rds")
    }
    saveRDS(x, file = name, ...)
    return(invisible())
  }

  if(!is.logical(zip))
    stop("- argument 'zip' must be logical.")

  if(!zip){
    if(dir.exists(name))
      stop("name: specified directory name already exist. Please choose another one.")

    dirpath <- name
  } else {
    if(file.exists(paste0(name, ".zip")))
      stop("name: a file with the same name for the zip file already exist. Please choose another one.")

    dirpath <- paste0(tempdir(), "/", name)
  }

  dir.create(dirpath)
  dir.create(paste0(dirpath, "/points"))
  dir.create(paste0(dirpath, "/buffers"))
  dir.create(paste0(dirpath, "/landscapes/lsm_rasters/"), recursive = T)
  dir.create(paste0(dirpath, "/landscapes/ext_rasters/"), recursive = T)

  # Save points and buffers
  terra::writeVector(x@points, paste0(dirpath, "/points/points.shp"))
  terra::writeVector(x@buffers, paste0(dirpath, "/buffers/buffers.shp"))

  # Save landscapes of main and rasters
  save_rasts(dirpath, "/landscapes/lsm_rasters/", x@landscapes$lsm_rasters, gdal, ...)
  save_rasts(dirpath, "/landscapes/ext_rasters/", x@landscapes$ext_rasters, gdal, ...)

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
  saveRDS(info, file = paste0(dirpath, "/info.rds"))

  cat(strwrap("MultiLand\n\n
              This directory was created by R package multilandr.\n
              No modifications of folders or files should be made by hand.\n
              This folder may be loaded into an R session as a 'MultiLand' object with function
              load_mland().",
              prefix = "\n", initial = ""), file = paste0(dirpath, "/README.txt"))

  if(zip){
    wrk_dir <- getwd()
    setwd(dirpath)
    zip(paste0(wrk_dir, "/", name), "./")
    setwd(wrk_dir)
    unlink(dirpath, recursive = T)
    message(paste0("'MultiLand' object successfully exported as '", name, ".zip'."))
  } else {
    message(paste0("'MultiLand' object successfully exported into directory '", name, "'."))
  }
}
