#' Load 'MultiLand' or 'MultiLandMetrics' object
#'
#' Imports a zip file into an object of class 'MultiLand' that was previously
#' saved with [mland_save()]. Alternatively, loads to the environment an RDS object depicting a
#' 'MultiLandMetrics' object.
#'
#' @param path A string depicting the path to a zip file, to load objects of class
#' 'MultiLand', or to a RDS file to load objects of class 'MultiLandMetrics'.
#' @param ... Other parameters passed to [readRDS()] when trying to load an object of class
#' 'MultiLandMetrics'.
#'
#' @return A 'MultiLand' or a 'MultiLandMetrics' object.
#'
#' @seealso [mland_save()], [mland()], [mland_metrics()]
#'
#' @export
#'
#' @examples
#' # Loads mland object from a zip file, previously created with mland_save()
#' mland_obj <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
#' ernesdesign <- mland_load(mland_obj)
#'
#' # Loads a MultiLandMetrics object previously generated with mland_metrics() and
#' # exported as a RDS object with mland_save() or saveRDS()
#'
#' mlm_obj <- system.file("extdata", "ed_metrics.rds", package = "multilandr")
#' ed_metrics <- mland_load(mlm_obj)
mland_load <- function(path,
                       ...){
  if(is.character(path)){
    if(substr(path, (nchar(path) + 1) - 4, nchar(path)) == ".zip"){
      if(!file.exists(path))
        stop("Could not find required zip file.")
      tmp <- tempdir()
      objs <- utils::unzip(path, exdir = tmp)
    } else {
      if(substr(path, (nchar(path) + 1) - 4, nchar(path)) == tolower(".rds")){
        if(!file.exists(path))
          stop("Could not find required .rds file.")

        out <- readRDS(file = path, ...)
        if(!is(out, "MultiLandMetrics")){
          stop("The provided file is not an object of class 'MultiLandMetrics'.")
        }
        return(out)
      } else {
        stop("- argument 'path' must be a string with the path to a zip file (for 'MultiLand' objects),
         or to a RDS file (for 'MultiLandMetrics' objects).")
      }
      objs <- paste0(path, "/", list.files(path))
    }
  } else {
    stop("- argument 'path' must be a string with the path to a zip file (for 'MultiLand' objects),
         or to a RDS file (for 'MultiLandMetrics' objects).")
  }

  if(!all(any(grepl("info.rds", objs)),
          any(grepl("/buffers/", objs)),
          any(grepl("/points/", objs)),
          any(grepl("README.txt", objs)),
          any(grepl("/landscapes/", objs)))){
    stop("- directory content is corrupted or was not generated via mland_save().")
  }

  # Loads mland info
  info <- readRDS(grep("info.rds", objs, value = T))

  # Loads points and buffers
  info@points <- terra::vect(grep("points.gpkg", objs, value = T))
  info@buffers <- terra::vect(grep("buffers.gpkg", objs, value = T))

  ff <- function(x, objs){
    terra::rast(grep(x, objs, value = T))
  }

  # Loads main rasters
  if(length(info@landscapes$lsm_rasters) > 0){
    for(i in 1:info@n_layers){
      if(!info@on_the_fly){
        names <- as.list(paste0("/", names(info@landscapes$lsm_rasters[[i]])))
        info@landscapes$lsm_rasters[[i]] <- lapply(names, ff, objs)
        names(info@landscapes$lsm_rasters[[i]]) <- substring(names, 2)
      } else {
        info@landscapes$lsm_rasters[[i]] <- terra::rast(grep(paste0("landscapes/lsm_rasters/raster_", i),
                                                        objs, value = T))
      }
    }
  }

  # Loads extra rasters
  if(length(info@landscapes$ext_rasters) > 0){
    for(i in 1:length(info@landscapes$ext_rasters)){
      if(!info@on_the_fly){
        names <- as.list(names(info@landscapes$ext_rasters[[i]]))
        info@landscapes$ext_rasters[[i]] <- lapply(names, ff, objs)
        names(info@landscapes$ext_rasters[[i]]) <- names
      } else {
        info@landscapes$ext_rasters[[i]] <- terra::rast(grep(paste0("landscapes/ext_rasters/raster_", i),
                                               objs, value = T))
      }
    }
  }

  unlink(gsub("info.RDS", "", grep("info.RDS", objs, value = T)))
  message("'MultiLand' object was loaded successfully.")
  return(info)
}
