#' Check input raster
#'
#' Checks the validity of raster layers to be inputted in [mland()], intended to represent land cover. The function directly calls
#' [landscapemetrics::check_landscape()].
#'
#' @param raster An object of class 'RasterLayer', 'RasterStack', 'RasterBrick', 'SpatRaster',
#' or a list of raster objects (one of 'RasterLayer' or 'SpatRaster').
#' @param verbose Print warning messages.
#'
#' @details Extracts basic information about the inputted raster: coordinate reference system (crs) - either "geographic",
#' "projected", or NA, units of the
#' coordinate reference system, class for the values of the inputted raster and the number of classes
#' found in the raster.
#'
#' @export
check_raster <- function(raster,
                         verbose = T){
  landscapemetrics::check_landscape(raster, verbose)
}
