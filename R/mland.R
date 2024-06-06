# Generates points layer
.gen_pointlayers <- function(points_layer, site_ref){

  # Generates points layers
  if(class(points_layer)[1] %in% c("SpatVector", "SpatialPoints", "SpatialPointsDataFrame", "sf")){
    if(class(points_layer)[1] == "SpatVector"){
      if(terra::geomtype(points_layer) != "points"){
        stop("- points_layer layer must be an object of class 'SpatVector' (geom = 'points'), 'SpatialPointsDataFrame','SpatialPoints' or 'sf'.",
             call. = FALSE)
      } else {
        l_points <- points_layer
      }
    } else {
      l_points <- suppressWarnings(terra::vect(points_layer))
    }
  } else {
    l_points <- suppressWarnings(terra::vect(points_layer))
    if(class(l_points)[1] != "SpatVector" | terra::geomtype(l_points) != "points"){
      stop("- points_layer layer must be an object of class 'SpatVector' (geom = 'points'), 'SpatialPointsDataFrame', 'SpatialPoints' or 'sf'.",
           call. = FALSE)
    }
  }
  l_points$id <- 1:length(l_points)

  # Checks points layer crs
  chk <- .check_pointsCRS(l_points)
  if(chk != FALSE){ stop(chk) }

  # Checks site reference
  if(!is.null(site_ref)){
    chk <- .check_pointsref(l_points, site_ref)
    if(chk != 0){
      if(chk == 1){
        warning(strwrap(paste0("- could not find a column with the provided points reference \"",
                               site_ref, "\" in points_layer layer data. The argument was ignored."),
                        prefix = "\n", initial = ""), call. = FALSE)
        site_ref <- NULL
      } else {
        if(chk == 2){
          site_ref <- NULL
          warning(strwrap(paste0("- duplicated names for different points were found by the points
                                 reference \"", site_ref, "\". Points reference label was
                                 discarded."), prefix = "\n", initial = ""), call. = FALSE)
        } else {
          site_ref <- NULL
          warning(strwrap(paste0("- one or more NA were found for the label of one ore more point
          by the points reference \"", site_ref, "\". Points reference label was discarded."),
                          prefix = "\n", initial = ""), call. = FALSE)
        }
      }
    }
  }

  out <- list(l_points, site_ref)
  return(out)
}

# Generates layers
.generate_layers <- function(points_layer, site_ref, rast_layer,
                             ext_rast_layer, rad){

  pp       <- .gen_pointlayers(points_layer, site_ref)
  l_points <- pp[[1]]
  site_ref <- pp[[2]]
  ex_p <- terra::ext(l_points)
  chk_alr <- FALSE

  # Loads class layer values
  raster_values <- vector("list", length(rast_layer))
  if(length(rast_layer) > 0){
    for(i in 1:length(rast_layer)){
      cl <- rast_layer[[i]]
      chk <- .check_classCRS(l_points, cl, ex_p, rad)
      if(chk == 1)
        stop("- rasters CRS must be equal to points layer CRS.",  call. = FALSE)
      if(!chk_alr & chk == 2){
        warning("- extent of the provided points layer exceeds the extent of one or more provided raster layers.",  call. = FALSE)
        chk_alr <- TRUE
      }
      if(!chk_alr & chk == 3){
        warning("- the distance between the outer border of the provided points layer and outer border of one or more provided raster layers is lower than the maximum provided radius.",  call. = FALSE)
        chk_alr <- TRUE
      }
      # Gets layer classes
      raster_values[[i]] <- sort(terra::unique(cl)[, 1])
    }
  }

  # Check extra rasters
  if(length(ext_rast_layer) > 0){
    for(i in 1:length(ext_rast_layer)){
      cl <- ext_rast_layer[[i]]
      chk <- .check_classCRS(l_points, cl, ex_p, rad)
      if(chk == 1)
        stop("- rasters CRS must be equal to points layer CRS.",  call. = FALSE)
      if(!chk_alr & chk == 2){
        warning("- extent of the provided points layer exceeds the extent of one or more provided raster layers.",  call. = FALSE)
        chk_alr <- TRUE
      }
      if(!chk_alr & chk == 3){
        warning("- the distance between the outer border of the provided points layer and outer border of one or more provided raster layers is lower than the maximum provided radius.",  call. = FALSE)
        chk_alr <- TRUE
      }
    }
  }

  out <- list(raster_values, l_points, site_ref)
  return(out)
}

# Generates buffers
.buffering <- function(l_points, radii, bufftype, segs){

  # Generates buffers
  for(i in 1:length(l_points)){
    ind_point <- l_points[i, ]
    for(j in 1:length(radii)){
      buff <- terra::buffer(ind_point, width = radii[j], quadsegs = segs, capstyle = bufftype)
      if(i == 1 && j == 1){ l_buffers <- buff } else { l_buffers <- rbind(l_buffers, buff) }
    }
  }

  # Clean and finish buffers object
  l_buffers$id  <- rep(1:length(l_points), each = length(radii))
  l_buffers$radius  <- rep(radii, length(l_points))

  out <- l_buffers
  return(out)
}

# Generates intersections
.intersecting <- function(l_rasters, l_buffers, classes, ext_rast_layer, on_the_fly){

  # Intersections with main rasters
  intersects <- vector("list", length(l_rasters))
  n_classes <- vector("numeric", length(l_rasters))
  if(length(l_rasters) > 0){
    for(i in 1:length(intersects)){
      n_classes[i] <- length(classes[[i]])
      if(!on_the_fly){
        intersects[[i]] <- vector("list", length(l_buffers))
        for(j in 1:length(l_buffers)){
          clip <- suppressWarnings(tryCatch(terra::crop(l_rasters[[i]],
                                                         terra::ext(l_buffers[j, ])), error = c))
          if(!is.list(clip)){
            intersects[[i]][[j]] <- terra::mask(clip, l_buffers[j, ])
          } else {
            empty_raster <- terra::rast(nrows = 1, ncols = 1, crs = terra::crs(l_buffers[j, ]),
                                        ext = l_buffers[j, ],
                                        resolution = terra::res(l_rasters[[i]]), vals = NA)
            intersects[[i]][[j]] <- empty_raster
          }
        }

        names(intersects[[i]]) <- paste0("RasterLayer", i, "-", terra::values(l_buffers)$id, "-",
                                         terra::values(l_buffers)$radius)
      } else {
        intersects[[i]] <- l_rasters[[i]]
        names(intersects)[[i]] <- paste0("RasterLayer ", i)
      }
    }
  }

  # Intersections with extra rasters
  intersects_ext <- vector("list", length(ext_rast_layer))
  if(length(ext_rast_layer) > 0){
    for(i in 1:length(intersects_ext)){
      if(!on_the_fly){
        intersects_ext[[i]] <- vector("list", length(l_buffers))
        for(j in 1:length(l_buffers)){
          clip <- suppressWarnings(tryCatch(terra::crop(ext_rast_layer[[i]],
                                                        terra::ext(l_buffers[j, ])), error = c))
          if(!is.list(clip)){
            intersects_ext[[i]][[j]] <- terra::mask(clip, l_buffers[j, ])
          } else {
            empty_raster <- terra::rast(nrows = 1, ncols = 1, crs = terra::crs(l_buffers[j, ]),
                                        ext = l_buffers[j, ],
                                        resolution = terra::res(ext_rast_layer[[i]]), vals = NA)
            intersects_ext[[i]][[j]] <- empty_raster
          }
        }

        names(intersects_ext[[i]]) <- paste0("ExtRasterLayer", i, "-", terra::values(l_buffers)$id, "-",
                                             terra::values(l_buffers)$radius)
      } else {
        intersects_ext[[i]] <- ext_rast_layer[[i]]
        names(intersects_ext)[[i]] <- paste0("ExtRasterLayer ", i)
      }
    }
  }

  out <- list(intersects, n_classes, intersects_ext)
  return(out)
}

# Generates labels
.labeling <- function(class_names, classes, n_classes){

  if(length(class_names) > 0){
    if(length(class_names) != length(n_classes)){
      warning(strwrap("- number of elements of argument 'class_names' differ with the number of raster
                      layers. Class names were discarded.", prefix = "\n", initial = ""),
              call. = FALSE)

      class_names <- vector("list")
    } else {
      for(i in 1:length(classes)){
        if(length(class_names[[i]])/2 != n_classes[i]){
          warning(strwrap(paste0("- number of labels defined in argument 'class_names' differ with the number of
                                 classes of rasterlayer ", i , ". Class names were discarded."),
                          prefix = "\n", initial = ""),
                  call. = FALSE)
          class_names <- vector("list")
          break
        }
        if(!all(as.numeric(class_names[[i]][seq(1, length(class_names[[i]]), 2)]) %in% classes[[i]])){
          warning(paste0("- one or more classes were not found as classes in rasterlayer ", i ,
                         ". Class names were discarded."), call. = FALSE)
          class_names <- vector("list")
          break
        }
      }
    }
  }

  if(length(class_names) > 0){
    # Change - for _ in class_names
    if(length(n_classes) > 0){
      for(i in 1:length(classes)){
        pos <- seq(2, length(class_names[[i]]), 2)
        if(any(grepl("-", class_names[[i]][pos]))){
          class_names[[i]][pos] <- gsub("-", "_", class_names[[i]][pos])
          warning("Pattern \"-\" in class names was replaced by \"_\".", call. = FALSE)
        }
      }
    }
  }
  return(class_names)
}

# Raster labeling
rast_labels <- function(rast_names, rast_layer, ext_rast_layer){
  if(length(rast_names) > 0){
    total_length <- length(rast_layer) + length(ext_rast_layer)
    if(length(rast_names) != total_length){
      warning(strwrap(paste0("- length of argument 'rast_names' differs with the length of provided
                               rasters in arguments 'rast_layer' and/or 'ext_rast_layer'. Argument was discarded."),
                      prefix = "\n", initial = ""))
      rast_names <- rep(NA, total_length)
    }
    out <- list(data.frame(rasterlayer = 1:length(rast_layer),
                           name = rast_names[1:length(rast_layer)]),
                data.frame(rasterlayer = 1:length(ext_rast_layer),
                           name = rast_names[(length(rast_layer) + 1):total_length]))
  } else {
    if(length(rast_layer) > 0){
      lsm_names <- data.frame(rasterlayer = 1:length(rast_layer),
                              name = rep(NA, length(rast_layer)))
    } else {
      lsm_names <- data.frame()
    }
    if(length(ext_rast_layer) > 0){
      ext_names <- data.frame(rasterlayer = 1:length(ext_rast_layer),
                              name = rep(NA, length(ext_rast_layer)))
    } else {
      ext_names <- data.frame()
    }
    out <- list(lsm_names, ext_names)
  }

  return(out)
}

rast_tolist <- function(raster){
  if(!is.list(raster)) raster <- terra::as.list(raster)
  if(length(raster) > 0){
    for(i in 1:length(raster)){
      if(class(raster[[i]]) %in% c("RasterLayer", "RasterStack", "RasterBrick"))
        raster[[i]] <- terra::rast(raster[[i]])
    }
  }
  return(raster)
}

#' Generates object of class 'MultiLand'
#'
#' Creates an object of class 'MultiLand', which is the
#' main object to be used by other functions
#' of the package to generate plots, calculate landscape metrics and perform other relevant
#' analyses.
#'
#' @param points_layer An object of class 'SpatVector', 'SpatialPoints', 'SpatialPointsDataFrame' or 'sf', or a string with the
#'   path to a vector file.
#' @param class_names A list matching each raster value with a class name. See Details.
#' @param site_ref A string with the name of the column containing the identity of the sites in
#'   points layer data (argument `points_layer`). See Details.
#' @param radii A numeric vector with the radii (in meters) from which buffers will be created.
#' @param bufftype Type of buffer to be created: "round" for circular buffers (default) or "square".
#' @param segs Number of line segments to use to approximate a quarter circle during buffer generation. Only valid when
#'   `bufftype = "round"`. Default is 20.
#' @param rast_layer,ext_rast_layer An object of class 'SpatRaster', 'RasterLayer', 'RasterStack', 'RasterBrick',
#'   or a list of raster objects (any of 'RasterLayer' or 'SpatRaster').
#' @param rast_names A character vector with the names of the raster layers provided in `rast_layer`
#' and `ext_rast_layer`. See Details.
#' @param on_the_fly Logical. If FALSE (default) intersections between buffers and raster layers will
#'   be calculated. If TRUE, only buffers will be generated. See Details.
#' @param progress Logical. If TRUE (default), progress of the analysis will be printed.
#'
#' @details `mland()` is the primary function of the package. It creates an object of class
#'   'MultiLand' that holds relevant objects and information about points, buffers and intersections
#'   between buffers and raster layers.
#'
#'   The function firstly creates buffers with center in the sites defined in `points_layer`, and size defined by
#'   the values of `radii`. If each point defined in `points_layer` has an associated name or id for
#'   ulterior identification, the user should provide the name of the attribute inside `points_layer`
#'   containing this information, by passing this string through the argument `site_ref`.
#'
#'   Argument `rast_layer` must be provided with raster layers with discrete values from which different landscape metrics (provided by package [landscapemetrics])
#'   could be calculated. Extra raster layers can be provided in `ext_rast_layer`, from which other metrics can be
#'   calculated. For instance, an extra raster layer could be one depicting continuous values of slope,
#'   from which a mean value per landscape may be calculated. Raster layers should be in a coordinate reference
#'   system with meters as the unit, and all raster and vector layers must be in the same coordinate reference system.
#'   The user may check the validity of the raster layer with [check_raster()].
#'
#'   The extent of the provided points layer should not exceed the limits of the extent of the provided raster layers. In addition,
#'   the difference between the outer borders of provided points layer and raster layers should not be less than the maximum
#'   provided radius in `radii`. The purpose is to avoid generating total or partially "empty landscapes" due to the existence of
#'   non-overlapping regions between the buffers (generated around each point given a provided radius) and the raster layers.
#'   If any of this happens, a warning will be returned.
#'
#'   If `on_the_fly = FALSE` (default), intersections between buffers and raster layers
#'   defined in `rast_layer` and/or `ext_rast_layer` will be generated. Otherwise, if `on_the_fly = TRUE`, only buffers will be generated. The
#'   latter approach may be particularly useful for 'MultiLand' objects with numerous points
#'   (hundreds or thousands), in order to avoid returning an object excessively heavy for memory. If
#'   this is the case, intersections between buffers and raster layers will be generated when
#'   required ("on the fly"). For instance, to calculate metrics with [mland_metrics()].
#'
#'   The names of the provided raster layers can be defined in `rast_names`. If so, this must be
#'   a character vector with as many names (strings) as provided raster layers in arguments `rast_layer` and
#'   `ext_rast_layer`, in the mentioned order. If there is no need of a name for a particular raster layer, the given element in the vector
#'   should be `NA`. Definition of these names could be useful when applying other functions of
#'   the package to the object generated here. For instance, to get the name of the raster layer
#'   of a particular row of the data.frame with landscape metrics exported by [mland_metrics()].
#'
#'   Classes names can be associated with each value of the raster layers defined in `rast_layer`, for a
#'   easier future identification. If so, the user must provide a list with as many elements as
#'   raster layers defined in `rast_layer`. If a 'SpatRaster' with multiple layers, a 'RasterStack' or a
#'   'RasterBrick' is provided, the
#'   number of raster layers are extracted from these objects. Each element of the list must be a
#'   vector built from concatenated pairs of values, with the value of the raster (the class) in the
#'   first place and the associated class name in the second place. For example, in the case only
#'   one rasterlayer is provided (with four unique values: 1, 2, 3 and 4), a plausible definition
#'   for the argument `class_names` could be the following:
#'
#' \preformatted{
#'  list(c(1, "Forest", 2, "Crops", 3, "Urban", 4, "Grassland"))
#' }
#'
#'   If, for instance, two raster layers are provided (with four unique values for the first layer,
#'   and two unique values for the second one), a plausible definition would be:
#'
#' \preformatted{
#'  list(c(1, "Forest", 2, "Crops", 3, "Urban", 4, "Grassland"),
#'       c(1, "Burnt Areas", 2, "Non-burnt Areas"))
#' }
#'
#' @return An object of class 'MultiLand'. This object can be used to generate useful plots with
#'   [mland_plot()], calculate metrics with [mland_metrics()] and calculate buffer's overlapping with
#'   [mland_overlap()]. See ?MultiLand for more details on the content of this object.
#' @export
#' @seealso [mland_plot()], [mland_metrics()], [mland_overlap()], [generate_points()]
#' @examples
#' # Loads main raster with land covers
#' elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
#'
#' # Main raster should have discrete values (e.g. land covers). This can be
#' # checked with the function check_raster():
#'
#' check_raster(elchaco)
#'
#' # Loads extra raster with NDVI values
#' elchaco_ndvi <- terra::rast(system.file("extdata", "elchaco_ndvi.tif",
#'                                         package = "multilandr"))
#'
#' # Classes names
#' cl_names <- c(1, "Forest",
#'               2, "Grassland",
#'               3, "Crops",
#'               4, "Pastures",
#'               5, "Water",
#'               6, "Urban")
#'
#' # Loads points
#' elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.gpkg",
#'                                          package = "multilandr"))
#'
#' # Creates 'MultiLand' object by loading main raster, an extra raster and points.
#' ernesdesign <- mland(points_layer = elchaco_sites,
#'                      rast_layer = elchaco,
#'                      radii = seq(1000, 5000, 1000),
#'                      class_names = list(cl_names),
#'                      site_ref = "name",
#'                      ext_rast_layer = elchaco_ndvi,
#'                      rast_names = c("landcover", "NDVI"),
#'                      segs = 20)
#'
#' # Returns basic information about the object
#' ernesdesign
#'
#' # Returns the classes of each rasterlayer and its names, if initially provided
#' ernesdesign@classes
#'
#' \dontrun{
#' # Loads another main raster. Same classes as "elchaco", but a different year.
#' elchaco2 <- terra::rast(system.file("extdata", "elchaco2.tif",
#'                                     package = "multilandr"))
#'
#' # Creates 'MultiLand' with two raster layers.
#' ernesdesign2 <- mland(points_layer = elchaco_sites,
#'                       rast_layer = list(elchaco, elchaco2),
#'                       radii = seq(1000, 5000, 1000),
#'                       class_names = list(cl_names, cl_names),
#'                       site_ref = "name")
#'
#' # Creates the same object but with "on_the_fly = T". Intersections between
#' # buffers and rasters will not be generated in this step
#' ernesdesign3 <- mland(points_layer = elchaco_sites,
#'                       rast_layer = list(elchaco, elchaco2),
#'                       radii = seq(1000, 5000, 1000),
#'                       class_names = list(cl_names, cl_names),
#'                       site_ref = "name",
#'                       on_the_fly = T)
#'
#' # Creates a MultiLand object with hundreds of points. In this case, these
#' # points were generated with generate_points(), another function from this
#' # package. Also, "on_the_fly = TRUE" assures that no intersections between buffers
#' # and the raster are created in this step.
#'
#' # Loads points
#' otf_sites <- terra::vect(system.file("extdata", "otf_sites.gpkg",
#'                                      package = "multilandr"))
#'
#' # Creates MultiLand object
#' otf_design <- mland(points_layer = otf_sites,
#'                     rast_layer = elchaco,
#'                     radii = 2000,
#'                     class_names = list(c(1, "Forest",
#'                                          2, "Grassland",
#'                                          3, "Crops",
#'                                          4, "Pastures",
#'                                          5, "Water",
#'                                          6, "Urban")),
#'                     on_the_fly = TRUE)
#' }
mland <- function(points_layer,
                  rast_layer = NULL,
                  radii,
                  class_names = NULL,
                  site_ref = NULL,
                  bufftype = "round",
                  segs = 20,
                  ext_rast_layer = NULL,
                  rast_names = NULL,
                  on_the_fly = FALSE,
                  progress = TRUE){

  # Check arguments
  environment(.mland_chk_args) <- environment()
  chk <- .mland_chk_args()
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

  # Transform rast_layer and ext_rast_layer arguments into lists
  rast_layer <- rast_tolist(rast_layer)
  ext_rast_layer <- rast_tolist(ext_rast_layer)

  # Sort radii
  radii <- radii[order(unique(radii))]

  # Generate class layers and points
  if(progress) message("Loading layers")
  list_layers <- tryCatch(.generate_layers(points_layer, site_ref, rast_layer,
                                           ext_rast_layer, max(radii)),
                         error = function(e){
                           message("")
                           stop(e)})
  classes     <- list_layers[[1]]
  l_points    <- list_layers[[2]]
  site_ref    <- list_layers[[3]]
  l_rasters   <- rast_layer
  n_points    <- length(l_points)

  # Generate buffers
  if(progress) message("Generating buffers")
  l_buffers <- .buffering(l_points, radii, bufftype, segs)

  # CRS data extraction
  crs_proj <- terra::crs(l_points)

  # Generate intersections between buffers and classes
  if(progress & !on_the_fly) message("Generating intersections")
  ints       <- .intersecting(l_rasters, l_buffers, classes, ext_rast_layer, on_the_fly)
  n_classes  <- ints[[2]]
  landscapes <- list(ints[[1]], ints[[3]])
  names(landscapes) <- c("lsm_rasters", "ext_rasters")

  # Class names generation
  class_names <- .labeling(class_names, classes, n_classes)
  if(length(n_classes) > 0){
    if(length(class_names) == 0){
      df_classes <- data.frame(rasterlayer = rep(1:length(l_rasters), times = n_classes),
                               class = unlist(classes),
                               classname = NA)
    } else {
      unlisted_classes <- unlist(class_names)
      df <- data.frame(class = unlist(classes))
      df_classes <- data.frame(rasterlayer = rep(1:length(l_rasters), times = n_classes),
                               class = as.numeric(unlisted_classes[seq(1, length(unlisted_classes),
                                                                       2)]),
                               classname = unlisted_classes[seq(2, length(unlisted_classes), 2)])
      df_classes <- df_classes[order(df_classes$rasterlayer, df_classes$class), ]
    }
  } else {
    df_classes <- data.frame()
  }
  # Extra raster names
  rast_names <- rast_labels(rast_names, rast_layer, ext_rast_layer)
  names(rast_names) <- c("lsm", "ext")

  # Data frame with references for each point and radii
  n_radii <- length(radii)
  n_points <- length(l_points)
  if(!is.null(site_ref)){
    site_ref_ids <- rep(terra::values(l_points)[, site_ref], each = n_radii)
  } else { site_ref_ids <- rep(1:n_points, each = n_radii)}

  df_reference <- data.frame(row_id = 1:(n_points*n_radii),
                             layer = rep(NA, n_points, each = n_radii),
                             point_id = rep(1:n_points, each = n_radii),
                             site = site_ref_ids,
                             radius = rep(radii, n_points))

  # Unique id for MultiLand object
  id <- as.numeric(keygen())

  # Output
  out <- new("MultiLand",
             call        = match.call(),
             idkey       = id,
             crs_proj    = crs_proj,
             n_layers    = length(l_rasters),
             n_classes   = n_classes,
             classes     = df_classes,
             points      = l_points,
             buffers     = l_buffers,
             on_the_fly  = on_the_fly,
             landscapes  = landscapes,
             rast_names  = rast_names,
             l_ref       = df_reference,
             site_ref    = site_ref,
             radii       = radii)

  invisible(out)
}
