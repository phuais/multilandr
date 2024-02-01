# Generates points layer
.gen_pointlayers <- function(points, site_ref){

  # Generates points layers
  if(class(points)[1] %in% c("SpatVector", "SpatialPoints", "SpatialPointsDataFrame")){
    if(class(points)[1] == "SpatVector"){
      if(terra::geomtype(points) != "points"){
        stop("- points layer must be an object of class 'SpatVector' (geom = 'points'), 'SpatialPointsDataFrame' or 'SpatialPoints'.",
             call. = FALSE)
      } else {
        l_points <- points
      }
    } else {
      l_points <- suppressWarnings(terra::vect(points))
    }
  } else {
    l_points <- suppressWarnings(terra::vect(points))
    if(class(l_points)[1] != "SpatVector" | terra::geomtype(l_points) != "points"){
      stop("- points layer must be an object of class 'SpatVector' (geom = 'points'), 'SpatialPointsDataFrame' or 'SpatialPoints'.",
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
                               site_ref, "\" in points layer data. The argument was ignored."),
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
.generate_layers <- function(points, site_ref, raster, ext_raster){

  pp       <- .gen_pointlayers(points, site_ref)
  l_points <- pp[[1]]
  site_ref <- pp[[2]]

  # Loads class layer values
  raster_values <- vector("list", length(raster))
  if(length(raster) > 0){
    for(i in 1:length(raster)){
      cl <- raster[[i]]
      chk <- .check_classCRS(l_points, cl)
      if(chk) stop("- raster CRS must be equal to points layer CRS.",  call. = FALSE)
      # Gets layer classes
      raster_values[[i]] <- sort(terra::unique(cl)[, 1])
    }
  }

  # Check extra rasters
  if(length(ext_raster) > 0){
    for(i in 1:length(ext_raster)){
      cl <- ext_raster[[i]]
      chk <- .check_classCRS(l_points, cl)
      if(chk) stop("- extra rasters CRS must be equal to points layer CRS.",  call. = FALSE)
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
.intersecting <- function(l_rasters, l_buffers, classes, ext_raster, onthefly){

  # Intersections with main rasters
  intersects <- vector("list", length(l_rasters))
  n_classes <- vector("numeric", length(l_rasters))
  if(length(l_rasters) > 0){
    for(i in 1:length(intersects)){
      n_classes[i] <- length(classes[[i]])
      if(!onthefly){
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
  intersects_ext <- vector("list", length(ext_raster))
  if(length(ext_raster) > 0){
    for(i in 1:length(intersects_ext)){
      if(!onthefly){
        intersects_ext[[i]] <- vector("list", length(l_buffers))
        for(j in 1:length(l_buffers)){
          clip <- suppressWarnings(tryCatch(terra::crop(ext_raster[[i]],
                                                        terra::ext(l_buffers[j, ])), error = c))
          if(!is.list(clip)){
            intersects_ext[[i]][[j]] <- terra::mask(clip, l_buffers[j, ])
          } else {
            empty_raster <- terra::rast(nrows = 1, ncols = 1, crs = terra::crs(l_buffers[j, ]),
                                        ext = l_buffers[j, ],
                                        resolution = terra::res(ext_raster[[i]]), vals = NA)
            intersects_ext[[i]][[j]] <- empty_raster
          }
        }

        names(intersects_ext[[i]]) <- paste0("ExtRasterLayer", i, "-", terra::values(l_buffers)$id, "-",
                                             terra::values(l_buffers)$radius)
      } else {
        intersects_ext[[i]] <- ext_raster[[i]]
        names(intersects_ext)[[i]] <- paste0("ExtRasterLayer ", i)
      }
    }
  }

  out <- list(intersects, n_classes, intersects_ext)
  return(out)
}

# Generates labels
.labeling <- function(classnames, classes, n_classes){

  if(length(classnames) > 0){
    if(length(classnames) != length(n_classes)){
      warning(strwrap("- number of elements of argument 'classnames' differ with the number of raster
                      layers. Classnames were discarded.", prefix = "\n", initial = ""),
              call. = FALSE)

      classnames <- vector("list")
    } else {
      for(i in 1:length(classes)){
        if(length(classnames[[i]])/2 != n_classes[i]){
          warning(strwrap(paste0("- number of labels defined in argument 'classnames' differ with the number of
                                 classes of rasterlayer ", i , ". Classnames were discarded."),
                          prefix = "\n", initial = ""),
                  call. = FALSE)
          classnames <- vector("list")
          break
        }
        if(!all(as.numeric(classnames[[i]][seq(1, length(classnames[[i]]), 2)]) %in% classes[[i]])){
          warning(paste0("- one or more classes were not found as classes in rasterlayer ", i ,
                         ". Classnames were discarded."), call. = FALSE)
          classnames <- vector("list")
          break
        }
      }
    }
  }

  if(length(classnames) > 0){
    # Change - for _ in classnames.
    if(length(n_classes) > 0){
      for(i in 1:length(classes)){
        pos <- seq(2, length(classnames[[i]]), 2)
        if(any(grepl("-", classnames[[i]][pos]))){
          classnames[[i]][pos] <- gsub("-", "_", classnames[[i]][pos])
          warning("Pattern \"-\" in classnames was replaced by \"_\".", call. = FALSE)
        }
      }
    }
  }
  return(classnames)
}

# Raster labeling
rast_labels <- function(layer_names, raster, ext_raster){
  if(length(layer_names) > 0){
    total_length <- length(raster) + length(ext_raster)
    if(length(layer_names) != total_length){
      warning(strwrap(paste0("- length of argument 'layer_names' differs with the length of provided
                               rasters in arguments 'raster' and/or 'ext_raster'. Argument was discarded."),
                      prefix = "\n", initial = ""))
      layer_names <- rep(NA, total_length)
    }
    out <- list(data.frame(rasterlayer = 1:length(raster),
                           name = layer_names[1:length(raster)]),
                data.frame(rasterlayer = 1:length(ext_raster),
                           name = layer_names[(length(raster) + 1):total_length]))
  } else {
    if(length(raster) > 0){
      lsm_names <- data.frame(rasterlayer = 1:length(raster),
                              name = rep(NA, length(raster)))
    } else {
      lsm_names <- data.frame()
    }
    if(length(ext_raster) > 0){
      ext_names <- data.frame(rasterlayer = 1:length(ext_raster),
                              name = rep(NA, length(ext_raster)))
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
#' @param points An object of class 'SpatVector', 'SpatialPoints' or 'SpatialPointsDataFrame', or a string with the
#'   path to a shapefile.
#' @param classnames A list matching each raster value with a class name. See Details.
#' @param site_ref A string with the name of the column containing the identity of the sites in
#'   points layer data. See Details.
#' @param radii A numeric vector with the radii (in meters) from which buffers will be created.
#' @param bufftype Type of buffer to be created: "round" for circular buffers (default) or "square".
#' @param segs Number of line segments to use to approximate a quarter circle during buffer generation. Only valid when
#'   `bufftype = "round"`. Default is 20.
#' @param raster,ext_raster An object of class 'SpatRaster', 'RasterLayer', 'RasterStack', 'RasterBrick',
#'   or a list of raster objects (any of 'RasterLayer' or 'SpatRaster').
#' @param layer_names A character vector with the names of the rasterlayers provided in `raster`
#' and `ext_raster`. See Details.
#' @param onthefly Logical. If FALSE (default) intersections between buffers and rasterlayers will
#'   be calculated. If TRUE, only buffers will be generated. See Details.
#' @param progress Logical. If TRUE (default), progress of the analysis will be printed.
#'
#' @details `mland()` is the primary function of the package. It creates an object of class
#'   'MultiLand' that holds relevant objects and information about points, buffers and intersections
#'   between the latters and rasterlayers.
#'
#'   The function firstly creates buffers with center in the sites defined in `points`, and size defined by
#'   the values of `radii`. If each point defined in `points` has an associated name or id for
#'   ulterior identification, the user should provide the name of the attribute inside `points`
#'   containing this information, by passing this string through the argument `site_ref`.
#'
#'   Argument `raster` must be provided with rasterlayers with discrete values from which different landscape metrics (provided by package [landscapemetrics])
#'   could be calculated. Extra rasterlayers can be provided in `ext_raster`, from which other metrics can be
#'   calculated. For instance, an extra rasterlayer could be one depicting continuous values of slope,
#'   from which a mean value per landscape may be calculated.
#'
#'   If `onthefly = FALSE` (default), intersections between buffers and rasterlayers defined in
#'   `raster` and/or `ext_raster` will be generated. Otherwise, if `onthefly = TRUE`, only buffers will be generated. The
#'   latter approach may be particularly useful for 'MultiLand' objects with numerous points
#'   (hundreds or thousands), in order to avoid returning an object excessively heavy for memory. If
#'   this is the case, intersections between buffers and rasterlayers will be generated when
#'   required ("on the fly"). For instance, to calculate metrics with [metrics()].
#'
#'   The names of the provided rasterlayers can be defined in `layer_names`. If so, this must be
#'   a character vector with as many names (strings) as provided rasterlayers in arguments `raster` and
#'   `ext_raster`, in the mentioned order. If there is no need of a name for a particular raster layer, the given element in the vector
#'   should be `NA`. Definition of these names could be useful when applying other functions of
#'   the package to the object generated here. For instance, to get the name of the rasterlayer
#'   of a particular row of the data.frame with landscape metrics exported by [metrics()].
#'
#'   Classes names can be associated with each value of the rasterlayers defined in `raster`, for a
#'   easier future identification. If so, the user must provide a list with as many elements as
#'   rasterlayers defined in `raster`. If a 'SpatRaster' with multiple layers, a 'RasterStack' or a
#'   'RasterBrick' is provided, the
#'   number of rasterlayers are extracted from these objects. Each element of the list must be a
#'   vector built from concatenated pairs of values, with the value of the raster (the class) in the
#'   first place and the associated class name in the second place. For example, in the case only
#'   one rasterlayer is provided (with four unique values: 1, 2, 3 and 4), a plausible definition
#'   for the argument `classnames` could be the following:
#'
#' \preformatted{
#'  list(c(1, "Forest", 2, "Crops", 3, "Urban", 4, "Grassland"))
#' }
#'
#'   If, for instance, two rasterlayers are provided (with four unique values for the first layer,
#'   and two unique values for the second one), a plausible definition would be:
#'
#' \preformatted{
#'  list(c(1, "Forest", 2, "Crops", 3, "Urban", 4, "Grassland"),
#'       c(1, "Burnt Areas", 2, "Non-burnt Areas"))
#' }
#'
#' @return An object of class 'MultiLand'. This object can be used to generate useful plots with
#'   [mland_plot()], calculate metrics with [metrics()] and calculate buffer's overlapping with
#'   [overlapping()]. See ?MultiLand for more details on the content of this object.
#' @export
#' @seealso [mland_plot()], [metrics()], [overlapping()], [land_points()]
#' @examples
#' # Loads main raster and extra raster
#' elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
#' elchaco_ndvi <- terra::rast(system.file("extdata", "elchaco_ndvi.tif", package = "multilandr"))
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
#' elchaco_sites <- terra::vect(system.file("extdata", "elchaco_sites.shp", package = "multilandr"))
#'
#' # Creates 'MultiLand' object by loading main raster, an extra raster and points.
#' ernesdesign <- mland(points = elchaco_sites,
#'                      raster = elchaco,
#'                      radii = seq(1000, 5000, 1000),
#'                      classnames = list(cl_names),
#'                      site_ref = "name",
#'                      ext_raster = elchaco_ndvi,
#'                      layer_names = c("landuse", "NDVI"),
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
#' elchaco2 <- terra::rast(system.file("extdata", "elchaco2.tif", package = "multilandr"))
#'
#' # Creates 'MultiLand' with two rasterlayers.
#' ernesdesign2 <- mland(points = elchaco_sites,
#'                       raster = list(elchaco, elchaco2),
#'                       radii = seq(1000, 5000, 1000),
#'                       classnames = list(cl_names, cl_names),
#'                       site_ref = "name",
#'                       segs = 20)
#'
#' # Creates the same object but with "onthefly = T". Intersections between
#' # buffers and rasters will not be generated in this step
#' ernesdesign3 <- mland(points = elchaco_sites,
#'                       raster = list(elchaco, elchaco2),
#'                       radii = seq(1000, 5000, 1000),
#'                       classnames = list(cl_names, cl_names),
#'                       site_ref = "name",
#'                       segs = 20,
#'                       onthefly = T)
#'
#' # Creates a MultiLand object with hundreds of points. In this case, these
#' # points were generated with land_points(), another function from this package. Also,
#' # "onthefly = TRUE" assures that no intersections between buffers and the raster are
#' # created in this step.
#'
#' # Loads points
#' otf_sites <- terra::vect(system.file("extdata", "otf_sites.shp", package = "multilandr"))
#'
#' # Creates MultiLand object
#' otf_design <- mland(points = otf_sites,
#'                     raster = elchaco,
#'                     radii = 2000,
#'                     classnames = list(c(1, "Forest",
#'                                         2, "Grassland",
#'                                         3, "Crops",
#'                                         4, "Pastures",
#'                                         5, "Water",
#'                                         6, "Urban")),
#'                     onthefly = TRUE,
#'                     segs = 20)
#'
#'
#' }
mland <- function(points, raster = NULL, radii, classnames = NULL, site_ref = NULL,
                  bufftype = "round", segs = 20, ext_raster = NULL, layer_names = NULL,
                  onthefly = FALSE, progress = TRUE){

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

  # Transform raster and ext_raster arguments into lists
  raster     <- rast_tolist(raster)
  ext_raster <- rast_tolist(ext_raster)

  # Sort radii
  radii <- radii[order(unique(radii))]

  # Generate class layers and points
  if(progress) message("Loading layers")
  list_layers <- tryCatch(.generate_layers(points, site_ref, raster, ext_raster),
                         error = function(e){
                           message("")
                           stop(e)})
  classes     <- list_layers[[1]]
  l_points    <- list_layers[[2]]
  site_ref    <- list_layers[[3]]
  l_rasters   <- raster
  n_points    <- length(l_points)

  # Generate buffers
  if(progress) message("Generating buffers")
  l_buffers <- .buffering(l_points, radii, bufftype, segs)

  # CRS data extraction
  crs_proj <- terra::crs(l_points)

  # Generate intersections between buffers and classes
  if(progress & !onthefly) message("Generating intersections")
  ints       <- .intersecting(l_rasters, l_buffers, classes, ext_raster, onthefly)
  n_classes  <- ints[[2]]
  landscapes <- list(ints[[1]], ints[[3]])
  names(landscapes) <- c("lsm_rasters", "ext_rasters")

  # Classnames generation
  classnames <- .labeling(classnames, classes, n_classes)
  if(length(n_classes) > 0){
    if(length(classnames) == 0){
      df_classes <- data.frame(rasterlayer = rep(1:length(l_rasters), times = n_classes),
                               class = unlist(classes),
                               classname = NA)
    } else {
      unlisted_classes <- unlist(classnames)
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
  layer_names <- rast_labels(layer_names, raster, ext_raster)
  names(layer_names) <- c("lsm", "ext")

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
             onthefly    = onthefly,
             landscapes  = landscapes,
             layer_names = layer_names,
             l_ref       = df_reference,
             site_ref    = site_ref,
             radii       = radii)

  invisible(out)
}
