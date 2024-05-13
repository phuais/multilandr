# Credit to Zheyuan Li from stackoverflow
# https://stackoverflow.com/questions/39005958/r-how-to-get-row-column-subscripts-of-matched-
# elements-from-a-distance-matri/39006382#39006382
.finv <- function (k, dist_obj) {
  if (!inherits(dist_obj, "dist")) stop("please provide a 'dist' object")
  n <- attr(dist_obj, "Size")
  valid <- (k >= 1) & (k <= n * (n - 1) / 2)
  k_valid <- k[valid]
  j <- rep.int(NA_real_, length(k))
  j[valid] <- floor(((2 * n + 1) - sqrt((2 * n - 1) ^ 2 - 8 * (k_valid - 1))) / 2)
  i <- j + k - (2 * n - j) * (j - 1) / 2
  cbind(i, j)
}

# Get centroids of the patches
centroid_fn <- function(patches_id, patches, vals, total_patches, reso, closest_cell, progress){
  if(progress)
    cat("\r> Processed patches: ", patches_id , "/", total_patches)
  cells <- which(vals == patches_id)
  rowcols <- terra::rowColFromCell(patches, cells)
  positions <- c(cells[which(rowcols[, 2] == min(rowcols[, 2]))[1]],
                 cells[which(rowcols[, 2] == max(rowcols[, 2]))[1]],
                 cells[which(rowcols[, 1] == min(rowcols[, 1]))[1]],
                 cells[which(rowcols[, 1] == max(rowcols[, 1]))[1]])
  xy <- terra::xyFromCell(patches, positions)
  xy[1, 1] <-  xy[1, 1] - reso[1]/2
  xy[2, 1] <-  xy[2, 1] + reso[1]/2
  xy[1, 2] <-  xy[1, 2] - reso[1]/2
  xy[2, 2] <-  xy[2, 2] + reso[1]/2
  cc <- data.frame(x = mean(xy[1:2, 1]), y = mean(xy[3:4, 2]))

  if(closest_cell){
    val <- patches[terra::cellFromXY(patches, cc)]
    if(is.na(val) || val != patches_id){
      bingo <- F
      rad <- 1
      while(!bingo){
        pp <- terra::vect(cc)
        buff <- terra::buffer(pp, width = mean(reso)*rad, quadsegs = 10)
        extr <- terra::extract(patches, buff, cells = T)[[1]]
        suma <- sum(na.exclude(extr[, 2]))

        if(suma > 0){
          rows <- which(extr[, 2] == patches_id)
          if(suma > 1){
            cc <- terra::xyFromCell(r, extr[sample(rows, 1), 1])
          } else {
            cc <- terra::xyFromCell(r, extr[rows, 1])
          }
          cc <- as.data.frame(cc)
          bingo <- T
        } else {
          rad <- rad + 1
        }
      }
    }
  }

  return(cc)
}

# Gets metrics of the patches and raster with patches
patch_calc <- function(raster, patch_conditions){
  patch_metrics <- vector("character")
  patches_class <- vector("numeric")
  for(i in 1:length(patch_conditions)){
    patch_metrics <- append(patch_metrics, patch_conditions[[i]][[2]])
    patches_class <- append(patches_class, patch_conditions[[i]][[1]])
  }

  tab <- tryCatch.W.E(landscapemetrics::calculate_lsm(landscape = raster,
                                                      level = "patch", metric = patch_metrics))

  if(length(class(tab$value)) >= 2){
    if(any(class(tab$value)[1:2] %in% c("simpleError", "error"))){
      message("landscapemetrics errors:")
      stop(tab$value)
    }
  } else { tab <- tab$value }

  df_tmp <- as.data.frame(tab$value)
  df_tmp <- reshape(df_tmp, idvar = c("id", "class"),
                    timevar = c("metric"), drop = c("layer", "level"),
                    direction = "wide")

  patch_rast <- landscapemetrics::get_patches(raster, class = patches_class, to_disk = T)$layer_1

  out <- list(df_tmp, patch_rast)
  return(out)
}

# Select patches with required conditions
patch_select <- function(df_tmp, patch_rast, patch_conditions){
  for(i in 1:length(patch_conditions)){
    if(is.na(patch_conditions[[i]][[1]][1])){
      patch_conditions[[i]][[1]] <- unique(df_tmp$class)
      message(paste0("- classes included in the filtering process: ",
                     paste0(patch_conditions[[i]][[1]], collapse = " ")))
    }
    df_tmp <- df_tmp[df_tmp$class %in% patch_conditions[[i]][[1]] &
                       df_tmp[paste0("value.", patch_conditions[[i]][[2]])] >=
                       as.numeric(patch_conditions[[i]][[3]]) &
                       df_tmp[paste0("value.", patch_conditions[[i]][[2]])] <=
                       as.numeric(patch_conditions[[i]][[4]]), ]
  }

  if(nrow(df_tmp) > 0){
    cls <- strsplit(names(patch_rast), "_")
    for(i in 1:length(patch_rast)){
      patch_rast[[i]] <- terra::match(patch_rast[[i]],
                                      df_tmp[df_tmp$class == as.numeric(cls[[i]][2]), "id"])
    }
  } else {
    return(NULL)
  }

  return(patch_rast)
}

#' Generates point coordinates
#'
#' Generates point coordinates over a rasterlayer extent.
#'
#' @param raster An object of class 'SpatRaster', 'RasterLayer', 'RasterStack' or 'RasterBrick'.
#' @param approach One of the following: "grid" to generate points through a grid, "random" to
#' generate points at random locations, or "patch" to generate points inside patches that meet
#' pre-defined conditions. See Details.
#' @param n Number of point to generate.
#' @param padding Numeric. Width (in meters) of the internal margin around the raster that will be
#' discarded from the analysis. See Details.
#' @param try Number of points to be generated in
#' each turn. Only applies if `approach = "random"`. See Details.
#' @param values The values of the rasterlayer where points should be placed. Only applies if
#' `approach = "random"`.
#' @param trim Logical. If TRUE (default) the number of final points will be trimmed to the value
#' defined in `n`.
#' @param attempts Number of attempts to generate new random points given the total required
#' points (`n`) and the minimum distance required in `distance`. Only applies if
#' `approach = "random"`. See Details.
#' @param distance Distance between points of the grid (if `approach = "grid"`) or minimum distance
#' between generated points (if `approach = "random"`).
#' @param offset Logical. If TRUE, each point coordinates will be randomly displaced around the area
#' occupied by the raster cell size. If FALSE (default), each point will be located at the center of
#' a given raster cell. Only applies if `approach = "random"`.
#' @param patch_conditions The conditions that patches must meet to be included as the
#' patches from which points will be generated. Only applies if `approach = "patch"`. See Details.
#' @param closest_cell Logical. If `approach = "patch"`, whether to return the coordinates of each patch centroid even if
#' they fall outside the patch (FALSE, default) or to move the point to the
#' closest cell of the patch if this happens (TRUE).
#' @param parallel Logical. If TRUE, part of the processes will be parallelized. See Details.
#' @param cores Number of cores to use if `parallel = TRUE`.
#' @param progress Logical. If TRUE (default), progress of the analysis will be printed.
#'
#' @details If `approach = "random"`, the user can restrict the locations of new generated points
#' inside raster cells with certain value or values, by defining them in `values`. Also a minimum distance
#' between the generated points can be defined in `distance` (also applies for the resolution of
#' the grid if `approach = "grid"`).
#'
#' If `approach = "random"` and a minimum distance was defined, the function will generate new
#' "random" points in sequential passes. In each pass, the function will try to generate new points
#' taking into account the minimum distance between points, by randomly generating a number of points as
#' defined in `try`. The function will perform this task until the new generated points is equal or
#' higher than `n`. If `try = NULL` (default), `try` will equals `n`. If in each turn no new points
#' were added (i.e. new points were located at less than the minimum distance to other previously
#' generated points), the function will record this event. If this event happens more than the
#' number of times defined in `attempts` before the total generated points equals `n`, the function will
#' terminate, and return the points that were successfully generated given the required parameters.
#' The user may try different values for `n`, `try` and `attempts` to get a desirable result.
#'
#' If `approach = "patch"`, the function will return as many points as patches that meet certain
#' conditions in relation to pre-defined metric values. Conditions can be defined in
#' argument `patch_conditions`, for which the helper function [conditions()] is available:
#'
#' \preformatted{
#'  conditions(list(class, metric, minimum value, maximum value),
#'             list(class, metric, minimum value, maximum value), ...)
#' }
#'
#' * class: the class (raster value) of the patch that must meet the defined conditions. More than one class can
#' be specified.
#' * metric: the patch-level metric whose values must meet the defined conditions. Only one metric
#' per condition can be defined. Available patch-level metrics can be found in [metrics_list()] and in
#' documentation of the package [landscapemetrics()].
#' * minimum value: the minimum value that the metric must have for the retained patches. If equal
#' to -Inf, and a maximum value is defined, patches whose values in the defined metric are equal
#' or lower to the maximum value will be retained.
#' * maximum value: the maximum value that the metric must have in the retained patches. If equal
#' to Inf, and a minimum value is defined, patches whose values in the defined metric are equal
#' or higher to the minimum value will be retained.
#'
#' Retained patches will be those patches that meet all patch conditions at the same time. Returned
#' point's coordinates will equal the centroid of each patch. If `closest_cell = TRUE`, the point's coordinates of the
#' centroids that did not fall inside the patch will be moved to the closest cell belonging to that
#' patch.
#'
#' To avoid generating points to close to the boundaries of the raster, the outer borders of the
#' raster can be discarded from the analysis, by considering the width inputted in `padding`.
#'
#' If `parallel = TRUE` the function will parallelize part of the processes. Parallelization
#' is done to obtain the coordinates of the patches if `approach = "patch"`. The number of
#' cores must be declared in `cores` (parallelization requires at least two cores). To use this
#' functionality, package `parallel` must be installed. So far, parallelization will run
#' in LINUX and MAC, but not in Windows.
#'
#' @return An object of class 'SpatVector' containing the coordinates of the generated points.
#'
#' @seealso [mland()]
#'
#' @export
#'
#' @examples
#' # Loads raster
#' elchaco <- terra::rast(system.file("extdata", "elchaco.tif", package = "multilandr"))
#'
#' # Returns points at "random" locations, but inside cells of value equals to 1.
#' chaco_coords <- generate_points(elchaco, approach = "random", values = 1, n = 500)
#'
#' # The same but points must be separated by at least 300 m between each other. Also, each point
#' # is randomly displaced inside the raster cell.
#' chaco_coords2 <- generate_points(elchaco, approach = "random", values = 1, n = 500,
#'                              try = 100, distance = 300, offset = TRUE)
#'
#' \dontrun{
#' # Returns as many points as patches that meet the defined condition. This is
#' # all patches of value equal to 1 of area between 9 and 11 hectares.
#' patch_sites <- generate_points(elchaco, approach = "patch",
#'                            patch_conditions = conditions(list(1, "area", 8, 12)))
#'}
generate_points <- function(raster,
                            approach = "grid",
                            n = NULL,
                            padding = 0,
                            try = NULL,
                            values = NULL,
                            patch_conditions = NULL,
                            trim = TRUE,
                            attempts = 10,
                            distance = NULL,
                            offset = FALSE,
                            closest_cell = FALSE,
                            parallel = FALSE,
                            cores = 1,
                            progress = TRUE){

  environment(.generate_points_chk_args) <- environment()
  chk <- .generate_points_chk_args()
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

  # Transform to SpatRaster, if required
  if(class(raster) %in% c("RasterLayer", "RasterStack", "RasterBrick"))
    raster <- terra::rast(raster)

  # Crop raster if padding != 0
  if(padding > 0){
    e <- terra::ext(raster)
    e_padd <- suppressWarnings(tryCatch(terra::ext(e[1] + padding,
                                                   e[2] - padding,
                                                   e[3] + padding,
                                                   e[4] - padding),
                                        error = c))
    if(is.list(e_padd)){
      stop("- could not get a valid extent given the provided padding. See ?generate_points.")
    } else {
      raster <- terra::crop(raster, e_padd)
    }
  }

  # Grid approach
  if(approach == "grid"){
    e <- terra::ext(raster)
    p <- terra::as.polygons(e)
    terra::crs(p) <- terra::crs(raster)
    p_sf <- sf::st_as_sf(p)
    points <- sf::st_make_grid(p_sf, cellsize = c(distance, distance), what = "corners")
    points <- terra::vect(points)
    points <- points[p, ]
  } else {
    points <- terra::vect(cbind(x = 0, y = 0), atts = data.frame(cell = 1, value = 1),
                              crs = terra::crs(raster))[-1, ]

    # Patch approach
    if(approach == "patch"){
      if(progress) message("Searching patches")
      foo <- patch_calc(raster, patch_conditions)
      patch_s <- patch_select(foo[[1]], foo[[2]], patch_conditions)
      if(!is.null(patch_s)){
        if(progress) message("Getting coordinates")
        for(i in 1:length(patch_s)){
          if(!is(patch_s[[i]], "SpatRaster"))
            patch_s[[i]] <- terra::rast(patch_s[[i]])
          total_patches <- terra::minmax(patch_s[[i]])[2, 1]
          reso <- terra::res(patch_s[[i]])
          vals <- terra::values(patch_s[[i]], mat = F)
          if(parallel){
            if(!requireNamespace("parallel", quietly = TRUE)) {
              stop("Package \"parallel\" must be installed to parallelize this process.",
                call. = FALSE)
            }
            centroids <- parallel::mclapply(1:total_patches, centroid_fn, mc.cores = cores,
                                            patch_s[[i]], vals, total_patches, reso, closest_cell,
                                            progress)
          } else {
            centroids <- lapply(1:total_patches, centroid_fn, patch_s[[i]], vals, total_patches, reso,
                                closest_cell, progress)
          }
          centroids <- do.call("rbind", centroids)
          next_points <- terra::vect(cbind(x = centroids$x, y = centroids$y),
                                     atts = data.frame(cell = 1:nrow(centroids)),
                                     crs =  terra::crs(raster))
          points <- rbind(points, next_points)
        }
      }
      if(length(points) == 0){
        cat("\n")
        stop("- no patches were found that matches the required patch conditions.", call. = F)
      }
      return(points)
    }

    # Random approach
    if(progress)
      pb <- txtProgressBar(min = 0, max = n, style = 3, width = 50, char = "=")
    n_attempts <- 0
    end <- F
    while(!end){
      if(is.null(try)){
        try <- n
      }
      new_points <- as.data.frame(terra::spatSample(raster, try, xy = T, cells = T))
      if(!is.null(values))
        new_points <- new_points[new_points[, 4] %in% values, ]

      colnames(new_points)[4] <- "value"
      if(nrow(new_points) > 0){
        d <- new_points[c("cell", "value")]
        new_points <- terra::vect(as.matrix(new_points[c("x", "y")]),
                                  atts = new_points[c("cell", "value")],
                                  crs = terra::crs(raster))

      } else {
        new_points <- terra::vect(cbind(x = 0, y = 0), atts = data.frame(cell = 1, value = 1),
                                  crs = terra::crs(raster))[-1, ]
      }

      past_points <- points

      # Random offset, if required
      if(offset){
        if(length(new_points) > 0){
          coords <- terra::crds(new_points)
          reso <- terra::res(raster)
          for(i in 1:length(new_points)){
            x_offset <- runif(1, -reso[1], reso[1])
            y_offset <- runif(1, -reso[2], reso[2])
            coords[i, 1] <- coords[i, 1] + x_offset
            coords[i, 2] <- coords[i, 2] + y_offset
          }
          new_points <- terra::vect(x = coords, atts = d, crs = terra::crs(raster))
        }
      }

      if(length(new_points) > 0){
        points <- rbind(points, new_points)
        points <- points[!duplicated(points$cell), ]
      }
      added_points <- length(points)

      if(!is.null(distance)){
        points_tobeadded <- length(new_points)
        dists <- dist(terra::crds(points))
        dists_filtered <- .finv(which(dists < distance), dists)
        p_toremove <- sort(unique(as.vector(dists_filtered)))

        # Remove "very connected" points, step by step
        finish <- F
        while(!finish){
          dists <- dist(terra::crds(points))
          dists_filtered <- .finv(which(dists < distance), dists)
          if(nrow(dists_filtered) > 0){
            core_points <- sort(table(as.vector(dists_filtered)))
            points_to_remove <- as.numeric(names(core_points[core_points == max(core_points)]))
            points <- points[-points_to_remove, ]
          } else {
            finish <- T
          }
        }

        added_points <- length(points) - length(past_points)
        if(added_points <= 0) points <- past_points
      }

      if(added_points <= 0){
        n_attempts <- n_attempts + 1
      } else {
        n_attempts <- 0
      }
      if(length(points) >= n) end <- T

      i <- if(length(points) > n) n else length(points)
      if(progress) setTxtProgressBar(pb, i)

      if(n_attempts == attempts){
        end <- T
        message(strwrap("\nCould not get the required number of random points. You may try
        increasing argument 'try' and/or 'attempts', or reduce n.",  prefix = "\n", initial = "\n"),
                (strwrap("If approach = \"patch\", you may define less
                         restrictive patch conditions.", prefix = "\n", initial = "\n")))
        message(paste0("Final sampled points = ", length(points), "/", n, " (",
                       round(100*length(points)/n, digits = 2), "%)"))
      }
    }
    if(progress) close(pb)

    if(trim){
      if(length(points) > n)
        points <- points[-sample(1:length(points), length(points) - n), ]
    }
  }
  points$id <- 1:length(points)
  points <- points["id"]

  return(points)
}
