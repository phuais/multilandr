#' Buffers overlapping
#'
#' Returns matrices informing the degree of overlapped area
#' between buffers of a 'MultiLand' object.
#'
#' @inheritParams mland_metrics
#' @param points Numeric or character vector depicting the points to be considered. If NULL, all
#' points will be taken into account. See Details.
#' @param radii Numeric vector depicting the radii to be considered. If NULL, all radii will be taken
#' into account.
#' @param digits Numeric. Number of digits for the values of overlapped areas. Default is 2.
#' @param title One of the following: "id" to output each point with its id (default), or "sitename" to
#' output each point with its pre-defined point name in `x`.
#' @param perc Logical. If TRUE (default) the degree of overlapped areas will be
#' presented as percentages. If FALSE, proportions will be outputted.
#'
#' @details
#' If argument `points` is a character vector,
#' [mland_overlap()] will assume that the 'MultiLand' object inputted in argument `x` was created with
#' `site_ref = TRUE`. This is, there is a column/attribute in the points layer with the names for
#' each distinct point. Therefore, the inputted values in argument `points` will be taken as these
#' identification names. Otherwise, if a numeric vector is declared, the inputted values
#' will be taken as the automatically generated point ids (created when executing [mland()]).
#'
#' @return A list with as many elements as different radius in `x`. Each element contains a
#' matrix with the percentages (or proportions if `perc = FALSE`) of overlapping of buffer areas.
#'
#' @examples
#' \dontrun{
#' # Loads a 'MultiLand' object
#' ernesdesign <- system.file("extdata", "ernesdesign.zip", package = "multilandr")
#' ernesdesign <- mland_load(ernesdesign)
#'
#' # Returns a matrix with the percentage of overlapping between buffers of each radii
#' mland_overlap(ernesdesign)
#'
#' # Selects only one radius and return the site names rather than the ids
#' mland_overlap(ernesdesign, radii = 5000, title = "sitename")
#' }
#'
#' @export
mland_overlap <- function(x,
                          points = NULL,
                          radii = NULL,
                          digits = 2,
                          perc = TRUE,
                          title = "id"){

  # Check arguments
  if(!is(x, "MultiLand")) stop("- argument 'x' must be an object of class 'MultiLand'.")
  environment(.mland_overlap_check_args) <- environment()
  chk <- .mland_overlap_check_args()
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

  if(is.null(points)){
    points <- 1:length(x@points)
  } else { points <- as.numeric(points) }
  if(is.null(radii)) radii <- x@radii

  df_reference <- x@l_ref

  # filter df_reference by site id or point id, and radii
  df_reference <- df_reference[df_reference$point_id %in% points &
                                 df_reference$radius %in% radii, ]

  # final number of radii and points to be evaluated
  n_radii  <- length(radii)
  n_points <- length(points)

  summary_list <- vector("list", n_radii)
  summary <- matrix(data = rep(0, n_points^2), nrow = n_points, ncol = n_points)
  # Generating matrix
  for(k in 1:n_radii){
    for(j in 1:n_points){
      p1 <- points[j]
      for(i in 1:n_points){
        if(j <= i){
          if(j != i){
            p2 <- points[i]
            buff1 <- x@buffers[df_reference[df_reference$radius == radii[k] &
                                            df_reference$point_id == p1, "row_id"], ]
            buff2 <- x@buffers[df_reference[df_reference$radius == radii[k] &
                                            df_reference$point_id == p2, "row_id"], ]
            buffarea <- terra::expanse(buff1)
            inter <- suppressWarnings(terra::intersect(buff1, buff2))
              if(length(inter) > 0){
              inter_area <- terra::expanse(inter)
              overlap <- round(inter_area/buffarea)
              if(perc){
                overlap <- round(inter_area/buffarea, digits = digits + 2)*100
              } else {
                overlap <- round(inter_area/buffarea, digits = digits)
              }
            } else {
              overlap <- 0
            }
            summary[i, j] <- overlap
          } else {
            summary[i, j] <- if(perc) 100 else 1
          }
        } else {
          summary[i, j] <- NA
        }
      }
    }
    if(title == "id"){
      point_ids_col <- "point_id"
    } else {
      point_ids_col <- "site"
    }
    rownames(summary) <- colnames(summary) <- df_reference[df_reference$radius == radii[k],
                                                           point_ids_col]
    summary_list[[k]] <- summary
    names(summary_list)[[k]] <- as.character(radii[k])
  }

  return(summary_list)
}
