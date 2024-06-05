.metrics_bind_chk_args <- function(){

  messages <- NULL
  what     <- NULL

  # Data frame
  if(!is.data.frame(data)){
    messages <- append(messages,
                       "- argument 'data' must be a data.frame.")
    what     <- append(what, 2)
  } else {
    if(!any(c("point_id", "site") %in% colnames(data))){
      messages <- append(messages,
                         "- argument 'data' must be a data.frame with at least one of the following
                         column names as unique identifiers for distinct sites: \"site\" or \"point_id\".")
      what     <- append(what, 2)
    } else {
      if("point_id" %in% colnames(data)){
        if(!all(sort(unique(data$point_id)) == sort(unique(x@data$point_id)))){
          messages <- append(messages,
                             "- unique values for column \"point_id\" do not match the corresponding
                             values in x@data$point_id. These site identifiers should match.
                             See ?metrics_bind")
          what     <- append(what, 2)
        }
      }
      if("site" %in% colnames(data)){
        if(!all(sort(unique(data$site)) == sort(unique(x@data$site)))){
          messages <- append(messages,
                             "- unique values for column \"site\" do not match the corresponding
                             values in x@data$site. These site identifiers should match.
                             See ?metrics_bind")
          what     <- append(what, 2)
        }
      }
    }
  }

  # Class names
  chk_classnames <- .chk_classnames(messages, what, class_names)
  messages    <- chk_classnames[[1]]
  what        <- chk_classnames[[2]]
  class_names <- chk_classnames[[3]]

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              class_names = class_names)

  return(out)
}

#' Metric's data preparation
#'
#' Merge data.frame with metric's values with a data.frame with other data.
#'
#' @inheritParams metrics_corr
#' @param data A data.frame with data from each sampling point/site. See Details.
#' @param raster,ext_raster,classes,radii,l_level,c_level Parameters to subset data.frame containing the
#' metrics values. See Details.
#' @param class_names Logical. If TRUE, classes names will be returned as the names of the classes
#' previously provided (if so) when `x` was generated. Default FALSE.
#'
#' @details Merges data.frame with metrics values, contained in an object of
#' class 'MultiLandMetrics' (returned by [mland_metrics()]) with a data.frame with other data for each
#' site. In this way, the returned data.frame will be prepared for later statistical or
#' visual analyses. The data.frame provided in `data` must have a column named "site" or "point_id",
#' containing unique identifiers for each sampling site, which must match with the identifiers
#' present in the data.frame contained in `x` (i.e. data.frame with metrics values for each site).
#' If "site", the function will assume that the site names are provided as identifiers. If "point_id",
#' the function will assume that point ids are being provided. In any case, these identifiers must
#' match the site identifiers in `x`.
#'
#' Argument `raster`, `ext_raster`, `classes`, `radii`, `l_level` and `c_level` can be defined to
#' subset the data.frame contained in `x`. In each one of these, an all-positive or an
#' all-negative vector can be passed, whether to include (all-postive) or exclude (all-negative)
#' the elements to be taken into account for the subsetting:
#' * rasterlayers: a numeric vector with the number of the rasterlayers to be included/excluded.
#' For example: `c(1, 2, 4)` to include rasterlayers 1, 2 and 4; `c(-2, -3)` to exclude rasterlayers 2
#' and 3.
#' * classes: must be a list with as many elements as defined rasterlayers in argument
#' `rasterlayers`. Each element of the list must be a numeric vector (classes identities) with the
#' classes to be included/excluded. If provided a character vector, [metrics_bind()] assumes that
#' classes names are provided. For example, for the case with 2 rasterlayers:
#' `list(c(3, 20, 35), c("Forest", "Crops"))` would include classes 3, 20 and 35 from rasterlayer 1
#' and classes "Forest" and "Crops" for rasterlayer 2. For the case of a unique rasterlayer, there
#' is no need to input a list. For example, for the case of a unique rasterlayer and the
#' exclusion of some classes: `c(-5, -10, -15)` to exclude classes 5, 10 and 15 of
#' the unique rasterlayer; `c("-Forest", "-Grassland")` to exclude classes "Forest" and "Grassland".
#' Note the "-" before each class name to indicate the exclusion of the classes.
#' * radii: a numeric vector to include/exclude particular radii. For example: `c(1000, 2000)` to
#' include only radii of 1000 and 2000 m; `c(-500, -1500)` to exclude radii of 500 and 1500 m.
#' * c_level: character vector with the class-level metrics to be included/excluded from
#' the analysis. For example: `c("np", "pland")` will include only the metrics "number of patches"
#' ("np") and "percentage of the landscape" ("pland") in the analysis, whereas `c("-np", "-pland")`
#' will exclude them. Note the "-" before each metric name to indicate the exclusion of the
#' metrics.
#' * l_level: character vector with the landscape-level metrics to be included/excluded from
#' the analysis. Extra calculations for extra rasterlayers are considered as landscape-level metrics,
#' and must be provided as "fun_" + the name of the function.
#'
#' @return A data.frame equal to sampling data provided in `data` but with additional columns
#' containing the values of the metrics for each sampling site.
#'
#' @seealso [mland_metrics()]
#'
#' @export
#'
#' @examples
#' # Get sites names from ed_metrics and creates ad-hoc data.frame with random values of
#' # "richness" (the response variable). Only for the purpose of this example
#' sites <- ed_metrics@points$name
#' sampling_data <- data.frame(site = rep(sites, each = 10),
#'                             richness = sample(1:500, 150))
#'
#' # With no filters, all columns with all metrics at all spatial scales are added to
#' # the sampling data
#' new_data <- metrics_bind(ed_metrics, sampling_data)
#'
#' # Subset for metrics of class "Forest", radius 5000 and metric "pland"
#' new_data <- metrics_bind(ed_metrics, sampling_data, class_names = TRUE,
#'                          classes = "Forest", radii = 3000, c_level = "pland")
#'
#' # In this format, the data.frame can be passed to a fitting model
#' fit <- lm(richness ~ r1_Forest_pland_3000, data = new_data)
metrics_bind <- function(x,
                         data,
                         raster = NULL,
                         classes = NULL,
                         radii = NULL,
                         c_level = NULL,
                         l_level = NULL,
                         ext_raster = NULL,
                         class_names = FALSE){

  if(!is(x, "MultiLandMetrics")) stop("x must be an object of class 'MultiLandMetrics'.")
  environment(.metrics_bind_chk_args) <- environment()
  chk <- .metrics_bind_chk_args()
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

  environment(.pair_subsets) <- environment()
  df <- .pair_subsets()
  new_df_wide <- df[[1]]
  out <- merge(data, new_df_wide)
  cols <- colnames(out)[!colnames(out) %in% c("point_id", "site")]
  out <- out[, c("point_id", "site", cols)]
  return(out)
}
