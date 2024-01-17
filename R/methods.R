#' @include classes.R
#NULL

if(!isGeneric("show"))
  methods::setGeneric("show", function(object) standardGeneric("show"))

#' Show 'MultiLand' object
#'
#' @param object Prints relevant information about a 'MultiLand' object.
#'
#' @export
methods::setMethod(f = "show", signature = "MultiLand",
          definition =
          function(object){
            cat("class            : MultiLand\n")
            cat("On the fly       :", object@onthefly, "\n")
            cat("Rasterlayers     :", object@n_layers, "\n")
            if(length(object@n_classes) > 0){
              cat("  n classes      :", object@n_classes, "\n")
            } else {
              cat("  n classes      : -\n")
            }
            cat("Ext. rasterlayers:", length(object@landscapes[[2]]), "\n")
            cat("n points         :", nrow(object@points), "\n")
            if(!is.null(object@site_ref)){
              cat("Site reference   : \"", object@site_ref, "\"\n", sep = "")
            } else {
              cat("Site reference   : -\n")
            }
            if(length(object@radii) > 6){
              cat("Radii (m)        :", object@radii[1:5], "...", object@radii[length(object@radii)], "\n")
            } else {
              cat("Radii (m)        :", object@radii, "\n")
            }
          }
)

#' Show 'MultiLandMetrics' object
#'
#' @param object Prints relevant information about a 'MultiLandMetrics' object.
#'
#' @export
methods::setMethod(f = "show", signature = "MultiLandMetrics",
          definition =
            function(object){
              cat("class            : MultiLandMetrics\n")
              cat("Number of layers :", object@n_layers, "\n")
              cat("Number of classes:", object@n_classes, "\n")
              cat("Number of points :", object@n_points, "\n")
              cat("Radii (m)        :", show_sample(object@radii, limit = 5), "\n")
              cat("Metrics\n  Landscape-level:",
                  show_sample(unique(object@metrics[object@metrics$level == "landscape", "metric"]),
                              limit = 5),
                         "\n  Class-level    :",
                  show_sample(unique(object@metrics[object@metrics$level == "class", "metric"]),
                              limit = 5),
                         "\n  Patch-level    :",
                  show_sample(unique(object@metrics[object@metrics$level == "patch", "metric"]),
                              limit = 5), "\n")
              cat("Extra calcs.     :", show_sample(unique(object@ext_calcs$fun), limit = 5), "\n")
            }
)
