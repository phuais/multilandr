#' Metrics list
#'
#' List of available landscape metrics provided by package `landscapemetrics` to be calculated with
#' [mland_metrics()]. It simply calls [landscapemetrics::list_lsm()]. For more information regarding the
#' definition and equations of metrics, please check the user manual of `landscapemetrics`.
#'
#' @param type Character vector. Type according to FRAGSTATS grouping. One or more of the following:
#' "area and edge", "core area", "shape", "aggregation", "complexity", and or "diversity". Default
#' NULL considers all types.
#' @param level Character vector. Level of metrics. Either "patch", "class" or "landscape"
#' (or a vector with a combination of these). Default NULL considers all levels.
#' @param metric Abbreviation of metrics (e.g. "area").
#' @param name Full name of metrics (e.g. "core area").
#' @param what Selected level of metrics: either "patch", "class" or "landscape". It is also
#' possible to specify functions as a vector of strings, e.g. what = c("lsm_c_ca", "lsm_l_ta").
#'
#' @return A data.frame with the list of available landscape metrics, including information
#' regarding the level, type, metric, name and function name provided by package `landscapemetrics`.
#' @export
#'
#' @references
#' Hesselbarth, M.H.K., Sciaini, M., With, K.A., Wiegand, K., Nowosad, J. 2019. landscapemetrics:
#' an open-source R tool to calculate landscape metrics. - Ecography 42:1648-1657(ver. 0).
#'
#' McGarigal, K., SA Cushman, and E Ene. 2012. FRAGSTATS v4: Spatial Pattern Analysis Program for
#' Categorical and Continuous Maps. Computer software program produced by the authors at the
#' University of Massachusetts, Amherst. \cr
#' Available at the following web site: https://www.umass.edu/landeco/
metrics_list <- function(level = NULL,
                         metric = NULL,
                         name = NULL,
                         type = NULL,
                         what = NULL){

  tab <- tryCatch.W.E(landscapemetrics::list_lsm(level = level,
                                                 metric = metric,
                                                 name = name,
                                                 type = type,
                                                 what = what))

  if(length(class(tab$value)) >= 2){
    if(any(class(tab$value)[1:2] %in% c("simpleError", "error"))){
      if(grepl("Selected metrics do not exist", tab$value)){
        cat("\n")
        stop(strwrap("Selected metrics do not exist. Use metrics_list() to see all
                  available metrics.", prefix = "\n", initial = ""))
      } else {
        cat("\n")
        message("landscapemetrics errors:")
        stop(tab$value)
      }
    } else { tab <- tab$value }
  }
  tab <- as.data.frame(tab)

  return(tab)
}
