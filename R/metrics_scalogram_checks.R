.metrics_scalogram_chk_args <- function(){

  messages <- NULL
  what     <- NULL

  # Points
  chk_points <- .chk_points2(messages, what, x, points)
  messages <- chk_points[[1]]
  what     <- chk_points[[2]]
  points   <- chk_points[[3]]

  # Show class names
  if(!is.logical(show_class_names)){
    messages <- append(messages,
                       "- argument 'show_class_names' must be logical. Default FALSE was taken.")
    what     <- append(what, 1)
    show_class_names <- FALSE
  }

  # Class names
  chk_classnames <- .chk_classnames(messages, what, show_class_names)
  messages   <- chk_classnames[[1]]
  what       <- chk_classnames[[2]]
  show_class_names <- chk_classnames[[3]]

  # Aggregation
  if(!is.logical(aggregation)){
    messages <- append(messages,
                       "- argument 'aggregation' must be logical. Default FALSE was taken.")
    what     <- append(what, 1)
    aggregation <- FALSE
  }

  # Aggregation function
  if(fun != "mean"){
    if(!exists(fun)){
      messages <- append(messages,
                         paste0("- could not find a function named \"", fun, "\" in the current
                         environment. Mispelled?"))
      what     <- append(what, 2)
      fun <- "mean"
    } else {
      args <- list(1:5)
      val_test <- tryCatch(do.call(fun, args),
                           error = c)
      bad <- FALSE
      if(is.list(val_test)) bad <- TRUE
      if(is.numeric(val_test)){
        if(length(val_test) != 1) bad <- TRUE
      } else {
        bad <- TRUE
      }
      if(bad){
        messages <- append(messages,
                           "- function provided in argument 'fun' does not make sense in this context.
                           See ?metrics_scalogram")
        what     <- append(what, 2)
        fun <- "mean"
      }
    }
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              points = points,
              show_class_names = show_class_names,
              fun = fun)

  return(out)
}
