.metrics_corr_chk_args <- function(...){

  messages <- NULL
  what     <- NULL

  if(!method %in% c("pearson", "spearman", "kendall")){
    messages <- append(messages,
                       "- argument 'method' must be one of the following: \"pearson\", \"spearman\" or
                       \"kendall\".")
    what     <- append(what, 2)
  } else {
    if(!is.null(fun)){
      if(!exists(fun)){
        messages <- append(messages,
                           paste0("- could not find a function named \"", fun, "\" in the current
                         environment. Mispelled?"))
        what     <- append(what, 2)
        fun <- NULL
      } else {
        args <- list(1:5, 1:5, ...)
        cor_test <- tryCatch(do.call(fun, args),
                             error = c)
        bad <- F
        if(is.list(cor_test)) bad <- T
        if(is.numeric(cor_test)){
          if(length(cor_test) != 1) bad <- T
        } else {
          bad <- T
        }
        if(bad){
          messages <- append(messages,
                             "- function defined in argument 'fun' was not defined properly.
                           See ?metrics_corr")
          what     <- append(what, 2)
          fun <- NULL
        }
      }
    }
    if(!is.null(fun)){
      message(strwrap("- a user-defined function was provided through argument 'fun'.
                      Argument 'method' was ignored.", prefix = "\n", initial = "\n"))
    }
  }

  if(!is.logical(classnames)){
    messages <- append(messages,
                       "- argument 'classnames' must be logical. Default FALSE was taken.")
    what     <- append(what, 1)
    classnames <- FALSE
  }

  # Classnames
  chk_classnames <- .chk_classnames(messages, what, classnames)
  messages   <- chk_classnames[[1]]
  what       <- chk_classnames[[2]]
  classnames <- chk_classnames[[3]]

  if(!display %in% c("radii", "rl", "both")){
    messages <- append(messages,
                       "- argument 'display' must be one of the following: \"radii\", \"rl\" or \"both\".
                       Default \"radii\" was taken.")
    what     <- append(what, 1)
    display <- "radii"
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              fun = fun,
              classnames = classnames,
              display = display)

  return(out)
}

.metrics_plots_chk_args <- function(){

  messages <- NULL
  what     <- NULL

  if(!is.logical(upper)){
    messages <- append(messages,
                       "- argument 'upper' must be logical. Default TRUE was taken.")
    what     <- append(what, 1)
    upper <- TRUE
  }

  if(!is.logical(diag)){
    messages <- append(messages,
                       "- argument 'diag' must be logical. Default TRUE was taken.")
    what     <- append(what, 1)
    diag <- TRUE
  }

  if(!is.logical(smooth)){
    messages <- append(messages,
                       "- argument 'smooth' must be logical. Default TRUE was taken.")
    what     <- append(what, 1)
    smooth <- TRUE
  }

  # points style
  args <- list(shape = 21, size = 2, col = "black", fill = "white", alpha = 1)
  if(!all(names(args) %in% names(st_points))){
    for(i in 1:5){
      if(!names(args)[i] %in% names(st_points)){
        st_points <- append(st_points, args[i])
        names(st_points)[length(st_points)] <- names(args)[i]
      }
    }
  }

  # buffers style
  args <- list(lty = 1, lwd = 1, col = "black", alpha = 1)
  if(!all(names(args) %in% names(st_lines))){
    for(i in 1:5){
      if(!names(args)[i] %in% names(st_lines)){
        st_lines <- append(st_lines, args[i])
        names(st_lines)[length(st_lines)] <- names(args)[i]
      }
    }
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              upper = upper,
              diag = diag,
              smooth = smooth,
              st_points = st_points,
              st_lines = st_lines)

  return(out)
}
