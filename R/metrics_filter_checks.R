.metrics_filter_chk_args <- function(){

  messages <- NULL
  what     <- NULL

  if(!is.list(conditions)){
    messages <- append(messages,
                       "- argument 'conditions' must be a list.")
    what     <- append(what, 2)
  } else {
    if(all(unlist(lapply(conditions, is.null)))){
      messages <- append(messages,
                         "- no conditions were defined. Nothing to do.")
      what     <- append(what, 2)
    } else {
      if(length(conditions) > 0){
        for(i in 1:length(conditions)){
          if(!is.list(conditions[[i]])){
            messages <- append(messages,
                               paste0("- condition number ", i, " is not a list. Each condition must
                                    be a list. See ?metrics_filter"))
            what     <- append(what, 2)
          } else {
            if(length(conditions[[i]]) != 6){
              messages <- append(messages,
                                 paste0("- condition number ", i, " must be a list with six elements:
                                  c(rasterlayer, class, radius, metric, min. value, max. value).
                                    More details in ?metrics_filter"))
              what     <- append(what, 2)
            } else {
              # Rasterlayer
              if(!is.na(conditions[[i]][[1]][1])){
                if(!conditions[[i]][[1]] %in% unique(x@data$rasterlayer)){
                  messages <- append(messages,
                                     paste0("- rasterlayer (1st element) ", conditions[[i]][[1]],
                                            " from condition ", i, " was not found as a defined layer in
                                        'x'. Mispelled?"))
                  what     <- append(what, 2)
                }
              } else {
                conditions[[i]][[1]] <- unique(x@data$rasterlayer)
                message(paste0("- condition ", i, ": rasterlayers included in the filtering process: ",
                               paste0(conditions[[i]][[1]], collapse = " ")))
              }

              # Class
              if(!2 %in% what){
                if(!is.null(conditions[[i]][[2]])){
                  if(!is.na(conditions[[i]][[2]][1])){
                    for(j in 1:length(conditions[[i]][[2]])){
                      for(r in 1:length(conditions[[i]][[1]])){
                        if(!grepl("ext", conditions[[i]][[1]][r])){
                          foo <- x@classes[x@classes$rasterlayer == conditions[[i]][[1]][r], ]
                          if(!is.na(suppressWarnings(as.numeric(conditions[[i]][[2]][j])))){
                            if(!conditions[[i]][[2]][j] %in% foo$class){
                              messages <- append(messages,
                                                 paste0("- class ", conditions[[i]][[2]][j],
                                                        " from condition ", i, " was not found as a defined
                                              class of rasterlayer ", conditions[[i]][[1]][r],
                                                        " in 'x'. Mispelled?"))
                              what     <- append(what, 2)
                            }
                          } else {
                            if(is.character(conditions[[i]][[2]][j])){
                              if(!conditions[[i]][[2]][j] %in% foo$classname){
                                messages <- append(messages,
                                                   paste0("- class '", conditions[[i]][[2]][j],
                                                          "' from condition ", i, " was not found as a defined
                                                class of rasterlayer ", conditions[[i]][[1]][r],
                                                          " in 'x'. Mispelled?"))
                                what     <- append(what, 2)
                              } else {
                                conditions[[i]][[2]][j] <- foo[foo$classname == conditions[[i]][[2]][j], "class"]
                              }
                            }
                          }
                        }
                      }
                    }
                    conditions[[i]][[2]] <- suppressWarnings(as.numeric(conditions[[i]][[2]]))
                  } else {
                    conditions[[i]][[2]] <- na.exclude(unique(x@data$class))
                    message(paste0("- condition ", i,
                                   ": all classes from all requested rasterlayers were included in the filtering process."))
                    # message(paste0("- condition ", i, ": classes included in the filtering process: ",
                    #                paste0(conditions[[i]][[2]], collapse = " ")))
                  }
                }
              }

              # Radius
              if(!is.na(conditions[[i]][[3]][1])){
                if(!conditions[[i]][[3]] %in% x@radii){
                  messages <- append(messages,
                                     paste0("- radius (2nd element) ", conditions[[i]][[3]],
                                            " from condition ", i, " was not found as a defined radius in
                                        'x'. Mispelled?"))
                  what     <- append(what, 2)
                }
              } else {
                conditions[[i]][[3]] <- x@radii
                message(paste0("- condition ", i, ": radii included in the filtering process: ",
                               paste0(conditions[[i]][[3]], collapse = " ")))
              }

              # Metric
              if(length(conditions[[i]][[4]]) > 1){
                messages <- append(messages,
                                   "- only one metric can be defined per condition.")
                what     <- append(what, 2)
              } else {
                if(!conditions[[i]][[4]] %in% unique(x@data$metric)){
                  messages <- append(messages,
                                     paste0("- metric (3rd element) \"", conditions[[i]][[4]],
                                            "\" from condition ", i, " was not found as a defined metric in
                                      'x'. Mispelled?"))
                  what     <- append(what, 2)
                } else {
                  if(!"fun" %in%  unlist(strsplit(as.character(conditions[[i]][[4]]), "_"))){
                    if(is.null(conditions[[i]][[2]])){
                      if(!"landscape" %in% x@metrics[x@metrics$metric == conditions[[i]][[4]], "level"]){
                        messages <- append(messages,
                                           paste0("- metric (3rd element) \"", conditions[[i]][[4]],
                                                  "\" assumed of landscape-level (2nd element = NULL) from
                                          condition ", i, " was not found as a defined metric in 'x'.
                                           Mispelled?"))
                        what     <- append(what, 2)
                      }
                    } else {
                      if(!"class" %in% x@metrics[x@metrics$metric == conditions[[i]][[4]], "level"]){
                        messages <- append(messages,
                                           paste0("- metric (3rd element) \"", conditions[[i]][[4]],
                                                  "\" assumed of class-level (2nd element != NULL) from
                                          condition ", i, " was not found as a defined metric in
                                          'x'. Mispelled?"))
                        what     <- append(what, 2)
                      }
                    }
                  }
                }
              }
              # Min. value
              if(is.na(suppressWarnings(as.numeric(conditions[[i]][[5]][1])))){
                messages <- append(messages,
                                   paste0("- min. value (5th element) from condition ", i,
                                          " must be numeric."))
                what     <- append(what, 2)
              }
              # Max. value
              if(is.na(suppressWarnings(as.numeric(conditions[[i]][[6]][1])))){
                messages <- append(messages,
                                   paste0("- max. value (6th element) from condition ", i,
                                          " must be numeric."))
                what     <- append(what, 2)
              }
            }
          }
        }
      } else {
        messages <- append(messages,
                           "- no conditions were defined. Nothing to do.")
        what     <- append(what, 2)
      }
    }
  }

  if(!output %in% c("MLM", "spatial", "data", "coords")){
    messages <- append(messages,
                       "- argument output must be \"MLM\", \"spatial\", \"data\" or \"coords\". Default
                       \"MLM\" was taken. See ?metrics_filter")
    what     <- append(what, 1)
    output <- "MLM"
  }

  warnings <- messages[which(what == 1)]
  errors   <- messages[which(what == 2)]

  out <- list(warnings = warnings,
              errors = errors,
              conditions = conditions,
              output = output)
  return(out)
}
