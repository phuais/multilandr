# Copyright (C) 2010-2012  The R Core Team
tryCatch.W.E <- function(expr)
{
  W <- NULL
  w.handler <- function(w){ # warning handler
    W <<- w
    invokeRestart("muffleWarning")
  }
  out <- list(value = withCallingHandlers(tryCatch(expr, error = function(e) e),
                                   warning = w.handler), warning = W)
  if(!is.null(out$warning)){
    message("landscapemetrics warnings:")
    message(paste0(out$warning))
  }
  return(out)
}

is.wholenumber <-  function(x, tol = .Machine$double.eps^0.5)  abs(x - round(x)) < tol

is.positive.wholenumber <-  function(x){
  if(!is.na(x)){
    if(all(is.wholenumber(x), x > 0)) T else F
  } else {
    F
  }
}

is.positive.numeric <- function(x){
  if(all(is.numeric(x), x > 0)) T else F
}

is.numberinchar <- function(x){
  if(is.na(suppressWarnings(as.numeric(x)))) F else T
}

# Obtains legend from plot
# Credits to Luciano Selzer:
# https://stackoverflow.com/questions/11883844/inserting-a-table-under-the-legend-in-a-ggplot2-histogram
.g_legend <- function(a.gplot){
  tmp    <- ggplot2::ggplot_gtable(ggplot2::ggplot_build(a.gplot))
  leg    <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  if(length(leg) > 0){
    legend <- tmp$grobs[[leg]]
  } else {
    legend <- NULL
  }
  return(legend)
}

length_unique <- function(x){ length(unique(x)) }

mmerge <- function(x){
  merged_raster <- terra::merge(x[[1]], x[[2]])
  if(length(x) > 2){
    for(i in 3:length(x)){
      merged_raster <- terra::merge(merged_raster, x[[i]])
    }
  }
  merged_raster
}

keygen <- function(){
  id <- ""
  for(i in 1:9){ id <- paste0(id, sample(1:9, 1)) }
  id
}

rbind_and_remove <- function(x, y, cols){
  x <- rbind(x, y)
  index <- rownames(unique(x[, cols]))
  x
}

rbind_and_replace <- function(x, y, cols){
  y <- rbind(y, x)
  y <- y[!duplicated(y[, cols], fromLast = T), ]
  y
}

show_sample <- function(x, limit = 6){
  if(length(x) > limit){
    x <- c(x[1:limit], "...")
  }
  x
}
