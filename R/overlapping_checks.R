.overlapping_check_args <- function(){

  messages <- NULL
  what     <- NULL

  # Points
  chk_points <- .chk_points(messages, what, x, points)
  messages <- chk_points[[1]]
  what     <- chk_points[[2]]
  points   <- chk_points[[3]]

  # Radii
  chk_radii <- .chk_radii(messages, what, x, radii)
  messages <- chk_radii[[1]]
  what     <- chk_radii[[2]]

  # title
  chk_title <- .chk_title(messages, what, x, title)
  messages  <- chk_title[[1]]
  what      <- chk_title[[2]]
  title     <- chk_title[[3]]
  if(chk_points[[4]]) title <- "sitename"

  # digits
  if(!is.positive.wholenumber(digits)){
    messages <- append(messages,
                       "- argument 'digits' must be a positive wholenumber. Default 2 was taken.")
    what     <- append(what, 1)
    digits   <- 2
  }

  # Percentage?
  if(!is.logical(perc)){
    messages <- append(messages,
                       "- argument 'perc' must be logical. Default TRUE was taken.")
    what     <- append(what, 1)
    perc     <- TRUE
  }

  errors   <- messages[which(what == 2)]
  warnings <- messages[which(what == 1)]

  out <- list(warnings = warnings,
              errors = errors,
              points = points,
              title = title,
              digits = digits,
              perc = perc)
  return(out)
}
