> lines.circular
function (x, y, join = FALSE, nosort = FALSE, offset = 1, shrink = 1, 
          plot.info = NULL, zero = NULL, rotation = NULL, modulo = NULL,  ...) {
  xcircularp <- attr(as.circular(x), "circularp")
  if (is.null(modulo)) 
    modulo <- xcircularp$modulo
  if (is.null(plot.info)) {
    if (is.null(zero)) 
      zero <- xcircularp$zero
    if (is.null(rotation)) 
      rotation <- xcircularp$rotation
    next.points <- 0
  }
  else {
    zero <- plot.info$zero
    rotation <- plot.info$rotation
    next.points <- plot.info$next.points
  }
  ok <- complete.cases(x, y)
  x <- x[ok]
  y <- y[ok]
  if (length(x)) {
    x <- conversion.circular(x, units = "radians", modulo = modulo)
    attr(x, "circularp") <- attr(x, "class") <- NULL
    attr(y, "circularp") <- attr(y, "class") <- NULL
    if (rotation == "clock") 
      x <- -x
    x <- x + zero
    ll <- LinesCircularRad(x, y, join, nosort, offset, shrink,  ...)
  }
  return(invisible(list(x = ll$x, y = ll$y, zero = zero, rotation = rotation, next.points = next.points)))
}
<environment: namespace:circular>